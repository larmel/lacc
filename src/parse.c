#include "error.h"
#include "eval.h"
#include "type.h"
#include "string.h"
#include "preprocess.h"
#include "symbol.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static struct block *declaration(struct block *);
static struct typetree *declaration_specifiers(enum token_type *);
static struct typetree *declarator(struct typetree *, const char **);
static struct typetree *pointer(const struct typetree *);
static struct typetree *direct_declarator(struct typetree *, const char **);
static struct typetree *parameter_list(const struct typetree *);
static struct block *initializer(struct block *block, struct var target);
static struct block *block(struct block *);
static struct block *statement(struct block *);

static struct block *expression(struct block *);
static struct block *assignment_expression(struct block *);
static struct block *conditional_expression(struct block *);
static struct block *logical_and_expression(struct block *);
static struct block *logical_or_expression(struct block *);
static struct block *inclusive_or_expression(struct block *);
static struct block *exclusive_or_expression(struct block *);
static struct block *and_expression(struct block *block);
static struct block *equality_expression(struct block *block);
static struct block *relational_expression(struct block *block);
static struct block *shift_expression(struct block *block);
static struct block *additive_expression(struct block *block);
static struct block *multiplicative_expression(struct block *block);
static struct block *cast_expression(struct block *block);
static struct block *postfix_expression(struct block *block);
static struct block *unary_expression(struct block *block);
static struct block *primary_expression(struct block *block);

static struct var constant_expression();

/* Namespaces. */
struct namespace
    ns_ident = {"identifiers"},
    ns_label = {"labels"},
    ns_tag = {"tags"}
    ;

/* Current declaration, accessed for creating new blocks or adding init code
 * in head block. */
struct decl *decl;

/* Parse the next external declaration. */
struct decl *parse()
{
    static int done_last_iteration;

    decl = cfg_create();
    decl->head = cfg_block_init(decl);
    decl->body = cfg_block_init(decl);

    while (peek().token != '$') {
        decl->fun = NULL;
        declaration(decl->body);

        if (decl->head->n || decl->fun) {
            return decl;
        }
    }

    if (!done_last_iteration) {
        int i, found;
        struct symbol *sym;
        for (i = found = 0; i < ns_ident.size; ++i) {
            sym = ns_ident.symbol[i];
            if (sym->symtype == SYM_TENTATIVE && sym->linkage == LINK_INTERN) {
                found = 1;
                eval_assign(decl->head, var_direct(sym), var_int(0));
            }
        }

        done_last_iteration = 1;
        if (found) {
            return decl;
        }
    }

    cfg_finalize(decl);
    return NULL;
}

/* C99: Define __func__ as static const char __func__[] = sym->name; */
static void define_builtin__func__(const char *name)
{
    struct var str = var_string(strlabel(name), strlen(name) + 1);
    struct symbol
         farg = { "__func__", NULL, SYM_DEFINITION, LINK_INTERN },
        *func;

    assert(ns_ident.current_depth == 1);

    farg.type = str.type;
    func = sym_add(&ns_ident, farg);
    eval_assign(decl->head, var_direct(func), str);
}

/* Cover both external declarations, functions, and local declarations (with
 * optional initialization code) inside functions. */
static struct block *
declaration(struct block *parent)
{
    struct typetree *base;
    struct symbol arg = {0};
    enum token_type stc = '$';

    base = declaration_specifiers(&stc);
    switch (stc) {
    case EXTERN:
        arg.symtype = SYM_DECLARATION;
        arg.linkage = LINK_EXTERN;
        break;
    case STATIC:
        arg.symtype = SYM_TENTATIVE;
        arg.linkage = LINK_INTERN;
        break;
    case TYPEDEF:
        arg.symtype = SYM_TYPEDEF;
        break;
    default:
        if (!ns_ident.current_depth) {
            arg.symtype = SYM_TENTATIVE;
            arg.linkage = LINK_EXTERN;
        } else {
            arg.symtype = SYM_DEFINITION;
            arg.linkage = LINK_NONE;
        }
        break;
    }

    while (1) {
        struct symbol *sym;

        arg.name = NULL;
        arg.type = declarator(base, &arg.name);
        if (!arg.name) {
            consume(';');
            return parent;
        }

        sym = sym_add(&ns_ident, arg);
        assert(sym->type);
        if (ns_ident.current_depth) {
            assert(ns_ident.current_depth > 1);
            sym_list_push_back(&decl->locals, sym);
        }

        switch (peek().token) {
        case ';':
            consume(';');
            return parent;
        case '=': 
            if (sym->symtype == SYM_DECLARATION) {
                error("Extern symbol '%s' cannot be initialized.", sym->name);
            }
            if (!sym->depth && sym->symtype == SYM_DEFINITION) {
                error("Symbol '%s' was already defined.", sym->name);
                exit(1);
            }
            consume('=');
            sym->symtype = SYM_DEFINITION;
            if (!sym->depth || sym->n) {
                decl->head = initializer(decl->head, var_direct(sym));
            } else {
                parent = initializer(parent, var_direct(sym));
            }
            assert(sym->type->size);
            if (peek().token != ',') {
                consume(';');
                return parent;
            }
            break;
        case '{': {
            int i;
            if (sym->type->type != FUNCTION || sym->depth) {
                error("Invalid function definition.");
                exit(1);
            }
            sym->symtype = SYM_DEFINITION;
            decl->fun = sym;

            push_scope(&ns_ident);
            define_builtin__func__(sym->name);
            for (i = 0; i < sym->type->n; ++i) {
                struct symbol sarg = {
                    SYM_DEFINITION,
                    LINK_NONE,
                };
                sarg.name = arg.type->member[i].name;
                sarg.type = sym->type->member[i].type;;
                if (!sarg.name) {
                    error("Missing parameter name at position %d.", i + 1);
                    exit(1);
                }
                sym_list_push_back(&decl->params, sym_add(&ns_ident, sarg));
            }
            parent = block(parent);
            pop_scope(&ns_ident);

            return parent;
        }
        default:
            break;
        }
        consume(',');
    }
}

/* Parse and emit initializer code for target variable in statements such as
 * int b[] = {0, 1, 2, 3}. Generate a series of assignment operations on
 * references to target variable.
 */
static struct block *initializer(struct block *block, struct var target)
{
    int i;
    const struct typetree *type;

    assert(target.kind == DIRECT);

    if (peek().token == '{') {
        type = target.type;
        target.lvalue = 1;
        consume('{');
        switch (type->type) {
        case OBJECT:
            for (i = 0; i < type->n; ++i) {
                target.type = type->member[i].type;
                target.offset = type->member[i].offset;
                block = initializer(block, target);
                if (i < type->n - 1) {
                    consume(',');
                }
            }
            break;
        case ARRAY:
            target.type = type->next;
            for (i = 0; !type->size || i < type->size / type->next->size; ++i) {
                block = initializer(block, target);
                target.offset += type->next->size;
                if (peek().token != ',') {
                    break;
                }
                consume(',');
            }
            /* Incomplete array type can only be in the root level of target
             * type tree, thus safe to overwrite type directly in symbol. */
            if (!type->size) {
                assert(!target.symbol->type->size);
                assert(target.symbol->type->type == ARRAY);

                ((struct typetree *) target.symbol->type)->size = target.offset;
            }
            if (target.offset < type->size) {
                error("Incomplete array initializer is not yet supported.");
            }
            break;
        default:
            error("Block initializer only apply to array or object type.");
            exit(1);
        }
        consume('}');
    } else {
        block = assignment_expression(block);
        if (!target.symbol->depth && block->expr.kind != IMMEDIATE) {
            error("Initializer must be computable at load time.");
            exit(1);
        }
        if (target.kind == DIRECT && !target.type->size) {
            ((struct symbol *) target.symbol)->type =
                type_complete(target.symbol->type, block->expr.type);
        }
        eval_assign(block, target, block->expr);
    }

    return block;
}

/* Parse struct declaration list.
 *
 *      { int a; long b; }
 */
static void struct_declaration_list(struct typetree *obj)
{
    struct namespace ns = {0};
    push_scope(&ns);

    do {
        struct typetree *base = declaration_specifiers(NULL);

        do {
            struct symbol sym = {0};

            sym.type = declarator(base, &sym.name);
            if (!sym.name) {
                error("Missing name in struct member declarator.");
            } else if (!sym.type->size) {
                error("Field '%s' has incomplete type.", sym.name);
            } else {
                sym_add(&ns, sym);
                type_add_member(obj, sym.type, sym.name);
            }

            if (peek().token == ',') {
                consume(',');
                continue;
            }
        } while (peek().token != ';');

        consume(';');
    } while (peek().token != '}');

    type_align_struct_members(obj);
    pop_scope(&ns);
}

/* Parse struct or union declaration.
 *
 *      struct foo { ... }
 */
static struct typetree *struct_or_union_declaration(void)
{
    const char *tag_name = NULL;
    struct typetree *tag_type = NULL;
    struct typetree *type;

    next();
    if (peek().token == IDENTIFIER) {
        struct token ident = consume(IDENTIFIER);
        struct symbol *tag = sym_lookup(&ns_tag, ident.strval);
        if (!tag) {
            struct symbol arg = { NULL, NULL, SYM_TYPEDEF };
            arg.name = strdup(ident.strval);
            arg.type = (type = type_init_object());
            tag = sym_add(&ns_tag, arg);
        } else if (is_integer(tag->type)) {
            error("Tag '%s' was previously defined as enum type.", tag->name);
            exit(1);
        }

        /* Retrieve type from existing tag, possibly providing a complete
         * definition that will be available for later declarations. Overwrites
         * existing type information from symbol table. */
        type = (struct typetree *) tag->type;
        tag_type = type;
        tag_name = tag->name;
        if (peek().token == '{' && type->size) {
            error("Redefiniton of object '%s'.", tag->name);
            exit(1);
        }
    } else {
        type = type_init_object();
    }

    if (peek().token == '{') {
        consume('{');
        struct_declaration_list(type);
        consume('}');
    }

    /* Return to the caller a copy of the root node, which can be overwritten
     * with new type qualifiers without altering the tag registration. */
    if (tag_type) {
        type = calloc(1, sizeof(*type));
        *type = *tag_type;
        type->next = tag_type;
        type->tag_name = tag_name;
    }

    return type;
}

/* Parse enumerator list.
 *
 *      { FOO = 1; BAR; }
 */
static void enumerator_list(void)
{
    struct token tok;
    struct symbol arg = { NULL, NULL, SYM_ENUM_VALUE };

    consume('{');
    arg.type = type_init_integer(4);
    while (1) {
        tok = consume(IDENTIFIER);
        arg.name = strdup(tok.strval);
        if (peek().token == '=') {
            struct var val;

            consume('=');
            val = constant_expression();
            if (val.type->type != INTEGER) {
                error("Implicit conversion from non-integer type in enum.");
            }
            arg.enum_value = val.value.integer;
        }

        sym_add(&ns_ident, arg);
        arg.enum_value++;
        if (peek().token != '}') {
            consume(',');
            continue;
        }

        break;
    }

    consume('}');
}

/* Parse enum declaration.
 *
 *      enum tag { ... }
 */
static struct typetree *enum_declaration(void)
{
    struct typetree *type = type_init_integer(4);
    struct symbol *tag = NULL;

    consume(ENUM);
    if (peek().token == IDENTIFIER) {
        struct token ident = next();
        struct symbol arg = { NULL, NULL, SYM_TYPEDEF };

        arg.name = strdup(ident.strval); /* Remove strdup ? */
        arg.type = type;
        tag = sym_lookup(&ns_tag, ident.strval);
        if (!tag || (tag->depth < ns_tag.current_depth && peek().token == '{'))
        {
            tag = sym_add(&ns_tag, arg);
        } else if (!is_integer(tag->type)) {
            error("Tag '%s' was previously defined as object type.", tag->name);
            exit(1);
        }

        if (peek().token == '{' && tag->enum_value) {
            error("Redefiniton of enum '%s'.", tag->name);
            exit(1);
        }
    }

    if (peek().token == '{') {
        enumerator_list();
        if (tag) {
            /* Use enum_value as a sentinel to represent definition, checked on 
             * lookup to detect duplicate definitions. */
            tag->enum_value = 1;
        }
    }

    return type;
}

/* Parse type, qualifiers and storage class. Do not assume int by default, but
 * require at least one type specifier. Storage class is returned as token
 * value, unless the provided pointer is NULL, in which case the input is parsed
 * as specifier-qualifier-list.
 */
static struct typetree *declaration_specifiers(enum token_type *stc)
{
    struct typetree *type = NULL;
    struct token tok;
    int done = 0;

    /* Use a compact bit representation to hold state about declaration 
     * specifiers and qualifiers. Initialize storage class to sentinel value. */
    unsigned short spec = 0x0000;
    unsigned short qual = 0x0000;
    if (stc)       *stc =    '$';

    #define set_specifier(d) \
        if (spec & d) error("Duplicate type specifier '%s'.", tok.strval); \
        next(); spec |= d;

    #define set_qualifier(d) \
        if (qual & d) error("Duplicate type qualifier '%s'.", tok.strval); \
        next(); qual |= d;

    #define set_storage_class(s) \
        if (!stc) error("Unexpected storage class in qualifier list."); \
        else if (*stc != '$') error("Multiple storage class specifiers."); \
        next(); *stc = tok.token;

    do {
        switch ((tok = peek()).token) {
        case VOID:      set_specifier(0x001); break;
        case CHAR:      set_specifier(0x002); break;
        case SHORT:     set_specifier(0x004); break;
        case INT:       set_specifier(0x008); break;
        case SIGNED:    set_specifier(0x010); break;
        case UNSIGNED:  set_specifier(0x020); break;
        case LONG:
            if (spec & 0x040) {
                set_specifier(0x080);
            } else {
                set_specifier(0x040);   
            }
            break;
        case FLOAT:     set_specifier(0x100); break;
        case DOUBLE:    set_specifier(0x200); break;
        case CONST:     set_qualifier(0x01); break;
        case VOLATILE:  set_qualifier(0x02); break;
        case IDENTIFIER: {
            struct symbol *tag = sym_lookup(&ns_ident, tok.strval);
            if (tag && tag->symtype == SYM_TYPEDEF && !type) {
                consume(IDENTIFIER);
                type = calloc(1, sizeof(*type));
                *type = *tag->type;
            } else {
                done = 1;
            }
            break;
        }
        case UNION:
        case STRUCT:
            if (!type) {
                type = struct_or_union_declaration();
            } else {
                done = 1;
            }
            break;
        case ENUM:
            if (!type) {
                type = enum_declaration();
            } else {
                done = 1;
            }
            break;
        case AUTO:
        case REGISTER:
        case STATIC:
        case EXTERN:
        case TYPEDEF:
            set_storage_class();
            break;
        default:
            done = 1;
            break;
        }

        if (type && spec) {
            /* Catch errors early without having a check in too many places. */
            error("Invalid combination of declaration specifiers.");
            exit(1);
        }
    } while (!done);

    #undef set_specifier
    #undef set_qualifier
    #undef set_storage_class

    if (type) {
        if (qual & type->qualifier) {
            error("Duplicate type qualifier:%s%s.",
                (qual & 0x01) ? " const" : "",
                (qual & 0x02) ? " volatile" : "");
        }
    } else if (spec) {
        type = calloc(1, sizeof(*type));
        *type = type_from_specifier(spec);
    } else {
        error("Missing type specifier.");
        exit(1);
    }

    type->qualifier |= qual;
    return type;
}

static struct typetree *declarator(struct typetree *base, const char **symbol)
{
    while (peek().token == '*') {
        base = pointer(base);
    }

    return direct_declarator(base, symbol);
}

static struct typetree *pointer(const struct typetree *base)
{
    struct typetree *type = type_init_pointer(base);

    #define set_qualifier(d) \
        if (type->qualifier & d) \
            error("Duplicate type qualifier '%s'.", peek().strval); \
        type->qualifier |= d;

    consume('*');
    while (1) {
        if (peek().token == CONST) {
            set_qualifier(0x01);
        } else if (peek().token == VOLATILE) {
            set_qualifier(0x02);
        } else break;
        next();
    }

    #undef set_qualifier

    return type;
}

/* Parse array declarations of the form [s0][s1]..[sn], resulting in type
 * [s0] [s1] .. [sn] (base).
 *
 * Only the first dimension s0 can be unspecified, yielding an incomplete type.
 * Incomplete types are represented by having size of zero.
 */
static struct typetree *
direct_declarator_array(struct typetree *base)
{
    if (peek().token == '[') {
        long length = 0;

        consume('[');
        if (peek().token != ']') {
            struct var expr = constant_expression();
            assert(expr.kind == IMMEDIATE);
            if (expr.type->type != INTEGER || expr.value.integer < 1) {
                error("Array dimension must be a natural number.");
                exit(1);
            }
            length = expr.value.integer;
        }
        consume(']');

        base = direct_declarator_array(base);
        if (!base->size) {
            error("Array has incomplete element type.");
            exit(1);
        }

        base = type_init_array(base, length);
    }

    return base;
}

/* Parse function and array declarators. Some trickery is needed to handle
 * declarations like `void (*foo)(int)`, where the inner *foo has to be 
 * traversed first, and prepended on the outer type `* (int) -> void` 
 * afterwards making it `* (int) -> void`.
 * The type returned from declarator has to be either array, function or
 * pointer, thus only need to check for type->next to find inner tail.
 */
static struct typetree *
direct_declarator(struct typetree *base, const char **symbol)
{
    struct typetree *type = base;
    struct typetree *head, *tail = NULL;
    struct token ident;

    switch (peek().token) {
    case IDENTIFIER:
        ident = consume(IDENTIFIER);
        if (!symbol) {
            error("Unexpected identifier in abstract declarator.");
            exit(1);
        }
        *symbol = strdup(ident.strval);
        break;
    case '(':
        consume('(');
        type = head = tail = declarator(NULL, symbol);
        while (tail->next) {
            tail = (struct typetree *) tail->next;
        }
        consume(')');
        break;
    default:
        break;
    }

    while (peek().token == '[' || peek().token == '(') {
        switch (peek().token) {
        case '[':
            type = direct_declarator_array(base);
            break;
        case '(':
            consume('(');
            type = parameter_list(base);
            consume(')');
            break;
        default:
            assert(0);
        }
        if (tail) {
            tail->next = type;
            type = head;
        }
        base = type;
    }

    return type;
}

/* FOLLOW(parameter-list) = { ')' }, peek to return empty list; even though K&R
 * require at least specifier: (void)
 * Set parameter-type-list = parameter-list, including the , ...
 */
static struct typetree *parameter_list(const struct typetree *base)
{
    struct typetree *type;

    type = type_init_function();
    type->next = base;

    while (peek().token != ')') {
        const char *name;
        struct typetree *decl;

        name = NULL;
        decl = declaration_specifiers(NULL);
        decl = declarator(decl, &name);
        if (decl->type == NONE) {
            if (type->n) {
                error("Incomplete type in parameter list.");
            }
            break;
        }

        if (decl->type == ARRAY) {
            decl = type_init_pointer(decl->next);
        }

        type_add_member(type, decl, name);

        if (peek().token != ',') {
            break;
        }

        consume(',');
        if (peek().token == ')') {
            error("Unexpected trailing comma in parameter list.");
            exit(1);
        } else if (peek().token == DOTS) {
            consume(DOTS);
            type->flags |= 0x02;
            break;
        }
    }

    return type;
}

/* Treat statements and declarations equally, allowing declarations in between
 * statements as in modern C. Called compound-statement in K&R.
 */
static struct block *
block(struct block *parent)
{
    consume('{');
    push_scope(&ns_ident);
    push_scope(&ns_tag);
    while (peek().token != '}') {
        parent = statement(parent);
    }
    consume('}');
    pop_scope(&ns_tag);
    pop_scope(&ns_ident);
    return parent;
}

static struct switch_context
{
    struct block *default_label;
    struct block **case_label;
    struct var *case_value;
    int n;
} *switch_ctx;

static void add_switch_case(struct block *label, struct var value)
{
    struct switch_context *ctx = switch_ctx;

    ctx->n++;
    ctx->case_label =
        realloc(ctx->case_label, ctx->n * sizeof(*ctx->case_label));
    ctx->case_value =
        realloc(ctx->case_value, ctx->n * sizeof(*ctx->case_value));

    ctx->case_label[ctx->n - 1] = label;
    ctx->case_value[ctx->n - 1] = value;
}

static void free_switch_context(struct switch_context *ctx)
{
    assert( ctx );
    if (ctx->n) {
        free(ctx->case_label);
        free(ctx->case_value);
    }
    free(ctx);
}

/* Create or expand a block of code. Consecutive statements without branches
 * are stored as a single block, passed as parent. Statements with branches
 * generate new blocks. Returns the current block of execution after the
 * statement is done. For ex: after an if statement, the empty fallback is
 * returned. Caller must keep handles to roots, only the tail is returned. */
static struct block *statement(struct block *parent)
{
    struct block *node;
    struct token tok;

    /* Store reference to top of loop, for resolving break and continue. Use
     * call stack to keep track of depth, backtracking to the old value. */
    static struct block *break_target, *continue_target;
    struct block *old_break_target, *old_continue_target;

    /* Keep references to old switch context, pushing a new context on each
     * 'switch' statement. */
    struct switch_context *old_switch_ctx;

    switch ((tok = peek()).token) {
    case ';':
        consume(';');
        node = parent;
        break;
    case '{':
        node = block(parent); /* execution continues  */
        break;
    case IF: {
        struct block *right = cfg_block_init(decl),
                *next  = cfg_block_init(decl);

        consume(tok.token);
        consume('(');

        /* Node becomes a branch, store the expression as condition variable
         * and append code to compute the value. parent->expr holds the
         * result automatically. */
        parent = expression(parent);
        consume(')');

        parent->jump[0] = next;
        parent->jump[1] = right;

        /* The order is important here: Send right as head in new statement
         * graph, and store the resulting tail as new right, hooking it up
         * to the fallback of the if statement. */
        right = statement(right);
        right->jump[0] = next;

        if (peek().token == ELSE) {
            struct block *left = cfg_block_init(decl);
            consume(ELSE);

            /* Again, order is important: Set left as new jump target for
             * false if branch, then invoke statement to get the
             * (potentially different) tail. */
            parent->jump[0] = left;
            left = statement(left);

            left->jump[0] = next;
        }
        node = next;
        break;
    }
    case WHILE:
    case DO: {
        struct block *top = cfg_block_init(decl),
                *body = cfg_block_init(decl),
                *next = cfg_block_init(decl);
        parent->jump[0] = top; /* Parent becomes unconditional jump. */

        /* Enter a new loop, remember old break and continue target. */
        old_break_target = break_target;
        old_continue_target = continue_target;
        break_target = next;
        continue_target = top;

        consume(tok.token);

        if (tok.token == WHILE) {
            struct block *cond;

            consume('(');
            cond = expression(top);
            consume(')');
            cond->jump[0] = next;
            cond->jump[1] = body;

            /* Generate statement, and get tail end of body to loop back. */
            body = statement(body);
            body->jump[0] = top;
        } else if (tok.token == DO) {

            /* Generate statement, and get tail end of body */
            body = statement(top);
            consume(WHILE);
            consume('(');
            /* Tail becomes branch. (nb: wrong if tail is return?!) */
            body = expression(body);
            body->jump[0] = next;
            body->jump[1] = top;
            consume(')');
        }

        /* Restore previous nested loop */
        break_target = old_break_target;
        continue_target = old_continue_target;

        node = next;
        break;
    }
    case FOR: {
        struct block
            *top = cfg_block_init(decl),
            *body = cfg_block_init(decl),
            *increment = cfg_block_init(decl),
            *next = cfg_block_init(decl);

        /* Enter a new loop, remember old break and continue target. */
        old_break_target = break_target;
        old_continue_target = continue_target;
        break_target = next;
        continue_target = increment;

        consume(FOR);
        consume('(');
        if (peek().token != ';') {
            parent = expression(parent);
        }
        consume(';');
        if (peek().token != ';') {
            parent->jump[0] = top;
            top = expression(top);
            top->jump[0] = next;
            top->jump[1] = body;
            top = (struct block *) parent->jump[0];
        } else {
            /* Infinite loop */
            parent->jump[0] = body;
            top = body;
        }
        consume(';');
        if (peek().token != ')') {
            expression(increment)->jump[0] = top;
        }
        consume(')');
        body = statement(body);
        body->jump[0] = increment;

        /* Restore previous nested loop */
        break_target = old_break_target;
        continue_target = old_continue_target;

        node = next;
        break;
    }
    case GOTO:
        consume(GOTO);
        consume(IDENTIFIER);
        /* todo */
        consume(';');
        break;
    case CONTINUE:
    case BREAK:
        consume(tok.token);
        parent->jump[0] = (tok.token == CONTINUE) ? 
            continue_target :
            break_target;
        consume(';');
        /* Return orphan node, which is dead code unless there is a label
         * and a goto statement. */
        node = cfg_block_init(decl); 
        break;
    case RETURN:
        consume(RETURN);
        if (peek().token != ';') {
            parent = expression(parent);
        }
        consume(';');
        node = cfg_block_init(decl); /* orphan */
        break;
    case SWITCH: {
        int i;
        struct block
            *body, /* First block of switch statement. */
            *last; /* Last block of switch statement. */

        consume(SWITCH);
        consume('(');
        parent = expression(parent);
        consume(')');

        /* Breaking out of switch reaches next block. */
        node = cfg_block_init(decl);
        old_break_target = break_target;
        break_target = node;

        /* Push new switch context. */
        old_switch_ctx = switch_ctx;
        switch_ctx = calloc(1, sizeof(*switch_ctx));

        body = cfg_block_init(decl);
        last = statement(body);
        last->jump[0] = node;

        if (!switch_ctx->n && !switch_ctx->default_label) {
            parent->jump[0] = node;
        } else {
            struct block *cond = parent;

            for (i = 0; i < switch_ctx->n; ++i) {
                struct block *prev_cond = cond;
                struct block *label = switch_ctx->case_label[i];
                struct var value = switch_ctx->case_value[i];

                cond = cfg_block_init(decl);
                cond->expr = eval_expr(cond, IR_OP_EQ, value, parent->expr);
                cond->jump[1] = label;
                prev_cond->jump[0] = cond;
            }

            cond->jump[0] = (switch_ctx->default_label) ?
                switch_ctx->default_label : node;
        }

        free_switch_context(switch_ctx);
        break_target = old_break_target;
        switch_ctx = old_switch_ctx;
        break;
    }
    case CASE:
        consume(CASE);
        if (!switch_ctx) {
            error("Stray 'case' label, must be inside a switch statement.");
            node = parent;
        } else {
            struct var expr = constant_expression();
            consume(':');
            node = cfg_block_init(decl);
            add_switch_case(node, expr);
            parent->jump[0] = node;
            node = statement(node);
        }
        break;
    case DEFAULT:
        consume(DEFAULT);
        consume(':');
        if (!switch_ctx) {
            error("Stray 'default' label, must be inside a switch statement.");
            node = parent;
        } else if (switch_ctx->default_label) {
            error("Multiple 'default' labels inside the same switch.");
            node = parent;
        } else {
            node = cfg_block_init(decl);
            parent->jump[0] = node;
            switch_ctx->default_label = node;
            node = statement(node);
        }
        break;
    case IDENTIFIER: {
        const struct symbol *def;
        if ((
            def = sym_lookup(&ns_ident, tok.strval)) 
            && def->symtype == SYM_TYPEDEF
        ) {
            node = declaration(parent);
            break;
        }
        /* todo: handle label statement. */
    }
    case INTEGER_CONSTANT: /* todo: any constant value */
    case STRING:
    case '*':
    case '(':
        node = expression(parent);
        consume(';');
        break;
    default:
        node = declaration(parent);
        break;
    }

    return node;
}

static struct block *expression(struct block *block)
{
    block = assignment_expression(block);
    while (peek().token == ',') {
        consume(',');
        block = assignment_expression(block);
    }

    return block;
}

/* todo: Fix this rule (a lot more complicated than this...) */
static struct block *assignment_expression(struct block *block)
{
    struct var target;

    block = conditional_expression(block);
    if (peek().token == '=') {
        consume('=');
        target = block->expr;
        block = assignment_expression(block);
        block->expr = eval_assign(block, target, block->expr);
    }

    return block;
}

static struct var constant_expression()
{
    struct block *head = cfg_block_init(decl),
            *tail;

    tail = conditional_expression(head);
    if (tail != head || tail->expr.kind != IMMEDIATE) {
        error("Constant expression must be computable at compile time.");
        exit(1);
    }

    return tail->expr;
}

static struct block *conditional_expression(struct block *block)
{
    block = logical_or_expression(block);
    if (peek().token == '?') {
        struct var condition = block->expr;
        struct block
            *t = cfg_block_init(decl),
            *f = cfg_block_init(decl),
            *next = cfg_block_init(decl);

        consume('?');
        block->jump[0] = f;
        block->jump[1] = t;

        t = expression(t);
        t->jump[0] = next;

        consume(':');
        f = conditional_expression(f);
        f->jump[0] = next;

        next->expr = eval_conditional(condition, t, f);
        block = next;
    }

    return block;
}

static struct block *logical_or_expression(struct block *block)
{
    block = logical_and_expression(block);
    if (peek().token == LOGICAL_OR) {
        struct block 
            *right = cfg_block_init(decl),
            *next = cfg_block_init(decl);

        consume(LOGICAL_OR);
        block->jump[0] = right;
        block->jump[1] = next;

        right = logical_or_expression(right);
        right->jump[0] = next;

        next->expr =
            eval_expr(next, IR_OP_LOGICAL_OR, block->expr, right->expr);
        block = next;
    }

    return block;
}

static struct block *logical_and_expression(struct block *block)
{
    block = inclusive_or_expression(block);
    if (peek().token == LOGICAL_AND) {
        struct block 
            *right = cfg_block_init(decl),
            *next = cfg_block_init(decl);

        consume(LOGICAL_AND);
        block->jump[0] = next;
        block->jump[1] = right;

        right = logical_and_expression(right);
        right->jump[0] = next;

        next->expr =
            eval_expr(next, IR_OP_LOGICAL_AND, block->expr, right->expr);
        block = next;
    }

    return block;
}

static struct block *inclusive_or_expression(struct block *block)
{
    struct var value;

    block = exclusive_or_expression(block);
    while (peek().token == '|') {
        consume('|');
        value = block->expr;
        block = exclusive_or_expression(block);
        block->expr = eval_expr(block, IR_OP_BITWISE_OR, value, block->expr);
    }

    return block;
}

static struct block *exclusive_or_expression(struct block *block)
{
    struct var value;

    block = and_expression(block);
    while (peek().token == '^') {
        consume('^');
        value = block->expr;
        block = and_expression(block);
        block->expr = eval_expr(block, IR_OP_BITWISE_XOR, value, block->expr);
    }

    return block;
}

static struct block *and_expression(struct block *block)
{
    struct var value;

    block = equality_expression(block);
    while (peek().token == '&') {
        consume('&');
        value = block->expr;
        block = equality_expression(block);
        block->expr = eval_expr(block, IR_OP_BITWISE_AND, value, block->expr);
    }

    return block;
}

static struct block *equality_expression(struct block *block)
{
    struct var value;

    block = relational_expression(block);
    while (1) {
        value = block->expr;
        if (peek().token == EQ) {
            consume(EQ);
            block = relational_expression(block);
            block->expr = eval_expr(block, IR_OP_EQ, value, block->expr);
        } else if (peek().token == NEQ) {
            consume(NEQ);
            block = relational_expression(block);
            block->expr = 
                eval_expr(block, IR_OP_EQ, var_int(0),
                    eval_expr(block, IR_OP_EQ, value, block->expr));
        } else break;
    }

    return block;
}

static struct block *relational_expression(struct block *block)
{
    struct var value;

    block = shift_expression(block);
    while (1) {
        value = block->expr;
        switch (peek().token) {
        case '<':
            consume('<');
            block = shift_expression(block);
            block->expr = eval_expr(block, IR_OP_GT, block->expr, value);
            break;
        case '>':
            consume('>');
            block = shift_expression(block);
            block->expr = eval_expr(block, IR_OP_GT, value, block->expr);
            break;
        case LEQ:
            consume(LEQ);
            block = shift_expression(block);
            block->expr = eval_expr(block, IR_OP_GE, block->expr, value);
            break;
        case GEQ:
            consume(GEQ);
            block = shift_expression(block);
            block->expr = eval_expr(block, IR_OP_GE, value, block->expr);
            break;
        default:
            return block;
        }
    }
}

static struct block *shift_expression(struct block *block)
{
    return additive_expression(block);
}

static struct block *additive_expression(struct block *block)
{
    struct var value;

    block = multiplicative_expression(block);
    while (1) {
        value = block->expr;
        if (peek().token == '+') {
            consume('+');
            block = multiplicative_expression(block);
            block->expr = eval_expr(block, IR_OP_ADD, value, block->expr);
        } else if (peek().token == '-') {
            consume('-');
            block = multiplicative_expression(block);
            block->expr = eval_expr(block, IR_OP_SUB, value, block->expr);
        } else break;
    }

    return block;
}

static struct block *multiplicative_expression(struct block *block)
{
    struct var value;

    block = cast_expression(block);
    while (1) {
        value = block->expr;
        if (peek().token == '*') {
            consume('*');
            block = cast_expression(block);
            block->expr = eval_expr(block, IR_OP_MUL, value, block->expr);
        } else if (peek().token == '/') {
            consume('/');
            block = cast_expression(block);
            block->expr = eval_expr(block, IR_OP_DIV, value, block->expr);
        } else if (peek().token == '%') {
            consume('%');
            block = cast_expression(block);
            block->expr = eval_expr(block, IR_OP_MOD, value, block->expr);
        } else break;
    }

    return block;
}

#define FIRST_type_qualifier \
    CONST: case VOLATILE

#define FIRST_type_specifier \
    VOID: case CHAR: case SHORT: case INT: case LONG: case FLOAT: case DOUBLE: \
    case SIGNED: case UNSIGNED: case STRUCT: case UNION: case ENUM

#define FIRST_type_name \
    FIRST_type_qualifier: \
    case FIRST_type_specifier

#define FIRST(s) FIRST_ ## s

static struct block *cast_expression(struct block *block)
{
    struct typetree *type;
    struct token tok;
    struct symbol *sym;

    /* This rule needs two lookahead, to see beyond the initial parenthesis if
     * it is actually a cast or an expression. */
    if (peek().token == '(') {
        tok = peekn(2);
        switch (tok.token) {
        case IDENTIFIER:
            sym = sym_lookup(&ns_ident, tok.strval);
            if (!sym || sym->symtype != SYM_TYPEDEF)
                break;
        case FIRST(type_name):
            consume('(');
            type = declaration_specifiers(NULL);
            if (peek().token != ')') {
                type = declarator(type, NULL);
            }
            consume(')');
            block = cast_expression(block);
            block->expr = eval_cast(block, block->expr, type);
            return block;
        default:
            break;
        }
    }

    return unary_expression(block);
}

static struct block *unary_expression(struct block *block)
{
    struct var value;

    switch (peek().token) {
    case '&':
        consume('&');
        block = cast_expression(block);
        block->expr = eval_addr(block, block->expr);
        break;
    case '*':
        consume('*');
        block = cast_expression(block);
        block->expr = eval_deref(block, block->expr);
        break;
    case '!':
        consume('!');
        block = cast_expression(block);
        block->expr = eval_expr(block, IR_OP_EQ, var_int(0), block->expr);
        break;
    case '+':
        consume('+');
        block = cast_expression(block);
        block->expr.lvalue = 0;
        break;
    case '-':
        consume('-');
        block = cast_expression(block);
        block->expr = eval_expr(block, IR_OP_SUB, var_int(0), block->expr);
        break;
    case SIZEOF: {
        struct typetree *type;
        struct block *head = cfg_block_init(decl), *tail;
        consume(SIZEOF);
        if (peek().token == '(') {
            switch (peekn(2).token) {
            case FIRST(type_name):
                consume('(');
                type = declaration_specifiers(NULL);
                if (peek().token != ')') {
                    type = declarator(type, NULL);
                }
                consume(')');
                break;
            default:
                tail = unary_expression(head);
                type = (struct typetree *) tail->expr.type;
                break;
            }
        } else {
            tail = unary_expression(head);
            type = (struct typetree *) tail->expr.type;
        }
        if (type->type == FUNCTION) {
            error("Cannot apply 'sizeof' to function type.");
        }
        if (!type->size) {
            error("Cannot apply 'sizeof' to incomplete type.");
        }
        block->expr = var_int(type->size);
        break;
    }
    case INCREMENT:
        consume(INCREMENT);
        block = unary_expression(block);
        value = block->expr;
        block->expr = eval_expr(block, IR_OP_ADD, value, var_int(1));
        block->expr = eval_assign(block, value, block->expr);
        break;
    case DECREMENT:
        consume(DECREMENT);
        block = unary_expression(block);
        value = block->expr;
        block->expr = eval_expr(block, IR_OP_SUB, value, var_int(1));
        block->expr = eval_assign(block, value, block->expr);
        break;
    default:
        block = postfix_expression(block);
        break;
    }

    return block;
}

static struct block *postfix_expression(struct block *block)
{
    struct var root;

    block = primary_expression(block);
    root = block->expr;

    while (1) {
        struct var expr, copy, *arg;
        struct token tok;
        int i, j;

        switch ((tok = peek()).token) {
        case '[':
            do {
                /* Evaluate a[b] = *(a + b). The semantics of pointer arithmetic
                 * takes care of multiplying b with the correct width. */
                consume('[');
                block = expression(block);
                root = eval_expr(block, IR_OP_ADD, root, block->expr);
                root = eval_deref(block, root);
                consume(']');
            } while (peek().token == '[');
            break;
        case '(':
            /* Evaluation function call. */
            if (root.type->type != FUNCTION) {
                error("Calling non-function symbol.");
                exit(1);
            }
            arg = malloc(sizeof(struct var) * root.type->n);

            consume('(');
            for (i = 0; i < root.type->n; ++i) {
                if (peek().token == ')') {
                    error("Too few arguments to %s, expected %d but got %d.",
                        root.symbol->name, root.type->n, i);
                    exit(1);
                }
                block = assignment_expression(block);
                arg[i] = block->expr;
                /* todo: type check here. */
                if (i < root.type->n - 1) {
                    consume(',');
                }
            }
            while (is_vararg(root.type) && peek().token != ')') {
                consume(',');
                arg = realloc(arg, (i + 1) * sizeof(struct var));
                block = assignment_expression(block);
                arg[i] = block->expr;
                i++;
            }
            consume(')');

            for (j = 0; j < i; ++j) {
                param(block, arg[j]);
            }
            root = eval_call(block, root);
            free(arg);
            break;
        case '.':
            /* There is no short representation of symbol address, so unless
             * this is a deref there will be a new temporary. */
            root = eval_addr(block, root);
        case ARROW:
            next();
            tok = consume(IDENTIFIER);
            if (!is_pointer(root.type) || root.type->next->type != OBJECT) {
                error("Cannot access field of non-object type.");
                exit(1);
            } else {
                const struct member *field;

                field = find_type_member(type_deref(root.type), tok.strval);
                if (!field) {
                    error("Invalid field access, no member named %s.",
                        tok.strval);
                    exit(1);
                }

                /* Make it look like a pointer to the field type, then perform
                 * normal dereferencing. */
                root.type = type_init_pointer(field->type);
                root.offset += field->offset;
                root = eval_deref(block, root);
            }
            break;
        case INCREMENT:
            consume(INCREMENT);
            copy = eval_copy(block, root);
            expr = eval_expr(block, IR_OP_ADD, root, var_int(1));
            eval_assign(block, root, expr);
            root = copy;
            break;
        case DECREMENT:
            consume(DECREMENT);
            copy = eval_copy(block, root);
            expr = eval_expr(block, IR_OP_SUB, root, var_int(1));
            eval_assign(block, root, expr);
            root = copy;
            break;
        default:
            block->expr = root;
            return block;
        }
    }
}

static struct block *primary_expression(struct block *block)
{
    const struct symbol *sym;
    struct token tok;

    switch ((tok = next()).token) {
    case IDENTIFIER:
        sym = sym_lookup(&ns_ident, tok.strval);
        if (!sym) {
            error("Undefined symbol '%s'.", tok.strval);
            exit(1);
        }
        block->expr = var_direct(sym);
        break;
    case INTEGER_CONSTANT:
        block->expr = var_int(tok.intval);
        break;
    case '(':
        block = expression(block);
        consume(')');
        break;
    case STRING:
        block->expr = var_string(strlabel(tok.strval), strlen(tok.strval) + 1);
        break;
    default:
        error("Unexpected token '%s', not a valid primary expression.",
            tok.strval);
        exit(1);
    }

    return block;
}
