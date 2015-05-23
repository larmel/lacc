#include "error.h"
#include "ir.h"
#include "type.h"
#include "string.h"
#include "preprocess.h"
#include "symbol.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static block_t *declaration(block_t *);
static typetree_t *declaration_specifiers(enum token_type *);
static typetree_t *declarator(typetree_t *, const char **);
static typetree_t *pointer(const typetree_t *);
static typetree_t *direct_declarator(typetree_t *, const char **);
static typetree_t *parameter_list(const typetree_t *);
static block_t *initializer(block_t *block, var_t target);
static block_t *block(block_t *);
static block_t *statement(block_t *);

static block_t *expression(block_t *);
static block_t *assignment_expression(block_t *);
static block_t *conditional_expression(block_t *);
static block_t *logical_and_expression(block_t *);
static block_t *logical_or_expression(block_t *);
static block_t *inclusive_or_expression(block_t *);
static block_t *exclusive_or_expression(block_t *);
static block_t *and_expression(block_t *block);
static block_t *equality_expression(block_t *block);
static block_t *relational_expression(block_t *block);
static block_t *shift_expression(block_t *block);
static block_t *additive_expression(block_t *block);
static block_t *multiplicative_expression(block_t *block);
static block_t *cast_expression(block_t *block);
static block_t *postfix_expression(block_t *block);
static block_t *unary_expression(block_t *block);
static block_t *primary_expression(block_t *block);

static var_t constant_expression();

/* Namespaces. */
namespace_t
    ns_ident = {"identifiers"},
    ns_label = {"labels"},
    ns_tag = {"tags"}
    ;

/* Current declaration, accessed for creating new blocks or adding init code
 * in head block. */
decl_t *decl;

/* Parse the next external declaration. */
decl_t *parse()
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
        symbol_t *sym;
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
    symbol_t farg = { SYM_DEFINITION, LINK_INTERN }, *func;
    var_t str = var_string(string_constant_label(name), strlen(name) + 1);

    assert(ns_ident.depth == 1);

    farg.name = "__func__";
    farg.type = str.type;
    func = sym_add(&ns_ident, farg);
    eval_assign(decl->head, var_direct(func), str);
}

/* Cover both external declarations, functions, and local declarations (with
 * optional initialization code) inside functions. */
static block_t *
declaration(block_t *parent)
{
    typetree_t *base;
    symbol_t arg = {0};
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
        if (!ns_ident.depth) {
            arg.symtype = SYM_TENTATIVE;
            arg.linkage = LINK_EXTERN;
        } else {
            arg.symtype = SYM_DEFINITION;
            arg.linkage = LINK_NONE;
        }
        break;
    }

    while (1) {
        symbol_t *sym;

        arg.name = NULL;
        arg.type = declarator(base, &arg.name);
        if (!arg.name) {
            consume(';');
            return parent;
        }

        sym = sym_add(&ns_ident, arg);
        assert(sym->type);
        if (ns_ident.depth) {
            assert(ns_ident.depth > 1);
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
                symbol_t sarg = {
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
static block_t *initializer(block_t *block, var_t target)
{
    int i;
    const typetree_t *type;

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

/* Maybe a bit too clever here: overwriting existing typetree object already in
 * symbol table.
 */
static void struct_declaration_list(typetree_t *obj)
{
    namespace_t ns = {0};
    push_scope(&ns);

    do {
        typetree_t *base = declaration_specifiers(NULL);
        if (!base) {
            error("Missing type specifier in struct member declaration.");
            exit(1);
        }

        do {
            symbol_t sym = {0};

            sym.type = declarator(base, &sym.name);
            if (!sym.name) {
                error("Invalid struct member declarator.");
                exit(1);
            }

            sym_add(&ns, sym);
            type_add_member(obj, sym.type, sym.name);

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

static void
enumerator_list()
{
    struct token tok;
    symbol_t arg = { SYM_ENUM };

    arg.type = type_init(INTEGER);

    while (1) {
        tok = consume(IDENTIFIER);
        arg.name = strdup(tok.strval);
        if (peek().token == '=') {
            var_t val;

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
}

/* Parse type, storage class and qualifiers. Assume integer type by default.
 * Storage class is returned as token value, and error is raised if there are
 * more than one storage class given.
 * If stc is NULL, parse specifier_qualifier_list and give an error for any 
 * storage class present.
 *
 * This rule can be used to backtrack, i.e. if there is no valid declaration
 * specifier, NULL is returned. */
static typetree_t *
declaration_specifiers(enum token_type *stc)
{
    int consumed = 0;
    enum token_type sttok = '$';
    typetree_t *type = type_init(INTEGER);
    symbol_t *tag = NULL;

    do {
        struct token tok = peek();

        switch (tok.token) {
        case CONST:
            consume(CONST);
            type->flags.fconst = 1;
            break;
        case VOLATILE:
            consume(VOLATILE);
            type->flags.fvolatile = 1;
            break;
        case AUTO:
        case REGISTER:
        case STATIC:
        case EXTERN:
        case TYPEDEF:
            if (sttok != '$') {
                error("Only one storage class specifier allowed.");
            }
            if (!stc) {
                error("Storage class specifier not allowed in qualifier list.");
            }
            sttok = next().token;
            break;
        case IDENTIFIER:
            tag = sym_lookup(&ns_ident, tok.strval);
            if (tag && tag->symtype == SYM_TYPEDEF) {
                flags_t flags;
                /* todo: validate */
                consume(IDENTIFIER);
                flags = type->flags;
                *type = *(tag->type);
                type->flags.fvolatile |= flags.fvolatile;
                type->flags.fconst |= flags.fconst;
            } else {
                goto end;
            }
            break;
        case CHAR:
            consume(CHAR);
            type->size = 1;
            break;
        case SHORT:
            consume(SHORT);
            type->size = 2;
            break;
        case INT:
        case SIGNED:
            next();
            type->size = 4;
            break;
        case LONG:
            consume(LONG);
            type->size = 8;
            break;
        case UNSIGNED:
            consume(UNSIGNED);
            if (!type->size)
                type->size = 4;
            type->flags.funsigned = 1;
            break;
        case FLOAT:
            consume(FLOAT);
            type->type = REAL;
            type->size = 4;
            break;
        case DOUBLE:
            consume(DOUBLE);
            type->type = REAL;
            type->size = 8;
            break;
        case VOID:
            consume(VOID);
            type->type = NONE;
            break;
        case UNION:
        case STRUCT:
            next();
            type->type = OBJECT;
            type->size = 0;
            if (peek().token == IDENTIFIER) {
                struct token ident = consume(IDENTIFIER);
                tag = sym_lookup(&ns_tag, ident.strval);
                if (!tag) {
                    symbol_t arg = { SYM_TYPEDEF };
                    arg.name = strdup(ident.strval);
                    arg.type = type;
                    tag = sym_add(&ns_tag, arg);
                } else if (tag->type->type == INTEGER) {
                    error("Tag '%s' was previously defined as enum type.",
                        tag->name);
                    exit(1);
                }

                type = (typetree_t *) tag->type;
                if (peek().token != '{') {
                    /* Can still have volatile or const after. */
                    break;
                } else if (type->size) {
                    error("Redefiniton of object '%s'.", tag->name);
                    exit(1);
                }
            }
            consume('{');
            struct_declaration_list(type);
            consume('}');
            break;
        case ENUM:
            consume(ENUM);
            type->type = INTEGER;
            type->size = 4;
            if (peek().token == IDENTIFIER) {
                struct token ident;
                symbol_t arg = { SYM_TYPEDEF };

                ident = consume(IDENTIFIER);
                arg.name = strdup(ident.strval);
                arg.type = type;
                tag = sym_lookup(&ns_tag, ident.strval);
                if (!tag ||
                    (tag->depth < ns_tag.depth && peek().token == '{')
                ) {
                    tag = sym_add(&ns_tag, arg);
                } else if (tag->type->type != INTEGER) {
                    error("Tag '%s' was previously defined as object type.",
                        tag->name);
                    exit(1);
                }

                type = (typetree_t *) tag->type;
                if (peek().token != '{') {
                    break;
                } else if (tag->enum_value) {
                    error("Redefiniton of enum '%s'.", tag->name);
                    exit(1);
                }
            }
            consume('{');
            enumerator_list();
            if (tag) {
                /* Use enum_value to represent definition. */
                tag->enum_value = 1;
            }
            consume('}');
            break;
        default:
            goto end;
        }
    } while (++consumed);
end:
    if (stc && sttok != '$') *stc = sttok;
    return consumed ? type : NULL;
}

static typetree_t *
declarator(typetree_t *base, const char **symbol)
{
    while (peek().token == '*') {
        base = pointer(base);
    }
    return direct_declarator(base, symbol);
}

static typetree_t *
pointer(const typetree_t *base)
{
    typetree_t *type = type_init(POINTER);
    type->next = base;
    base = type;
    consume('*');
    while (peek().token == CONST || peek().token == VOLATILE) {
        if (next().token == CONST)
            type->flags.fconst = 1;
        else
            type->flags.fvolatile = 1;
    }
    return type;
}

/* Parse array declarations of the form [s0][s1]..[sn], resulting in type
 * [s0] [s1] .. [sn] (base).
 *
 * Only the first dimension s0 can be unspecified, yielding an incomplete type.
 * Incomplete types are represented by having size of zero.
 */
static typetree_t *
direct_declarator_array(typetree_t *base)
{
    if (peek().token == '[') {
        typetree_t *root;
        long length = 0;

        consume('[');
        if (peek().token != ']') {
            var_t expr = constant_expression();
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

        root = type_init(ARRAY);
        root->next = base;
        root->size = length * base->size;
        base = root;
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
static typetree_t *
direct_declarator(typetree_t *base, const char **symbol)
{
    typetree_t *type = base;
    typetree_t *head, *tail = NULL;
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
            tail = (typetree_t *) tail->next;
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
static typetree_t *parameter_list(const typetree_t *base)
{
    typetree_t *type;

    type = type_init(FUNCTION);
    type->next = base;

    while (peek().token != ')') {
        const char *name;
        enum token_type stc;
        typetree_t *decl;

        name = NULL;
        decl = declaration_specifiers(&stc);
        decl = declarator(decl, &name);
        if (decl->type == NONE) {
            break;
        }

        if (decl->type == ARRAY) {
            typetree_t *ptr = type_init(POINTER);
            ptr->next = decl->next;
            decl = ptr;
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
            type->vararg = 1;
            break;
        }
    }

    return type;
}

/* Treat statements and declarations equally, allowing declarations in between
 * statements as in modern C. Called compound-statement in K&R.
 */
static block_t *
block(block_t *parent)
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

/* Create or expand a block of code. Consecutive statements without branches
 * are stored as a single block, passed as parent. Statements with branches
 * generate new blocks. Returns the current block of execution after the
 * statement is done. For ex: after an if statement, the empty fallback is
 * returned. Caller must keep handles to roots, only the tail is returned. */
static block_t *
statement(block_t *parent)
{
    block_t *node;
    struct token tok;

    /* Store reference to top of loop, for resolving break and continue. Use
     * call stack to keep track of depth, backtracking to the old value. */
    static block_t *break_target, *continue_target;
    block_t *old_break_target, *old_continue_target;

    switch ((tok = peek()).token) {
    case ';':
        consume(';');
        node = parent;
        break;
    case '{':
        node = block(parent); /* execution continues  */
        break;
    case SWITCH:
    case IF:
        {
            block_t *right = cfg_block_init(decl),
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
                block_t *left = cfg_block_init(decl);
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
    case DO:
        {
            block_t *top = cfg_block_init(decl),
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
                consume('(');
                top = expression(top);
                consume(')');
                top->jump[0] = next;
                top->jump[1] = body;

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
    case FOR:
        {
            block_t *top = cfg_block_init(decl),
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
            } else {
                /* Infinite loop */
                parent->jump[0] = body;
                top = body;
            }
            consume(';');
            if (peek().token != ')') {
                increment = expression(increment);
                increment->jump[0] = top;
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
    case CASE:
    case DEFAULT:
        /* todo */
        break;
    case IDENTIFIER:
        {
            const symbol_t *def;
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

static block_t *expression(block_t *block)
{
    block = assignment_expression(block);
    while (peek().token == ',') {
        consume(',');
        block = assignment_expression(block);
    }

    return block;
}

/* todo: Fix this rule (a lot more complicated than this...) */
static block_t *assignment_expression(block_t *block)
{
    var_t target;

    block = conditional_expression(block);
    if (peek().token == '=') {
        consume('=');
        target = block->expr;
        block = assignment_expression(block);
        block->expr = eval_assign(block, target, block->expr);
    }

    return block;
}

static var_t constant_expression()
{
    block_t *head = cfg_block_init(decl),
            *tail;

    tail = conditional_expression(head);
    if (tail != head || tail->expr.kind != IMMEDIATE) {
        error("Constant expression must be computable at compile time.");
        exit(1);
    }

    return tail->expr;
}

static block_t *conditional_expression(block_t *block)
{
    return logical_or_expression(block);
}

static block_t *logical_or_expression(block_t *block)
{
    var_t res;
    block_t *next,
            *last = NULL;

    block = logical_and_expression(block);

    if (peek().token == LOGICAL_OR) {
        symbol_t *sym = sym_temp(&ns_ident, type_init(INTEGER));
        res = var_direct(sym);
        sym_list_push_back(&decl->locals, sym);
        res.lvalue = 1;

        eval_assign(block, res, block->expr);

        last = cfg_block_init(decl);
    }

    while (peek().token == LOGICAL_OR) {
        next = cfg_block_init(decl);

        consume(LOGICAL_OR);

        block->jump[1] = last;
        block->jump[0] = next;

        next = logical_and_expression(next);
        next->expr =
            eval_expr(next, IR_OP_LOGICAL_OR, block->expr, next->expr);
        eval_assign(next, res, next->expr);

        block = next;
    }

    if (last) {
        block->jump[0] = last;
        block = last;
        block->expr = res;
    }

    return block;
}

static block_t *logical_and_expression(block_t *block)
{
    var_t res;
    block_t *next,
            *last = NULL;

    block = inclusive_or_expression(block);

    if (peek().token == LOGICAL_AND) {
        symbol_t *sym = sym_temp(&ns_ident, type_init(INTEGER));
        res = var_direct(sym);
        sym_list_push_back(&decl->locals, sym);
        res.lvalue = 1;

        eval_assign(block, res, block->expr);

        last = cfg_block_init(decl);
    }

    while (peek().token == LOGICAL_AND) {
        next = cfg_block_init(decl);

        consume(LOGICAL_AND);

        block->jump[0] = last;
        block->jump[1] = next;

        next = inclusive_or_expression(next);
        next->expr =
            eval_expr(next, IR_OP_LOGICAL_AND, block->expr, next->expr);
        eval_assign(next, res, next->expr);

        block = next;
    }

    if (last) {
        block->jump[0] = last;
        block = last;
        block->expr = res;
    }

    return block;
}

static block_t *inclusive_or_expression(block_t *block)
{
    var_t value;

    block = exclusive_or_expression(block);
    while (peek().token == '|') {
        consume('|');
        value = block->expr;
        block = exclusive_or_expression(block);
        block->expr = eval_expr(block, IR_OP_BITWISE_OR, value, block->expr);
    }

    return block;
}

static block_t *exclusive_or_expression(block_t *block)
{
    var_t value;

    block = and_expression(block);
    while (peek().token == '^') {
        consume('^');
        value = block->expr;
        block = and_expression(block);
        block->expr = eval_expr(block, IR_OP_BITWISE_XOR, value, block->expr);
    }

    return block;
}

static block_t *and_expression(block_t *block)
{
    var_t value;

    block = equality_expression(block);
    while (peek().token == '&') {
        consume('&');
        value = block->expr;
        block = equality_expression(block);
        block->expr = eval_expr(block, IR_OP_BITWISE_AND, value, block->expr);
    }

    return block;
}

static block_t *equality_expression(block_t *block)
{
    var_t value;

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
                eval_expr(block, IR_OP_NOT,
                    eval_expr(block, IR_OP_EQ, value, block->expr));
        } else break;
    }

    return block;
}

static block_t *relational_expression(block_t *block)
{
    var_t value;

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

static block_t *shift_expression(block_t *block)
{
    return additive_expression(block);
}

static block_t *additive_expression(block_t *block)
{
    var_t value;

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

static block_t *multiplicative_expression(block_t *block)
{
    var_t value;

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

static block_t *cast_expression(block_t *block)
{
    typetree_t *type;
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
            if (!type) {
                error("Invalid cast expression, expected type-name.");
                exit(1);
            }
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

static block_t *unary_expression(block_t *block)
{
    var_t value;

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
        block->expr = eval_expr(block, IR_OP_NOT, block->expr);
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
        typetree_t *type;
        block_t *head = cfg_block_init(decl),
                *tail;

        consume(SIZEOF);
        if (peek().token == '(') {

            switch (peekn(2).token) {
            case FIRST(type_name):
                consume('(');
                type = declaration_specifiers(NULL);
                if (!type) {
                    error("Expected type-name.");
                    exit(1);
                }
                if (peek().token != ')') {
                    type = declarator(type, NULL);
                }
                consume(')');
                break;
            default:
                tail = unary_expression(head);
                type = (typetree_t *) tail->expr.type;
                break;
            }
        } else {
            tail = unary_expression(head);
            type = (typetree_t *) tail->expr.type;
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

static block_t *postfix_expression(block_t *block)
{
    var_t root, value;

    block = primary_expression(block);
    root = block->expr;

    while (1) {
        var_t expr, copy, *arg;
        struct token tok;
        int i, j;

        switch ((tok = peek()).token) {
        case '[':
            /* Evaluate a[b] = *(a + b). */
            do {
                consume('[');
                block = expression(block);
                value = eval_expr(block, IR_OP_MUL,
                    block->expr,
                    var_int(root.type->next->size));
                value = eval_expr(block, IR_OP_ADD, root, value);
                root = eval_deref(block, value);
                consume(']');
            } while (peek().token == '[');
            break;
        case '(':
            /* Evaluation function call. */
            if (root.type->type != FUNCTION) {
                error("Calling non-function symbol.");
                exit(1);
            }
            arg = malloc(sizeof(var_t) * root.type->n);

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
            while (root.type->vararg && peek().token != ')') {
                consume(',');
                arg = realloc(arg, (i + 1) * sizeof(var_t));
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
            root = eval_addr(block, root);
        case ARROW:
            next();
            tok = consume(IDENTIFIER);
            if (root.type->type == POINTER && 
                root.type->next->type == OBJECT)
            {
                int i;
                struct member *field;

                for (i = 0; i < root.type->next->n; ++i) {
                    field = &root.type->next->member[i];
                    if (!strcmp(tok.strval, field->name)) {
                        break;
                    }
                }
                if (i == root.type->next->n) {
                    error("Invalid field access, no field named %s.",
                        tok.strval);
                    exit(1);
                }

                root.kind = DEREF;
                root.type = field->type;
                root.offset += field->offset;
                root.lvalue = 1;
            } else {
                error("Cannot access field of non-object type.");
                exit(1);
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

static block_t *primary_expression(block_t *block)
{
    const symbol_t *sym;
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
        block->expr = 
            var_string(
                string_constant_label(tok.strval),
                strlen(tok.strval) + 1);
        break;
    default:
        error("Unexpected token '%s', not a valid primary expression.",
            tok.strval);
        exit(1);
    }

    return block;
}
