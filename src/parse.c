#include "error.h"
#include "ir.h"
#include "type.h"
#include "string.h"
#include "token.h"
#include "symbol.h"

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

static block_t *declaration(block_t *, const symbol_t **);
static typetree_t *declaration_specifiers(enum storage_class *);
static typetree_t *declarator(typetree_t *, const char **);
static typetree_t *pointer(const typetree_t *);
static typetree_t *direct_declarator(typetree_t *, const char **);
static typetree_t *parameter_list(const typetree_t *);
static void initializer(block_t *block, var_t target);
static block_t *block(block_t *);
static block_t *statement(block_t *);

/* expression nodes that are called in high level rules */
static var_t expression(block_t *);
static var_t constant_expression();
static var_t assignment_expression(block_t *);

extern int var_stack_offset;

/* Current declaration, accessed for creating new blocks or adding init code
 * in head block. */
static decl_t *decl;

decl_t *
parse()
{
    decl = cfg_create();
    decl->head = cfg_block_init(decl);
    decl->body = cfg_block_init(decl);

    while (peek() != '$') {
        decl->fun = NULL;
        declaration(decl->body, &decl->fun);

        if (decl->head->n || decl->fun) {
            if (decl->fun) {

                /* Hack: write stack offset as we are done adding all symbols,
                 * reading variable from symtab code.
                 * This will also be possible to move to parse function probably.
                 */
                decl->locals_size = (-1) * var_stack_offset;
            }
            return decl;
        }
    }

    cfg_finalize(decl);
    return NULL;
}

/* Cover both external declarations, functions, and local declarations (with
 * optional initialization code) inside functions. Symbol is bound to function
 * if encountered, otherwise not touched.
 */
static block_t *
declaration(block_t *parent, const symbol_t **symbol)
{
    static int in_function;

    typetree_t *base;
    enum storage_class stc;

    base = declaration_specifiers(&stc);

    while (1) {
        typetree_t *type;
        const symbol_t *sym;
        const char *name = NULL;
        
        type = declarator(base, &name);
        if (!name) {
            error("Missing declarator name.");
            exit(1);
        }
        if (type->type != FUNCTION && in_function) {
            stc = STC_AUTO;
        }
        sym = sym_add(name, type, stc);
        free((void *) name);

        switch (peek()) {
            case ';':
                consume(';');
                return parent;
            case '=': {
                consume('=');
                if (!sym->depth) {
                    initializer(decl->head, var_direct(sym));
                } else {
                    initializer(parent, var_direct(sym));
                }
                /*assert(type->size); */
                if (peek() != ',') {
                    consume(';');
                    return parent;
                }
                break;
            }
            case '{': {
                int i;
                if (type->type != FUNCTION || sym->depth > 0) {
                    error("Invalid function definition.");
                    exit(1);
                }
                in_function = 1;

                push_scope();
                for (i = 0; i < type->n_args; ++i) {
                    if (!type->params[i]) {
                        error("Missing parameter name at position %d.", i + 1);
                        exit(1);
                    }
                    sym_add(type->params[i], type->args[i], STC_AUTO);
                }
                parent = block(parent);
                *symbol = sym;
                pop_scope();

                in_function = 0;
                return parent;
            }
            default:
                break;
        }
        consume(',');
    }
}

/* Parse an emit initializer code for target variable.
 * int b[] = {0, 1, 2, 3} will emit a series of assignment operations on
 * references to symbol b.
 */
static void
initializer(block_t *block, var_t target)
{
    const typetree_t *type;
    var_t var;
    int i;

    if (peek() == '{') {
        assert(target.kind == DIRECT);
        type = target.type;
        target.lvalue = 1;
        consume('{');
        if (type->type == OBJECT) {
            for (i = 0; i < type->n_args; ++i) {
                target.type = type->args[i];
                initializer(block, target);
                target.offset += type->args[i]->size;
                if (peek() == '}')
                    break;
                consume(',');
            }
        } else if (type->type == ARRAY) {
            target.type = type->next;
            for (i = 0; !type->size || i < type->size / type->next->size; ++i) {
                initializer(block, target);
                target.offset += type->next->size;
                if (peek() == '}')
                    break;
                consume(',');
            }
            if (!type->size) {
                ((typetree_t *)type)->size = target.offset;
            }
        } else {
            error("Braces around initializer must represent array or object type.");
            exit(1);
        }
        if (target.offset < type->size) {
            error("Incomplete initializer is not yet supported.");
        }
        consume('}');
    } else {
        var = assignment_expression(block);
        if (var.kind != IMMEDIATE) {
            /*
            error("Initializer must be computable at load time.");
            exit(1);
            */
        }
        eval_assign(block, target, var);
    }
}

static void
struct_declaration_list(typetree_t *obj)
{
    push_scope();

    do {
        typetree_t *base;

        base = declaration_specifiers(NULL);
        do {
            typetree_t *member;
            const char *name;

            name = NULL;
            member = declarator(base, &name);
            if (!name) {
                error("Invalid struct member declarator.");
                exit(1);
            }
            sym_add(name, member, STC_NONE);

            obj->n_args++;
            obj->args = realloc(obj->args, sizeof(typetree_t *) * obj->n_args);
            obj->params = realloc(obj->params, sizeof(char *) * obj->n_args);

            obj->args[obj->n_args - 1] = member;
            obj->params[obj->n_args - 1] = name;
            obj->size += member->size;

            if (peek() == ',') {
                consume(',');
                continue;
            }

        } while (peek() != ';');

        consume(';');
    } while (peek() != '}');

    pop_scope();
}

/* Parse type, storage class and qualifiers. Assume integer type by default.
 * Storage class is returned as token value, and error is raised if there are
 * more than one storage class given.
 * At most one storage class can be specified, fallback to STC_EXTERN if none
 * is provided. This is correct in all cases except for automatic variables.
 * If stc is NULL, parse specifier_qualifier_list and give an error for any 
 * storage class present.
 *
 * This rule can be used to backtrack, i.e. if there is no valid declaration
 * specifier, NULL is returned.
 */
static typetree_t *
declaration_specifiers(enum storage_class *stc)
{
    int done;
    enum token stt;
    typetree_t *type;

    type = type_init(INTEGER);
    type->size = 0;
    stt  = '$';
    done = 0;

    do {
        switch (peek()) {
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
                if (stt != '$')
                    error("Only one storage class specifier allowed.");
                if (!stc)
                    error("Storage class specifier not allowed in specifier-qualifier-list.");
                stt = token();
                break;
            case IDENTIFIER:
            {
                const symbol_t *tdef;
                flags_t flags;

                tdef = sym_lookup(strval);
                if (tdef && tdef->storage == STC_TYPEDEF) {
                    consume(IDENTIFIER);
                    if (type->size)
                        error("Cannot combine type definition %s with other type specifiers.", strval);
                    flags = type->flags;
                    *type = *(tdef->type);
                    type->flags.fvolatile |= flags.fvolatile;
                    type->flags.fconst |= flags.fconst;
                } else {
                    done = 1;   
                }
                break;
            }
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
                token();
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
                type->size = 8;
                break;
            case STRUCT:
            case UNION:
                token();
                type->type = OBJECT;
                if (peek() == IDENTIFIER) {
                    error("Unsupported named struct or union.");
                    consume(IDENTIFIER);
                }
                consume('{');
                struct_declaration_list(type);
                consume('}');
                break;
            case ENUM:
                /* todo */
            default:
                done = 1;
        }
    } while (!done);

    if (stc) {
        *stc = ((stt == AUTO || stt == REGISTER) ? STC_AUTO :
                (stt == STATIC) ? STC_STATIC :
                (stt == TYPEDEF) ? STC_TYPEDEF : STC_EXTERN);
    }

    return (type->size) ? type : NULL;
}

static typetree_t *
declarator(typetree_t *base, const char **symbol)
{
    while (peek() == '*') {
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
    while (peek() == CONST || peek() == VOLATILE) {
        if (token() == CONST)
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
    if (peek() == '[') {
        typetree_t *root;
        long length = 0;

        consume('[');
        if (peek() != ']') {
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

    switch (peek()) {
        case IDENTIFIER:
            token();
            if (!symbol) {
                error("Unexpected identifier in abstract declarator.");
                exit(1);
            }
            *symbol = strdup(strval);
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

    while (peek() == '[' || peek() == '(') {
        switch (peek()) {
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

/* FOLLOW(parameter-list) = { ')' }, peek to return empty list;
 * even though K&R require at least specifier: (void)
 * Set parameter-type-list = parameter-list, including the , ...
 */
static typetree_t *
parameter_list(const typetree_t *base)
{
    typetree_t *type;
    const typetree_t **args = NULL;
    const char **params = NULL;
    int nargs = 0;

    while (peek() != ')') {
        const char *name;
        enum storage_class stc;
        typetree_t *decl;

        name = NULL;
        decl = declaration_specifiers(&stc);
        decl = declarator(decl, &name);

        if (decl->type == ARRAY) {
            typetree_t *ptr = type_init(POINTER);
            ptr->next = decl->next;
            decl = ptr;
        }

        nargs++;
        args   = realloc(args, sizeof(typetree_t *) * nargs);
        params = realloc(params, sizeof(char *) * nargs);
        args[nargs - 1]   = decl;
        params[nargs - 1] = name;

        if (peek() != ',')
            break;

        consume(',');
        if (peek() == ')') {
            error("Unexpected trailing comma in parameter list.");
            exit(1);
        } else if (peek() == DOTS) {
            consume(DOTS); /* todo: add vararg type */
            break;
        }
    }

    type = type_init(FUNCTION);
    type->next = base;
    type->n_args = nargs;
    type->args = args;
    type->params = params;
    return type;
}

/* Treat statements and declarations equally, allowing declarations in between
 * statements as in modern C. Called compound-statement in K&R.
 */
static block_t *
block(block_t *parent)
{
    consume('{');
    push_scope();
    while (peek() != '}') {
        parent = statement(parent);
    }
    consume('}');
    pop_scope();
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

    /* Store reference to top of loop, for resolving break and continue. Use
     * call stack to keep track of depth, backtracking to the old value. */
    static block_t *break_target, *continue_target;
    block_t *old_break_target, *old_continue_target;

    enum token t = peek();

    switch (t) {
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
            block_t *right = cfg_block_init(decl), *next = cfg_block_init(decl);
            consume(t);
            consume('(');

            /* node becomes a branch, store the expression as condition
             * variable and append code to compute the value. */
            parent->expr = expression(parent);
            consume(')');

            parent->jump[0] = next;
            parent->jump[1] = right;

            /* The order is important here: Send right as head in new statement
             * graph, and store the resulting tail as new right, hooking it up
             * to the fallback of the if statement. */
            right = statement(right);
            right->jump[0] = next;

            if (peek() == ELSE) {
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
            block_t *top = cfg_block_init(decl), *body = cfg_block_init(decl), *next = cfg_block_init(decl);
            parent->jump[0] = top; /* Parent becomes unconditional jump. */

            /* Enter a new loop, store reference for break and continue target. */
            old_break_target = break_target;
            old_continue_target = continue_target;
            break_target = next;
            continue_target = top;

            consume(t);

            if (t == WHILE) {
                consume('(');
                top->expr = expression(top);
                consume(')');
                top->jump[0] = next;
                top->jump[1] = body;

                /* Generate statement, and get tail end of body to loop back */
                body = statement(body);
                body->jump[0] = top;
            } else if (t == DO) {

                /* Generate statement, and get tail end of body */
                body = statement(top);
                consume(WHILE);
                consume('(');
                body->expr = expression(body); /* Tail becomes branch. (nb: wrong if tail is return?!) */
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
            block_t *top = cfg_block_init(decl), *body = cfg_block_init(decl), *increment = cfg_block_init(decl), *next = cfg_block_init(decl);

            /* Enter a new loop, store reference for break and continue target. */
            old_break_target = break_target;
            old_continue_target = continue_target;
            break_target = next;
            continue_target = increment;

            consume(FOR);
            consume('(');
            if (peek() != ';') {
                expression(parent);
            }
            consume(';');
            if (peek() != ';') {
                parent->jump[0] = top;
                top->expr = expression(top);
                top->jump[0] = next;
                top->jump[1] = body;
            } else {
                /* Infinite loop */
                parent->jump[0] = body;
                top = body;
            }
            consume(';');
            if (peek() != ')') {
                expression(increment);
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
            consume(t);
            parent->jump[0] = (t == CONTINUE) ? 
                continue_target :
                break_target;
            consume(';');
            /* Return orphan node, which is dead code unless there is a label
             * and a goto statement. */
            node = cfg_block_init(decl); 
            break;
        case RETURN:
            consume(RETURN);
            if (peek() != ';')
                parent->expr = expression(parent);
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
            if ((def = sym_lookup(strval)) && def->storage == STC_TYPEDEF) {
                node = declaration(parent, &def);
                break;
            }
            /* todo: handle label statement. */
        }
        case INTEGER_CONSTANT: /* todo: any constant value */
        case STRING:
        case '*':
        case '(':
            expression(parent);
            consume(';');
            node = parent;
            break;
        default:
        {
            const symbol_t *decl;
            node = declaration(parent, &decl);
            break;
        }
    }
    return node;
}

static var_t conditional_expression(block_t *block);
static var_t logical_expression(block_t *block);
static var_t or_expression(block_t *block);
static var_t and_expression(block_t *block);
static var_t equality_expression(block_t *block);
static var_t relational_expression(block_t *block);
static var_t shift_expression(block_t *block);
static var_t additive_expression(block_t *block);
static var_t multiplicative_expression(block_t *block);
static var_t cast_expression(block_t *block);
static var_t postfix_expression(block_t *block);
static var_t unary_expression(block_t *block);
static var_t primary_expression(block_t *block);

static var_t 
expression(block_t *block)
{
    return assignment_expression(block);
}

static var_t 
assignment_expression(block_t *block)
{
    var_t l = conditional_expression(block), r;
    if (peek() == '=') {
        consume('=');
        /* todo: node must be unary-expression or lower (l-value) */
        r = assignment_expression(block);
        l = eval_assign(block, l, r);
    }
    return l;
}

static var_t 
constant_expression()
{
    var_t expr;

    expr = conditional_expression(NULL);
    if (expr.kind != IMMEDIATE) {
        error("Constant expression must be computable at compile time.");
        exit(1);
    }
    return expr;
}

static var_t 
conditional_expression(block_t *block)
{
    var_t v = logical_expression(block);
    if (peek() == '?') {
        consume('?');
        expression(block);
        consume(':');
        conditional_expression(block);
    }
    return v;
}

/* merge AND/OR */
static var_t 
logical_expression(block_t *block)
{
    var_t l, r;
    l = or_expression(block);
    while (peek() == LOGICAL_OR || peek() == LOGICAL_AND) {
        optype_t optype = (token() == LOGICAL_AND) 
            ? IR_OP_LOGICAL_AND : IR_OP_LOGICAL_OR;

        r = and_expression(block);
        l = eval_expr(block, optype, l, r);
    }
    return l;
}

/* merge OR/XOR */
static var_t
or_expression(block_t *block)
{
    var_t l, r;
    l = and_expression(block);
    while (peek() == '|' || peek() == '^') {
        optype_t optype = (token() == '|') 
            ? IR_OP_BITWISE_OR : IR_OP_BITWISE_XOR;

        r = and_expression(block);
        l = eval_expr(block, optype, l, r);
    }
    return l;
}

static var_t
and_expression(block_t *block)
{
    var_t l, r;
    l = equality_expression(block);
    while (peek() == '&') {
        consume('&');
        r = and_expression(block);
        l = eval_expr(block, IR_OP_BITWISE_AND, l, r);
    }
    return l;
}

static var_t
equality_expression(block_t *block)
{
    return relational_expression(block);
}

static var_t
relational_expression(block_t *block)
{
    return shift_expression(block);
}

static var_t
shift_expression(block_t *block)
{
    return additive_expression(block);
}

static var_t
additive_expression(block_t *block)
{
    var_t l, r;
    l = multiplicative_expression(block);
    while (peek() == '+' || peek() == '-') {
        optype_t optype = (token() == '+') ? IR_OP_ADD : IR_OP_SUB;

        r = multiplicative_expression(block);
        l = eval_expr(block, optype, l, r);
    }
    return l;
}

static var_t
multiplicative_expression(block_t *block)
{
    var_t l, r;
    l = cast_expression(block);
    while (peek() == '*' || peek() == '/' || peek() == '%') {
        enum token tok = token();
        optype_t optype = (tok == '*') ?
            IR_OP_MUL : (tok == '/') ?
                IR_OP_DIV : IR_OP_MOD;

        r = cast_expression(block);
        l = eval_expr(block, optype, l, r);
    }
    return l;
}

static var_t
cast_expression(block_t *block)
{
    return unary_expression(block);
}

static var_t
unary_expression(block_t *block)
{
    var_t expr, temp;

    switch (peek()) {
        case '&':
            consume('&');
            expr = cast_expression(block);
            expr = eval_addr(block, expr);
            break;
        case '*':
            consume('*');
            expr = cast_expression(block);
            expr = eval_deref(block, expr);
            break;
        case SIZEOF:
            consume(SIZEOF);
            if (peek() == '(') {
                typetree_t *type;

                consume('(');
                type = declaration_specifiers(NULL);
                if (!type) {
                    expr = expression(NULL);
                } else {
                    if (peek() != ')') {
                        type = declarator(type, NULL);
                    }
                    expr.type = type;
                }
                consume(')');
            } else {
                expr = unary_expression(block);
            }
            if (expr.type->type == FUNCTION) {
                error("Cannot apply 'sizeof' to function type.");
            }
            if (!expr.type->size) {
                error("Cannot apply 'sizeof' to incomplete type.");
            }
            expr = var_long(expr.type->size);
            break;
        case INCREMENT:
            consume(INCREMENT);
            temp = unary_expression(block);
            expr = eval_expr(block, IR_OP_ADD, temp, var_long(1));
            expr = eval_assign(block, temp, expr);
            break;
        case DECREMENT:
            consume(DECREMENT);
            temp = unary_expression(block);
            expr = eval_expr(block, IR_OP_SUB, temp, var_long(1));
            expr = eval_assign(block, temp, expr);
            break;
        default:
            expr = postfix_expression(block);
    }
    return expr;
}

/* This rule is left recursive, build tree bottom up
 */
static var_t
postfix_expression(block_t *block)
{
    var_t root;
    int done;

    root = primary_expression(block);
    done = 0;

    do {
        var_t expr, copy, *arg;
        int i;

        switch (peek()) {
            case '[':
                /* Evaluate a[b] = *(a + b). */
                while (peek() == '[') {
                    consume('[');
                    expr = expression(block);
                    expr = eval_expr(block, IR_OP_MUL, expr, var_long(root.type->next->size));
                    expr = eval_expr(block, IR_OP_ADD, root, expr);
                    root = eval_deref(block, expr);
                    consume(']');
                }
                break;
            case '(':
                /* Evaluation function call. */
                if (root.type->type != FUNCTION) {
                    error("Calling non-function symbol.");
                    exit(1);
                }
                arg = malloc(sizeof(var_t) * root.type->n_args);

                consume('(');
                for (i = 0; i < root.type->n_args; ++i) {
                    if (peek() == ')') {
                        error("Too few arguments to function %s, expected %d but got %d.", root.symbol->name, root.type->n_args, i);
                        exit(1);
                    }
                    arg[i] = assignment_expression(block);
                    /* todo: type check here. */
                    if (i < root.type->n_args - 1)
                        consume(',');
                }
                consume(')');

                for (i = 0; i < root.type->n_args; ++i)
                    param(block, arg[i]);
                root = eval_call(block, root);

                free(arg);
                break;
            case '.':
                root = eval_addr(block, root);
            case ARROW:
                token();
                consume(IDENTIFIER);
                if (root.type->type == POINTER && 
                    root.type->next->type == OBJECT)
                {
                    int i, offset;
                    const typetree_t *field;

                    for (i = offset = 0; i < root.type->next->n_args; ++i) {
                        if (!strcmp(strval, root.type->next->params[i])) {
                            field = root.type->next->args[i];
                            break;
                        }
                        offset += root.type->next->args[i]->size;
                    }
                    if (i == root.type->next->n_args) {
                        error("Invalid field access, no field named %s.", strval);
                        exit(1);
                    }

                    root.kind = DEREF;
                    root.type = field;
                    root.offset += offset;
                    root.lvalue = 1;
                } else {
                    error("Cannot access field of non-object type.");
                    exit(1);
                }
                break;
            case INCREMENT:
                consume(INCREMENT);
                copy = eval_copy(block, root);
                expr = eval_expr(block, IR_OP_ADD, root, var_long(1));
                eval_assign(block, root, expr);
                root = copy;
                break;
            case DECREMENT:
                consume(DECREMENT);
                copy = eval_copy(block, root);
                expr = eval_expr(block, IR_OP_SUB, root, var_long(1));
                eval_assign(block, root, expr);
                root = copy;
                break;
            default:
                done = 1;
                break;
        }
    } while (!done);

    return root;
}

static var_t
primary_expression(block_t *block)
{
    var_t var;
    const char *lbl;
    const symbol_t *sym;

    switch (token()) {
        case IDENTIFIER:
            sym = sym_lookup(strval);
            if (!sym) {
                error("Undefined symbol '%s'.", strval);
                exit(1);
            }
            var = var_direct(sym);
            break;
        case INTEGER_CONSTANT:
            var = var_long(intval);
            break;
        case '(':
            var = expression(block);
            consume(')');
            break;
        case STRING:
            lbl = string_constant_label(strval);
            var = var_string(lbl, strlen(strval) + 1);
            break;
        default:
            error("Unexpected token, not a valid primary expression.");
            exit(1);
    }

    return var;
}
