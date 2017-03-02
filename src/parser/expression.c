#include "expression.h"
#include "declaration.h"
#include "eval.h"
#include "parse.h"
#include "symtab.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

static struct block *cast_expression(
    struct definition *def,
    struct block *block);

static const struct symbol *find_symbol(String name)
{
    const struct symbol *sym = sym_lookup(&ns_ident, name);
    if (!sym) {
        error("Undefined symbol '%s'.", str_raw(name));
        exit(1);
    }

    return sym;
}

/*
 * Parse call to builtin symbol __builtin_va_start, which is the result
 * of calling va_start(arg, s). Return type depends on second input
 * argument.
 */
static struct block *parse__builtin_va_start(
    struct definition *def,
    struct block *block)
{
    Type type;
    const struct member *mb;
    const struct symbol *sym;
    struct token param;

    consume('(');
    block = assignment_expression(def, block);
    consume(',');
    param = consume(IDENTIFIER);

    sym = find_symbol(param.d.string);
    type = def->symbol->type;
    if (!is_vararg(type)) {
        error("Function must be vararg to use va_start.");
        exit(1);
    }

    mb = get_member(type, nmembers(type) - 1);
    if (str_cmp(mb->name, sym->name) || sym->depth != 1) {
        error("Expected last function argument %s as va_start argument.",
            str_raw(mb->name));
        exit(1);
    }

    consume(')');
    eval__builtin_va_start(block, block->expr);
    return block;
}

/*
 * Parse call to builtin symbol __builtin_va_arg, which is the result of
 * calling va_arg(arg, T). Return type depends on second input argument.
 */
static struct block *parse__builtin_va_arg(
    struct definition *def,
    struct block *block)
{
    struct var value;
    Type type;

    consume('(');
    block = assignment_expression(def, block);
    value = eval(def, block, block->expr);
    consume(',');
    type = declaration_specifiers(NULL, NULL);
    if (peek().token != ')') {
        block = declarator(def, block, type, &type, NULL);
    }

    consume(')');
    block->expr = eval_expr(def, block, IR_OP_VA_ARG, value, type);
    return block;
}

/*
 * Special handling for builtin pseudo functions. These are expected to
 * behave as macros, thus should be no problem parsing as function call
 * in primary expression. Constructs like (va_arg)(args, int) will not
 * work with this scheme.
 */
static struct block *primary_expression(
    struct definition *def,
    struct block *block)
{
    const struct symbol *sym;
    struct token tok;

    switch ((tok = next()).token) {
    case IDENTIFIER:
        sym = find_symbol(tok.d.string);
        if (!strcmp("__builtin_va_start", str_raw(sym->name))) {
            block = parse__builtin_va_start(def, block);
        } else if (!strcmp("__builtin_va_arg", str_raw(sym->name))) {
            block = parse__builtin_va_arg(def, block);
        } else {
            block->expr = as_expr(var_direct(sym));
        }
        break;
    case NUMBER:
        block->expr = as_expr(var_numeric(tok.type, tok.d.val));
        assert(is_identity(block->expr));
        break;
    case '(':
        block = expression(def, block);
        consume(')');
        break;
    case STRING:
        sym = sym_add(
            &ns_ident,
            str_init(".LC"),
            type_create(
                T_ARRAY,
                basic_type__char,
                (size_t) tok.d.string.len + 1,
                NULL),
            SYM_STRING_VALUE,
            LINK_INTERN);
        /*
         * Store string value directly on symbol, memory ownership is in
         * string table from previously called str_register. The symbol
         * now exists as if declared static char .LC[] = "...".
         */
        ((struct symbol *) sym)->string_value = tok.d.string;
        /*
         * Result is an IMMEDIATE of type [] char, with a reference to
         * the new symbol containing the string literal. Will decay into
         * char * on evaluation.
         */
        block->expr = as_expr(var_direct(sym));
        assert(is_identity(block->expr));
        assert(block->expr.l.kind == IMMEDIATE);
        break;
    default:
        error("Unexpected '%s', not a valid primary expression.",
            str_raw(tok.d.string));
        exit(1);
    }

    return block;
}

typedef array_of(struct expression) ExprArray;

/*
 * Need to buffer parameter expressions before each function call, and
 * since calls can be nested, the same buffer cannot be used for all.
 */
static array_of(ExprArray *) args;
static unsigned max_depth;

static void cleanup(void)
{
    int i;
    ExprArray *a;

    for (i = 0; i < max_depth; ++i) {
        a = array_get(&args, i);
        array_clear(a);
        free(a);
    }

    array_clear(&args);
}

static ExprArray *push_argument_list(void)
{
    static int init;
    unsigned len;
    ExprArray *list;

    if (!init) {
        atexit(cleanup);
        init = 1;
    }

    len = array_len(&args);
    if (len == max_depth) {
        list = calloc(1, sizeof(*list));
        array_push_back(&args, list);
        max_depth = len + 1;
    } else {
        list = array_get(&args, len);
        args.length += 1;
    }

    return list;
}

static void pop_argument_list(void)
{
    ExprArray *list;
    assert(args.length);

    args.length -= 1;
    list = array_get(&args, array_len(&args));
    array_empty(list);
}

static struct block *postfix_expression(
    struct definition *def,
    struct block *block)
{
    struct expression root;
    struct var value, copy;
    const struct member *mbr;
    const struct symbol *sym;
    struct token tok;
    int i;
    Type type;
    ExprArray *args;

    /*
     * Special case for function calls directly on an identifier which
     * is not declared. Add a declaration like 'extern int foo()' to the
     * current scope.
     */
    if (context.standard == STD_C89) {
        tok = peek();
        if (tok.token == IDENTIFIER && peekn(2).token == '(') {
            sym = sym_lookup(&ns_ident, tok.d.string);
            if (!sym) {
                type = type_create(T_FUNCTION, basic_type__int);
                sym_add(&ns_ident, tok.d.string, type,
                    SYM_DECLARATION, LINK_EXTERN);
            }
        }
    }

    block = primary_expression(def, block);
    root = block->expr;
    while (1) {
        switch ((tok = peek()).token) {
        case '[':
            do {
                /*
                 * Evaluate a[b] = *(a + b). The semantics of pointer
                 * arithmetic takes care of multiplying b with the
                 * correct width.
                 */
                consume('[');
                value = eval(def, block, block->expr);
                block = expression(def, block);
                block->expr =
                    eval_expr(def, block, IR_OP_ADD, value,
                        eval(def, block, block->expr));
                block->expr =
                    as_expr(
                        eval_deref(def, block, eval(def, block, block->expr)));
                consume(']');
            } while (peek().token == '[');
            root = block->expr;
            break;
        case '(':
            type = root.type;
            if (is_pointer(root.type) && is_function(type_deref(root.type))) {
                type = type_deref(root.type);
            } else if (!is_function(root.type)) {
                error("Expression must have type pointer to function, was %t.",
                    type);
                exit(1);
            }
            consume('(');
            args = push_argument_list();
            for (i = 0; i < nmembers(type); ++i) {
                if (peek().token == ')') {
                    error("Too few arguments, expected %d but got %d.",
                        nmembers(type), i);
                    exit(1);
                }
                mbr = get_member(type, i);
                block = assignment_expression(def, block);
                if (!type_equal(block->expr.type, mbr->type)) {
                    value = eval(def, block, block->expr);
                    block->expr =
                        eval_expr(def, block, IR_OP_CAST, value, mbr->type);
                }
                block->expr = eval_param(def, block, block->expr);
                array_push_back(args, block->expr);
                if (i < nmembers(type) - 1) {
                    consume(',');
                }
            }
            while (is_vararg(type) && peek().token != ')') {
                consume(',');
                block = assignment_expression(def, block);
                if (is_float(block->expr.type)) {
                    /*
                     * Single-precision arguments to vararg function are
                     * automatically promoted to double.
                     */
                    value = eval(def, block, block->expr);
                    block->expr = eval_expr(def, block, IR_OP_CAST,
                        value, basic_type__double);
                }
                block->expr = eval_param(def, block, block->expr);
                array_push_back(args, block->expr);
                i++;
            }
            consume(')');
            for (i = 0; i < array_len(args); ++i) {
                param(block, array_get(args, i));
            }
            value = eval(def, block, root);
            block->expr = eval_expr(def, block, IR_OP_CALL, value);
            root = block->expr;
            pop_argument_list();
            break;
        case '.':
            consume('.');
            tok = consume(IDENTIFIER);
            mbr = find_type_member(root.type, tok.d.string);
            if (!mbr) {
                error("Invalid access, no member named '%s'.",
                    str_raw(tok.d.string));
                exit(1);
            }
            value = eval(def, block, root);
            value.type = mbr->type;
            value.field_width = mbr->field_width;
            value.field_offset = mbr->field_offset;
            value.offset += mbr->offset;
            block->expr = as_expr(value);
            root = block->expr;
            break;
        case ARROW:
            consume(ARROW);
            tok = consume(IDENTIFIER);
            if (is_pointer(root.type)
                && is_struct_or_union(type_deref(root.type)))
            {
                mbr = find_type_member(type_deref(root.type), tok.d.string);
                if (!mbr) {
                    error("Invalid access, no member named '%s'.",
                        str_raw(tok.d.string));
                    exit(1);
                }
                /*
                 * Make it look like a pointer to the member type, then
                 * perform normal dereferencing.
                 */
                value = eval(def, block, root);
                value.type = type_create(T_POINTER, mbr->type);
                value = eval_deref(def, block, value);
                value.field_width = mbr->field_width;
                value.field_offset = mbr->field_offset;
                value.offset += mbr->offset;
                block->expr = as_expr(value);
                root = block->expr;
            } else {
                error("Invalid member access to type %t.", root.type);
                exit(1);
            }
            break;
        case INCREMENT:
            consume(INCREMENT);
            value = eval(def, block, root);
            copy = eval_copy(def, block, value);
            root = eval_expr(def, block, IR_OP_ADD, value, var_int(1));
            eval_assign(def, block, value, root);
            block->expr = as_expr(copy);
            root = block->expr;
            break;
        case DECREMENT:
            consume(DECREMENT);
            value = eval(def, block, root);
            copy = eval_copy(def, block, value);
            root = eval_expr(def, block, IR_OP_SUB, value, var_int(1));
            eval_assign(def, block, value, root);
            block->expr = as_expr(copy);
            root = block->expr;
            break;
        default:
            block->expr = root;
            return block;
        }
    }
}

static struct block *unary_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct block *head, *tail;
    const struct symbol *sym;
    Type type;

    switch (peek().token) {
    case '&':
        consume('&');
        block = cast_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = as_expr(eval_addr(def, block, value));
        break;
    case '*':
        consume('*');
        block = cast_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = as_expr(eval_deref(def, block, value));
        break;
    case '!':
        consume('!');
        block = cast_expression(def, block);
        switch (block->expr.op) {
        case IR_OP_EQ:
            block->expr.op = IR_OP_NE;
            break;
        case IR_OP_NE:
            block->expr.op = IR_OP_EQ;
            break;
        case IR_OP_GE:
            block->expr.op = IR_OP_GT;
            value = block->expr.l;
            block->expr.l = block->expr.r;
            block->expr.r = value;
            break;
        case IR_OP_GT:
            block->expr.op = IR_OP_GE;
            value = block->expr.l;
            block->expr.l = block->expr.r;
            block->expr.r = value;
            break;
        default:
            value = eval(def, block, block->expr);
            block->expr = eval_expr(def, block, IR_OP_EQ, var_int(0), value);
            break;
        }
        break;
    case '~':
        consume('~');
        block = cast_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_expr(def, block, IR_OP_NOT, value);
        break;
    case '+':
        consume('+');
        block = cast_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_unary_plus(value);
        break;
    case '-':
        consume('-');
        block = cast_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_expr(def, block, IR_OP_NEG, value);
        break;
    case SIZEOF:
        consume(SIZEOF);
        if (peek().token == '(') {
            switch (peekn(2).token) {
            case IDENTIFIER:
                sym = sym_lookup(&ns_ident, peekn(2).d.string);
                if (!sym || sym->symtype != SYM_TYPEDEF)
                    goto exprsize;;
            case FIRST(type_name):
                consume('(');
                type = declaration_specifiers(NULL, NULL);
                if (peek().token != ')') {
                    block = declarator(def, block, type, &type, NULL);
                }
                consume(')');
                break;
            default: goto exprsize;
            }
        } else {
exprsize:   head = cfg_block_init(def);
            tail = unary_expression(def, head);
            type = tail->expr.type;
        }
        if (!size_of(type)) {
            if (is_vla(type)) {
                block->expr = eval_vla_size(def, block, type);
            } else {
                error("Cannot apply 'sizeof' to incomplete type.");
                exit(1);
            }
        } else {
            value = imm_unsigned(basic_type__unsigned_long, size_of(type));
            block->expr = as_expr(value);
        }
        break;
    case ALIGNOF:
        next();
        consume('(');
        type = declaration_specifiers(NULL, NULL);
        if (peek().token != ')') {
            block = declarator(def, block, type, &type, NULL);
        }
        if (is_function(type)) {
            error("Cannot apply '_Alignof' to function type.");
        }
        if (!size_of(type)) {
            error("Cannot apply '_Alignof' to incomplete type.");
        }
        value = imm_unsigned(basic_type__unsigned_long, type_alignment(type));
        block->expr = as_expr(value);
        consume(')');
        break;
    case INCREMENT:
        consume(INCREMENT);
        block = unary_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_expr(def, block, IR_OP_ADD, value, var_int(1));
        block->expr = as_expr(eval_assign(def, block, value, block->expr));
        break;
    case DECREMENT:
        consume(DECREMENT);
        block = unary_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_expr(def, block, IR_OP_SUB, value, var_int(1));
        block->expr = as_expr(eval_assign(def, block, value, block->expr));
        break;
    default:
        block = postfix_expression(def, block);
        break;
    }

    return block;
}

/*
 * This rule needs two lookahead; to see beyond the initial parenthesis
 * whether it is actually a cast or an expression.
 */
static struct block *cast_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct token tok;
    struct symbol *sym;
    Type type;

    if (peek().token == '(') {
        tok = peekn(2);
        switch (tok.token) {
        case IDENTIFIER:
            sym = sym_lookup(&ns_ident, tok.d.string);
            if (!sym || sym->symtype != SYM_TYPEDEF)
                break;
        case FIRST(type_name):
            consume('(');
            type = declaration_specifiers(NULL, NULL);
            if (peek().token != ')') {
                block = declarator(def, block, type, &type, NULL);
            }
            consume(')');
            block = cast_expression(def, block);
            value = eval(def, block, block->expr);
            block->expr = eval_expr(def, block, IR_OP_CAST, value, type);
            return block;
        default:
            break;
        }
    }

    return unary_expression(def, block);
}

static struct block *multiplicative_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct token t;

    block = cast_expression(def, block);
    while (1) {
        t = peek();
        if (t.token == '*') {
            consume('*');
            value = eval(def, block, block->expr);
            block = cast_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_MUL, value,
                eval(def, block, block->expr));
        } else if (t.token == '/') {
            consume('/');
            value = eval(def, block, block->expr);
            block = cast_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_DIV, value,
                eval(def, block, block->expr));
        } else if (t.token == '%') {
            consume('%');
            value = eval(def, block, block->expr);
            block = cast_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_MOD, value,
                eval(def, block, block->expr));
        } else break;
    }

    return block;
}

static struct block *additive_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct token t;

    block = multiplicative_expression(def, block);
    while (1) {
        t = peek();
        if (t.token == '+') {
            consume('+');
            value = eval(def, block, block->expr);
            block = multiplicative_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_ADD, value,
                eval(def, block, block->expr));
        } else if (t.token == '-') {
            consume('-');
            value = eval(def, block, block->expr);
            block = multiplicative_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_SUB, value,
                eval(def, block, block->expr));
        } else break;
    }

    return block;
}

static struct block *shift_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct token t;

    block = additive_expression(def, block);
    while (1) {
        t = peek();
        if (t.token == LSHIFT) {
            consume(LSHIFT);
            value = eval(def, block, block->expr);
            block = additive_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_SHL, value,
                eval(def, block, block->expr));
        } else if (t.token == RSHIFT) {
            consume(RSHIFT);
            value = eval(def, block, block->expr);
            block = additive_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_SHR, value,
                eval(def, block, block->expr));
        } else break;
    }

    return block;
}

static struct block *relational_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;

    block = shift_expression(def, block);
    while (1) {
        switch (peek().token) {
        case '<':
            consume('<');
            value = eval(def, block, block->expr);
            block = shift_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_GT,
                eval(def, block, block->expr), value);
            break;
        case '>':
            consume('>');
            value = eval(def, block, block->expr);
            block = shift_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_GT,
                value, eval(def, block, block->expr));
            break;
        case LEQ:
            consume(LEQ);
            value = eval(def, block, block->expr);
            block = shift_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_GE,
                eval(def, block, block->expr), value);
            break;
        case GEQ:
            consume(GEQ);
            value = eval(def, block, block->expr);
            block = shift_expression(def, block);
            block->expr = eval_expr(def, block, IR_OP_GE,
                value, eval(def, block, block->expr));
            break;
        default:
            return block;
        }
    }
}

static struct block *equality_expression(
    struct definition *def,
    struct block *block)
{
    enum optype op;
    struct var value;
    struct token t;

    block = relational_expression(def, block);
    while (1) {
        t = peek();
        if (t.token == EQ) {
            consume(EQ);
            op = IR_OP_EQ;
        } else if (t.token == NEQ) {
            consume(NEQ);
            op = IR_OP_NE;
        } else break;
        value = eval(def, block, block->expr);
        block = relational_expression(def, block);
        block->expr =
            eval_expr(def, block, op, value, eval(def, block, block->expr));
    }

    return block;
}

static struct block *and_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;

    block = equality_expression(def, block);
    while (peek().token == '&') {
        consume('&');
        value = eval(def, block, block->expr);
        block = equality_expression(def, block);
        block->expr = eval_expr(def, block, IR_OP_AND, value,
            eval(def, block, block->expr));
    }

    return block;
}

static struct block *exclusive_or_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;

    block = and_expression(def, block);
    while (peek().token == '^') {
        consume('^');
        value = eval(def, block, block->expr);
        block = and_expression(def, block);
        block->expr = eval_expr(def, block, IR_OP_XOR, value,
            eval(def, block, block->expr));
    }

    return block;
}

static struct block *inclusive_or_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;

    block = exclusive_or_expression(def, block);
    while (peek().token == '|') {
        consume('|');
        value = eval(def, block, block->expr);
        block = exclusive_or_expression(def, block);
        block->expr = eval_expr(def, block, IR_OP_OR, value,
            eval(def, block, block->expr));
    }

    return block;
}

static struct block *logical_and_expression(
    struct definition *def,
    struct block *block)
{
    struct block *right;

    block = inclusive_or_expression(def, block);
    if (peek().token == LOGICAL_AND) {
        consume(LOGICAL_AND);
        right = cfg_block_init(def);
        block = eval_logical_and(
            def, block, right, logical_and_expression(def, right));
    }

    return block;
}

static struct block *logical_or_expression(
    struct definition *def,
    struct block *block)
{
    struct block *right;

    block = logical_and_expression(def, block);
    if (peek().token == LOGICAL_OR) {
        consume(LOGICAL_OR);
        right = cfg_block_init(def);
        block = eval_logical_or(
            def, block, right, logical_or_expression(def, right));
    }

    return block;
}

static struct block *conditional_expression(
    struct definition *def,
    struct block *block)
{
    struct expression condition;
    struct block *t, *f, *next;

    block = logical_or_expression(def, block);
    if (peek().token == '?') {
        /* todo: type checking on immediate paths. */
        condition = block->expr;
        consume('?');
        t = cfg_block_init(def);
        if (is_immediate_true(block->expr)) {
            block = expression(def, block);
            consume(':');
            conditional_expression(def, t); /* throwaway */
        } else if (is_immediate_false(block->expr)) {
            expression(def, t); /* throwaway */
            consume(':');
            block = conditional_expression(def, block);
        } else {
            f = cfg_block_init(def);
            next = cfg_block_init(def);
            block->jump[0] = f;
            block->jump[1] = t;
            t = expression(def, t);
            t->jump[0] = next;
            consume(':');
            f = conditional_expression(def, f);
            f->jump[0] = next;
            next->expr = eval_conditional(def, condition, t, f);
            block = next;
        }
    }

    return block;
}

struct block *assignment_expression(
    struct definition *def,
    struct block *block)
{
    enum optype op = IR_OP_CAST;
    struct var target, value;

    block = conditional_expression(def, block);
    switch (peek().token) {
    case '=':
        consume('=');
        break;
    case MUL_ASSIGN:
        consume(MUL_ASSIGN);
        op = IR_OP_MUL;
        break;
    case DIV_ASSIGN:
        consume(DIV_ASSIGN);
        op = IR_OP_DIV;
        break;
    case MOD_ASSIGN:
        consume(MOD_ASSIGN);
        op = IR_OP_MOD;
        break;
    case PLUS_ASSIGN:
        consume(PLUS_ASSIGN);
        op = IR_OP_ADD;
        break;
    case MINUS_ASSIGN:
        consume(MINUS_ASSIGN);
        op = IR_OP_SUB;
        break;
    case AND_ASSIGN:
        consume(AND_ASSIGN);
        op = IR_OP_AND;
        break;
    case OR_ASSIGN:
        consume(OR_ASSIGN);
        op = IR_OP_OR;
        break;
    case XOR_ASSIGN:
        consume(XOR_ASSIGN);
        op = IR_OP_XOR;
        break;
    case RSHIFT_ASSIGN:
        consume(RSHIFT_ASSIGN);
        op = IR_OP_SHR;
        break;
    case LSHIFT_ASSIGN:
        consume(LSHIFT_ASSIGN);
        op = IR_OP_SHL;
        break;
    default:
        return block;
    }

    target = eval(def, block, block->expr);
    block = assignment_expression(def, block);
    if (op != IR_OP_CAST) {
        value = eval(def, block, block->expr);
        block->expr = eval_expr(def, block, op, target, value);
    }

    value = eval_assign(def, block, target, block->expr);
    block->expr = as_expr(value);
    return block;
}

struct var constant_expression(void)
{
    struct block
        *head = cfg_block_init(NULL),
        *tail;

    tail = conditional_expression(NULL, head);
    if (tail != head || !is_immediate(tail->expr)) {
        error("Constant expression must be computable at compile time.");
        exit(1);
    }

    return eval(NULL, tail, tail->expr);
}

struct block *expression(struct definition *def, struct block *block)
{
    block = assignment_expression(def, block);
    while (peek().token == ',') {
        consume(',');
        if (has_side_effects(block->expr)) {
            eval(def, block, block->expr);
        }
        block = assignment_expression(def, block);
    }

    return block;
}
