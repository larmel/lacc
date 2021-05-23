#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "declaration.h"
#include "eval.h"
#include "expression.h"
#include "initializer.h"
#include "parse.h"
#include "symtab.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/token.h>

#include <assert.h>

typedef array_of(struct expression) ExprArray;

/*
 * Need to buffer parameter expressions before each function call, and
 * since calls can be nested, the same buffer cannot be used for all.
 */
static array_of(ExprArray *) args;
static int max_depth;

static String
    str__builtin_va_start,
    str__builtin_va_arg,
    str__builtin_alloca;

INTERNAL void expression_parse_init(void)
{
    str__builtin_va_start = str_c("__builtin_va_start");
    str__builtin_va_arg = str_c("__builtin_va_arg");
    str__builtin_alloca = str_c("__builtin_alloca");
}

INTERNAL void expression_parse_finalize(void)
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

    consume('(');
    block = assignment_expression(def, block);
    consume(',');
    consume(IDENTIFIER);

    sym = find_symbol(access_token(0)->d.string);
    type = def->symbol->type;
    if (!is_vararg(type)) {
        error("Function must be vararg to use va_start.");
        exit(1);
    }

    mb = get_member(type, nmembers(type) - 1);
    if (!str_eq(mb->name, sym->name) || sym->depth != 1) {
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
    type = declaration_specifiers(NULL);
    if (peek() != ')') {
        block = declarator(def, block, type, &type, NULL);
    }

    consume(')');
    block->expr = eval_va_arg(def, block, value, type);
    return block;
}

/*
 * Implement alloca as a normal VLA.
 *
 *     void *ptr = alloca(n + 1);
 *
 * is translated to
 *
 *     size_t len = n + 1;
 *     char sym[len];
 *     void *ptr = (void *) sym;
 */
static struct block *parse__builtin_alloca(
    struct definition *def,
    struct block *block)
{
    struct var t1;
    struct symbol *sym;

    consume('(');
    block = assignment_expression(def, block);
    consume(')');

    t1 = create_var(def, basic_type__unsigned_long);
    eval_assign(def, block, t1, block->expr);

    sym = sym_create_temporary(type_create_vla(basic_type__char, t1.value.symbol));
    array_push_back(&def->locals, sym);

    block = declare_vla(def, block, sym);
    block->expr = eval_cast(def, block, var_direct(sym),
        type_create_pointer(basic_type__void));
    return block;
}

/*
 * Special handling for builtin pseudo functions. These are expected to
 * behave as macros, thus should be no problem parsing as function call
 * in primary expression. Constructs like (va_arg)(args, int) will not
 * work with this scheme.
 *
 * String constants become IMMEDIATE of type [] char, with a reference
 * to the new symbol containing the string literal. Decays into char *
 * on evaluation.
 */
static struct block *primary_expression(
    struct definition *def,
    struct block *block)
{
    const struct symbol *sym;
    const struct token *tok;

    next();
    tok = access_token(0);
    switch (tok->token) {
    case IDENTIFIER:
        sym = find_symbol(tok->d.string);
        if (str_eq(str__builtin_va_start, sym->name)) {
            block = parse__builtin_va_start(def, block);
        } else if (str_eq(str__builtin_va_arg, sym->name)) {
            block = parse__builtin_va_arg(def, block);
        } else if (str_eq(str__builtin_alloca, sym->name)) {
            block = parse__builtin_alloca(def, block);
        } else {
            block->expr = as_expr(var_direct(sym));
        }
        break;
    case NUMBER:
        block->expr = as_expr(var_numeric(tok->type, tok->d.val));
        assert(is_identity(block->expr));
        break;
    case '(':
        block = expression(def, block);
        consume(')');
        break;
    case STRING:
        sym = sym_create_string(tok->d.string);
        block->expr = as_expr(var_direct(sym));
        assert(is_identity(block->expr));
        assert(block->expr.l.kind == DIRECT);
        break;
    default:
        error("Unexpected '%s', not a valid primary expression.",
            str_raw(tok->d.string));
        exit(1);
    }

    return block;
}

static ExprArray *push_argument_list(void)
{
    int len;
    ExprArray *list;

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

static struct block *postfix(
    struct definition *def,
    struct block *block)
{
    int i;
    struct expression root;
    struct var value, copy;
    const struct member *mbr;
    Type type;
    String str;
    ExprArray *args;

    root = block->expr;

    while (1) {
        switch (peek()) {
        case '[':
            do {
                /*
                 * Evaluate a[b] = *(a + b). The semantics of pointer
                 * arithmetic takes care of multiplying b with the
                 * correct width.
                 */
                next();
                value = eval(def, block, block->expr);
                block = expression(def, block);
                block->expr =
                    eval_add(def, block, value, eval(def, block, block->expr));
                block->expr =
                    as_expr(
                        eval_deref(def, block, eval(def, block, block->expr)));
                consume(']');
            } while (peek() == '[');
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
            next();
            args = push_argument_list();
            for (i = 0; i < nmembers(type); ++i) {
                if (peek() == ')') {
                    error("Too few arguments, expected %d but got %d.",
                        nmembers(type), i);
                    exit(1);
                }
                mbr = get_member(type, i);
                block = assignment_expression(def, block);
                block->expr =
                    eval_prepare_arg(def, block, block->expr, mbr->type);
                array_push_back(args, block->expr);
                if (i < nmembers(type) - 1) {
                    consume(',');
                }
            }
            if (is_vararg(type)) {
                while (peek() != ')') {
                    consume(',');
                    block = assignment_expression(def, block);
                    block->expr = eval_prepare_vararg(def, block, block->expr);
                    array_push_back(args, block->expr);
                    i++;
                }
            }
            consume(')');
            for (i = 0; i < array_len(args); ++i) {
                eval_push_param(block, array_get(args, i));
            }
            value = eval(def, block, root);
            block->expr = eval_call(def, block, value);
            root = block->expr;
            pop_argument_list();
            break;
        case '.':
            next();
            consume(IDENTIFIER);
            str = access_token(0)->d.string;
            mbr = find_type_member(root.type, str, NULL);
            if (!mbr) {
                error("Invalid access, no member named '%s'.", str_raw(str));
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
            next();
            consume(IDENTIFIER);
            str = access_token(0)->d.string;
            value = eval_deref(def, block, eval(def, block, root));
            if (is_struct_or_union(value.type)) {
                mbr = find_type_member(value.type, str, NULL);
                if (!mbr) {
                    error("Invalid access, %t has no member named '%s'.",
                        value.type, str_raw(str));
                    exit(1);
                }
                value.type = mbr->type;
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
            next();
            value = eval(def, block, root);
            copy = eval_copy(def, block, value);
            root = eval_add(def, block, value, var_int(1));
            eval_assign(def, block, value, root);
            block->expr = as_expr(copy);
            root = block->expr;
            break;
        case DECREMENT:
            next();
            value = eval(def, block, root);
            copy = eval_copy(def, block, value);
            root = eval_sub(def, block, value, var_int(1));
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

static struct block *postfix_expression(
    struct definition *def,
    struct block *block)
{
    const struct symbol *sym;
    String str;
    Type type;

    /*
     * Special case for function calls directly on an identifier which
     * is not declared. Add a declaration like 'extern int foo()' to the
     * current scope.
     */
    if (context.standard == STD_C89) {
        if (peek() == IDENTIFIER && peekn(2) == '(') {
            str = access_token(1)->d.string;
            sym = sym_lookup(&ns_ident, str);
            if (!sym) {
                type = type_create_function(basic_type__int);
                sym_add(&ns_ident, str, type, SYM_DECLARATION, LINK_EXTERN);
            }
        }
    }

    block = primary_expression(def, block);
    return postfix(def, block);
}

static struct block *unary_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct block *head, *tail;
    const struct symbol *sym;
    String str;
    Type type;

    switch (peek()) {
    case '&':
        next();
        block = cast_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = as_expr(eval_addr(def, block, value));
        break;
    case '*':
        next();
        block = cast_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = as_expr(eval_deref(def, block, value));
        break;
    case '!':
        next();
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
            block->expr = eval_cmp_eq(def, block, var_int(0), value);
            break;
        }
        break;
    case '~':
        next();
        block = cast_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_not(def, block, value);
        break;
    case '+':
        next();
        block = cast_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_unary_plus(value);
        break;
    case '-':
        next();
        block = cast_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_neg(def, block, value);
        break;
    case SIZEOF:
        next();
        if (peek() == '(') {
            switch (peekn(2)) {
            case IDENTIFIER:
                str = access_token(2)->d.string;
                sym = sym_lookup(&ns_ident, str);
                if (!sym || sym->symtype != SYM_TYPEDEF)
                    goto exprsize;;
            case FIRST(type_name):
                consume('(');
                type = declaration_specifiers(NULL);
                if (peek() != ')') {
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
        if (is_complete(type)) {
            if (is_vla(type)) {
                block->expr = eval_vla_size(def, block, type);
            } else {
                value = imm_unsigned(basic_type__unsigned_long, size_of(type));
                block->expr = as_expr(value);
            }
        } else {
            error("Cannot apply 'sizeof' to incomplete type.");
            exit(1);
        }
        break;
    case ALIGNOF:
        next();
        consume('(');
        type = declaration_specifiers(NULL);
        if (peek() != ')') {
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
        next();
        block = unary_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_add(def, block, value, var_int(1));
        block->expr = as_expr(eval_assign(def, block, value, block->expr));
        break;
    case DECREMENT:
        next();
        block = unary_expression(def, block);
        value = eval(def, block, block->expr);
        block->expr = eval_sub(def, block, value, var_int(1));
        block->expr = as_expr(eval_assign(def, block, value, block->expr));
        break;
    default:
        block = postfix_expression(def, block);
        break;
    }

    return block;
}

static struct block *compound_literal(
    struct definition *def,
    struct block *block,
    Type type)
{
    struct var var;
    struct symbol *sym;

    sym = sym_create_unnamed(type);
    if (sym->linkage == LINK_INTERN) {
        def = cfg_init();
        initializer(def, def->body, sym);
        cfg_define(def, sym);
    } else {
        array_push_back(&def->locals, sym);
        block = initializer(def, block, sym);
    }

    var = var_direct(sym);
    block->expr = as_expr(var);
    return block;
}

/*
 * This rule needs two lookahead; to see beyond the initial parenthesis
 * whether it is actually a cast or an expression.
 *
 * Also handle compound literals, which are really postfix expressions,
 * but have the same prefix as a cast expression.
 */
static struct block *cast_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;
    struct symbol *sym;
    String str;
    Type type;

    if (peek() == '(') {
        switch (peekn(2)) {
        case IDENTIFIER:
            str = access_token(2)->d.string;
            sym = sym_lookup(&ns_ident, str);
            if (!sym || sym->symtype != SYM_TYPEDEF)
                break;
        case FIRST(type_name):
            next();
            type = declaration_specifiers(NULL);
            if (peek() != ')') {
                block = declarator(def, block, type, &type, NULL);
            }
            consume(')');
            if (peek() == '{') {
                block = compound_literal(def, block, type);
                return postfix(def, block);
            } else {
                block = cast_expression(def, block);
                value = eval(def, block, block->expr);
                block->expr = eval_cast(def, block, value, type);
                return block;
            }
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
    enum token_type t;

    block = cast_expression(def, block);
    while (1) {
        t = peek();
        if (t == '*') {
            next();
            value = eval(def, block, block->expr);
            block = cast_expression(def, block);
            block->expr = eval_mul(def, block, value,
                eval(def, block, block->expr));
        } else if (t == '/') {
            next();
            value = eval(def, block, block->expr);
            block = cast_expression(def, block);
            block->expr = eval_div(def, block, value,
                eval(def, block, block->expr));
        } else if (t == '%') {
            next();
            value = eval(def, block, block->expr);
            block = cast_expression(def, block);
            block->expr = eval_mod(def, block, value,
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
    enum token_type t;

    block = multiplicative_expression(def, block);
    while (1) {
        t = peek();
        if (t == '+') {
            next();
            value = eval(def, block, block->expr);
            block = multiplicative_expression(def, block);
            block->expr = eval_add(def, block, value,
                eval(def, block, block->expr));
        } else if (t == '-') {
            next();
            value = eval(def, block, block->expr);
            block = multiplicative_expression(def, block);
            block->expr = eval_sub(def, block, value,
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
    enum token_type t;

    block = additive_expression(def, block);
    while (1) {
        t = peek();
        if (t == LSHIFT) {
            next();
            value = eval(def, block, block->expr);
            block = additive_expression(def, block);
            block->expr =
                eval_lshift(def, block, value, eval(def, block, block->expr));
        } else if (t == RSHIFT) {
            next();
            value = eval(def, block, block->expr);
            block = additive_expression(def, block);
            block->expr =
                eval_rshift(def, block, value, eval(def, block, block->expr));
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
        switch (peek()) {
        case '<':
            next();
            value = eval(def, block, block->expr);
            block = shift_expression(def, block);
            block->expr = eval_cmp_gt(def, block,
                eval(def, block, block->expr), value);
            break;
        case '>':
            next();
            value = eval(def, block, block->expr);
            block = shift_expression(def, block);
            block->expr = eval_cmp_gt(def, block,
                value, eval(def, block, block->expr));
            break;
        case LEQ:
            next();
            value = eval(def, block, block->expr);
            block = shift_expression(def, block);
            block->expr = eval_cmp_ge(def, block,
                eval(def, block, block->expr), value);
            break;
        case GEQ:
            next();
            value = eval(def, block, block->expr);
            block = shift_expression(def, block);
            block->expr = eval_cmp_ge(def, block,
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
    struct var l, r;
    enum token_type t;

    block = relational_expression(def, block);
    while (1) {
        t = peek();
        if (t == EQ || t == NEQ) {
            next();
        } else break;

        l = eval(def, block, block->expr);
        block = relational_expression(def, block);
        r = eval(def, block, block->expr);
        if (t == EQ) {
            block->expr = eval_cmp_eq(def, block, l, r);
        } else {
            block->expr = eval_cmp_ne(def, block, l, r);
        }
    }

    return block;
}

static struct block *and_expression(
    struct definition *def,
    struct block *block)
{
    struct var value;

    block = equality_expression(def, block);
    while (try_consume('&')) {
        value = eval(def, block, block->expr);
        block = equality_expression(def, block);
        block->expr = eval_and(def, block, value,
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
    while (try_consume('^')) {
        value = eval(def, block, block->expr);
        block = and_expression(def, block);
        block->expr = eval_xor(def, block, value,
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
    while (try_consume('|')) {
        value = eval(def, block, block->expr);
        block = exclusive_or_expression(def, block);
        block->expr = eval_or(def, block, value, eval(def, block, block->expr));
    }

    return block;
}

static struct block *logical_and_expression(
    struct definition *def,
    struct block *block)
{
    struct block *top, *right;

    block = inclusive_or_expression(def, block);
    if (try_consume(LOGICAL_AND)) {
        top = cfg_block_init(def);
        right = logical_and_expression(def, top);
        block = eval_logical_and(def, block, top, right);
    }

    return block;
}

static struct block *logical_or_expression(
    struct definition *def,
    struct block *block)
{
    struct block *top, *right;

    block = logical_and_expression(def, block);
    if (try_consume(LOGICAL_OR)) {
        top = cfg_block_init(def);
        right = logical_or_expression(def, top);
        block = eval_logical_or(def, block, top, right);
    }

    return block;
}

INTERNAL struct block *conditional_expression(
    struct definition *def,
    struct block *block)
{
    int b;
    struct var temp;
    struct block *left, *right;
    Type type;

    block = logical_or_expression(def, block);
    if (!try_consume('?')) {
        return block;
    }

    block = scalar(def, block, "Conditional");
    if (is_immediate(block->expr)) {
        b = immediate_bool(block->expr);
        if (b == 1) {
            left = block = expression(def, block);
            consume(':');
            right = cfg_block_init(def);
            right = conditional_expression(def, right);
        } else {
            assert(b == 0);
            left = cfg_block_init(def);
            left = expression(def, left);
            consume(':');
            right = block = conditional_expression(def, block);
        }

        type = eval_conditional(def, left, right);
        if (is_void(type)) {
            block->expr = as_expr(var_void());
        } else {
            block->expr = eval_cast(def, block,
                eval(def, block, block->expr), type);
        }
    } else {
        left = cfg_block_init(def);
        right = cfg_block_init(def);
        block->jump[0] = right;
        block->jump[1] = left;
        block = cfg_block_init(def);
        left = expression(def, left);
        left->jump[0] = block;
        consume(':');
        right = conditional_expression(def, right);
        right->jump[0] = block;
        type = eval_conditional(def, left, right);
        if (is_void(type)) {
            block->expr = as_expr(var_void());
        } else {
            temp = create_var(def, type);
            left->expr = as_expr(eval_assign(def, left, temp, left->expr));
            right->expr = as_expr(eval_assign(def, right, temp, right->expr));
            block->expr = as_expr(temp);
        }
    }

    return block;
}

INTERNAL struct block *assignment_expression(
    struct definition *def,
    struct block *block)
{
    enum token_type t;
    struct var target, value;

    block = conditional_expression(def, block);
    t = peek();
    switch (t) {
    case '=':
    case MUL_ASSIGN:
    case DIV_ASSIGN:
    case MOD_ASSIGN:
    case PLUS_ASSIGN:
    case MINUS_ASSIGN:
    case AND_ASSIGN:
    case OR_ASSIGN:
    case XOR_ASSIGN:
    case RSHIFT_ASSIGN:
    case LSHIFT_ASSIGN:
        next();
        break;
    default:
        return block;
    }

    target = eval(def, block, block->expr);
    block = assignment_expression(def, block);
    if (t != '=') {
        value = eval(def, block, block->expr);
        switch (t) {
        default:
            assert(0);
        case MUL_ASSIGN:
            block->expr = eval_mul(def, block, target, value);
            break;
        case DIV_ASSIGN:
            block->expr = eval_div(def, block, target, value);
            break;
        case MOD_ASSIGN:
            block->expr = eval_mod(def, block, target, value);
            break;
        case PLUS_ASSIGN:
            block->expr = eval_add(def, block, target, value);
            break;
        case MINUS_ASSIGN:
            block->expr = eval_sub(def, block, target, value);
            break;
        case AND_ASSIGN:
            block->expr = eval_and(def, block, target, value);
            break;
        case OR_ASSIGN:
            block->expr = eval_or(def, block, target, value);
            break;
        case XOR_ASSIGN:
            block->expr = eval_xor(def, block, target, value);
            break;
        case RSHIFT_ASSIGN:
            block->expr = eval_rshift(def, block, target, value);
            break;
        case LSHIFT_ASSIGN:
            block->expr = eval_lshift(def, block, target, value);
            break;
        }
    }

    value = eval_assign(def, block, target, block->expr);
    block->expr = as_expr(value);
    return block;
}

INTERNAL struct var constant_expression(void)
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

INTERNAL struct block *expression(struct definition *def, struct block *block)
{
    block = assignment_expression(def, block);
    while (try_consume(',')) {
        if (has_side_effects(block->expr)) {
            eval(def, block, block->expr);
        }

        block = assignment_expression(def, block);
    }

    return block;
}
