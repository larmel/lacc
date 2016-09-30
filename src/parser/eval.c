#include "eval.h"
#include "declaration.h"
#include "parse.h"
#include "symtab.h"
#include "type.h"
#include <lacc/context.h>
#include <lacc/ir.h>

#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdlib.h>

int is_immediate_true(struct expression expr)
{
    union value val;
    const struct typetree *type;

    if (is_identity(expr) && expr.l.kind == IMMEDIATE) {
        val = expr.l.imm;
        type = expr.type;
        assert(is_scalar(type));
        return
            (is_signed(type)) ? val.i != 0l :
            (is_unsigned(type) || is_pointer(type)) ? val.u != 0ul :
            (is_float(type)) ? val.f != 0.0f : val.d != 0.0;
    }

    return 0;
}

int is_immediate_false(struct expression expr)
{
    union value val;
    const struct typetree *type;

    if (is_identity(expr) && expr.l.kind == IMMEDIATE) {
        val = expr.l.imm;
        type = expr.type;
        assert(is_scalar(type));
        return
            is_signed(type) ? val.i == 0l :
            is_unsigned(type) || is_pointer(type) ? val.u == 0ul :
            is_float(type) ? val.f == 0.0f : val.d == 0.0;
    }

    return 0;
}

static int is_nullptr(struct var val)
{
    return (is_integer(val.type) || is_pointer(val.type))
        && is_immediate_false(as_expr(val));
}

static int is_string(struct var val)
{
    return val.kind == IMMEDIATE && val.symbol
        && val.symbol->symtype == SYM_STRING_VALUE;
}

static struct var var_void(void)
{
    struct var var = {0};

    var.kind = IMMEDIATE;
    var.type = &basic_type__void;
    return var;
}

static struct var create_var(
    struct definition *def,
    const struct typetree *type)
{
    struct symbol *tmp;
    struct var res;
    assert(def);

    tmp = sym_create_temporary(type);
    res = var_direct(tmp);
    array_push_back(&def->locals, tmp);
    res.lvalue = 1;
    return res;
}

struct var var_direct(const struct symbol *sym)
{
    struct var var = {0};

    var.type = &sym->type;
    var.symbol = sym;

    switch (sym->symtype) {
    case SYM_CONSTANT:
        var.kind = IMMEDIATE;
        var.imm = sym->constant_value;
        break;
    case SYM_STRING_VALUE:
        var.kind = IMMEDIATE;
        break;
    default:
        assert(sym->symtype != SYM_LABEL);
        var.kind = DIRECT;
        var.lvalue = str_raw(sym->name)[0] != '.';
        break;
    }

    return var;
}

struct var var_int(int value)
{
    struct var var = {0};
    var.kind = IMMEDIATE;
    var.type = &basic_type__int;
    var.imm.i = value;
    return var;
}

struct var var_numeric(struct number n)
{
    struct var var = {0};
    var.kind = IMMEDIATE;
    var.type = n.type;
    var.imm = n.val;
    return var;
}

static struct var imm_signed(const struct typetree *type, long val)
{
    struct number num = {0};
    assert(is_signed(type));
    num.type = type;
    num.val.i = val;
    return var_numeric(num);
}

static struct var imm_unsigned(const struct typetree *type, unsigned long val)
{
    struct number num = {0};

    assert(is_unsigned(type));
    num.type = type;
    num.val.u = val;
    if (size_of(type) < 8) {
        assert(size_of(type) == 4
            || size_of(type) == 2
            || size_of(type) == 1);
        num.val.u &= (0xFFFFFFFFu >> ((4 - size_of(type)) * 8));
    }

    return var_numeric(num);
}

static struct var imm_float(float val)
{
    struct number num = {0};
    num.type = &basic_type__float;
    num.val.f = val;
    return var_numeric(num);
}

static struct var imm_double(double val)
{
    struct number num = {0};
    num.type = &basic_type__double;
    num.val.d = val;
    return var_numeric(num);
}

struct expression as_expr(struct var val)
{
    struct expression expr = {0};

    expr.op = IR_OP_CAST;
    expr.type = val.type;
    expr.l = val;
    assert(is_identity(expr));
    return expr;
}

#define eval_arithmetic_immediate(t, l, op, r) (                               \
    is_signed(t) ? imm_signed(t, (l).imm.i op (r).imm.i) :                     \
    is_unsigned(t) ? imm_unsigned(t, (l).imm.u op (r).imm.u) :                 \
    is_float(t) ? imm_float((l).imm.f op (r).imm.f) :                          \
        imm_double((l).imm.d op (r).imm.d))

#define eval_integer_immediate(t, l, op, r) (                                  \
    is_signed(t) ?                                                             \
        imm_signed(t, (l).imm.i op (r).imm.i) :                                \
        imm_unsigned(t, (l).imm.u op (r).imm.u))

#define eval_immediate_compare(t, l, op, r) \
    var_int(                                                                   \
        is_signed(t) ? (l).imm.i op (r).imm.i :                                \
        is_unsigned(t) ? (l).imm.u op (r).imm.u :                              \
        is_float(t) ? (l).imm.f op (r).imm.f : (l).imm.d op (r).imm.d)

/*
 * Add a statement to the list of ir operations in the block. Parameters
 * are given by the statement opcode.
 *
 * Current block can be NULL when parsing an expression that should not
 * be evaluated, for example argument to sizeof.
 */
static void emit_ir(struct block *block, enum sttype st, ...)
{
    va_list args;
    struct statement stmt = {0};

    if (block) {
        va_start(args, st);
        stmt.st = st;
        stmt.t = var_void();
        switch (st) {
        case IR_ASSIGN:
            stmt.t = va_arg(args, struct var);
        case IR_EXPR:
        case IR_PARAM:
        case IR_VA_START:
            stmt.expr = va_arg(args, struct expression);
            break;
        }

        array_push_back(&block->code, stmt);
        va_end(args);
    }
}

/*
 * Construct a struct expression object, setting the correct type and
 * doing basic sanity checking of the input.
 */
static struct expression create_expr(enum optype op, struct var l, ...)
{
    va_list args;
    struct expression expr;

    expr.l = l;
    expr.op = op;
    va_start(args, l);
    switch (op) {
    case IR_OP_CAST:
    case IR_OP_VA_ARG:
        expr.type = va_arg(args, const struct typetree *);
        break;
    case IR_OP_CALL:
        assert(is_pointer(l.type) && is_function(l.type->next));
        expr.type = l.type->next->next;
        break;
    case IR_OP_NOT:
        assert(is_integer(l.type));
        expr.type = l.type;
        break;
    case IR_OP_ADD:
    case IR_OP_SUB:
    case IR_OP_MUL:
    case IR_OP_DIV:
    case IR_OP_MOD:
        expr.r = va_arg(args, struct var);
        expr.type = l.type;
        break;
    case IR_OP_AND:
    case IR_OP_OR:
    case IR_OP_XOR:
    case IR_OP_SHL:
    case IR_OP_SHR:
        expr.r = va_arg(args, struct var);
        expr.type = l.type;
        assert(is_integer(l.type));
        break;
    case IR_OP_EQ:
    case IR_OP_NE:
    case IR_OP_GE:
    case IR_OP_GT:
        expr.r = va_arg(args, struct var);
        expr.type = &basic_type__int;
        break;
    }

    va_end(args);
    return expr;
}

struct var eval(
    struct definition *def,
    struct block *block,
    struct expression expr)
{
    struct var res;

    if (is_identity(expr)) {
        res = expr.l;
    } else if (is_void(expr.type)) {
        emit_ir(block, IR_EXPR, expr);
        res = var_void();
    } else {
        res = create_var(def, expr.type);
        emit_ir(block, IR_ASSIGN, res, expr);
        res.lvalue = 0;
    }

    return res;
}

static struct var eval_cast(
    struct definition *def,
    struct block *block,
    struct var var,
    const struct typetree *type)
{
    return eval(def, block,
        eval_expr(def, block, IR_OP_CAST, var, type));
}

static struct expression mul(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_arithmetic_immediate(type, l, *, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_MUL, l, r);
    }

    return expr;
}

static struct expression ediv(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_arithmetic_immediate(type, l, /, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_DIV, l, r);
    }

    return expr;
}

static struct expression mod(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    type = usual_arithmetic_conversion(l.type, r.type);
    if (!is_integer(type)) {
        error("Operands of modulo operator must be of integer type.");
        exit(1);
    }

    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, %, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_MOD, l, r);
    }

    return expr;
}

static struct expression add(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        type = usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(def, block, l, type);
        r = eval_cast(def, block, r, type);
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = eval_arithmetic_immediate(type, l, +, r);
            expr = as_expr(l);
        } else {
            expr = create_expr(IR_OP_ADD, l, r);
        }
    } else if (is_integer(l.type) && is_pointer(r.type)) {
        /* Make sure pointer is left, and integer right. */
        expr = add(def, block, r, l);
    } else if (is_pointer(l.type) && is_integer(r.type)) {
        if (!size_of(l.type->next)) {
            error("Pointer arithmetic on incomplete type.");
            exit(1);
        }
        /*
         * Special case for immediate string literal + constant, and
         * ADDRESS + constant. These can be evaluated immediately. If r
         * is immediate zero, no evaluation is necessary.
         */
        if ((is_string(l) || l.kind == ADDRESS)
            && r.kind == IMMEDIATE
            && is_integer(r.type))
        {
            assert(!is_tagged(l.type));
            l.offset += r.imm.i * size_of(l.type->next);
            expr = as_expr(l);
        } else if (is_constant(l) && r.kind == IMMEDIATE) {
            l.imm.i += r.imm.i;
            expr = as_expr(l);
        } else if (r.kind != IMMEDIATE || r.imm.i) {
            r = eval(def, block,
                    eval_expr(def, block, IR_OP_MUL,
                        var_int(size_of(l.type->next)), r));
            expr = create_expr(IR_OP_ADD, l, r);
        } else {
            expr = as_expr(l);
        }
    } else {
        error("Incompatible arguments to addition operator, was '%t' and '%t'.",
            l.type, r.type);
        exit(1);
    }

    return expr;
}

static struct expression sub(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    int size;
    struct expression expr;
    const struct typetree *type;

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        type = usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(def, block, l, type);
        r = eval_cast(def, block, r, type);
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = eval_arithmetic_immediate(type, l, -, r);
            expr = as_expr(l);
        } else {
            expr = create_expr(IR_OP_SUB, l, r);
        }
    } else if (is_pointer(l.type) && is_integer(r.type)) {
        if (!size_of(l.type->next)) {
            error("Pointer arithmetic on incomplete type.");
            exit(1);
        }
        /*
         * Special case for immediate string literal - constant, and
         * ADDRESS - constant. These can be evaluated immediately. If r
         * is immediate zero, no evaluation is necessary.
         */
        if ((is_string(l) || l.kind == ADDRESS)
            && r.kind == IMMEDIATE
            && is_integer(r.type))
        {
            assert(!is_tagged(l.type));
            l.offset -= r.imm.i * size_of(l.type->next);
            expr = as_expr(l);
        } else if (is_constant(l) && r.kind == IMMEDIATE) {
            l.imm.i -= r.imm.i;
            expr = as_expr(l);
        } else if (!is_immediate_false(as_expr(r))) {
            r = eval(def, block,
                    eval_expr(def, block, IR_OP_MUL,
                        var_int(size_of(l.type->next)), r));
            expr = create_expr(IR_OP_SUB, l, r);
        } else {
            expr = as_expr(l);
        }
    } else if (is_pointer(l.type) && is_pointer(r.type)) {
        size = size_of(r.type->next);
        if (!size_of(l.type->next) || size_of(l.type->next) != size) {
            error("Referenced type is incomplete.");
            exit(1);
        }
        /*
         * Result is ptrdiff_t, which will be signed 64 bit integer.
         * Reinterpret cast both pointers to unsigned long (no codegen),
         * and store result as signed long. Then divide by element size
         * to get diff.
         */
        l = eval_cast(def, block, l, &basic_type__unsigned_long);
        r = eval_cast(def, block, r, &basic_type__unsigned_long);
        l = eval(def, block, eval_expr(def, block, IR_OP_SUB, l, r));
        expr = eval_expr(def, block, IR_OP_DIV, l, var_int(size));
    } else {
        error("Incompatible arguments to subtraction operator, was %t and %t.",
            l.type, r.type);
        exit(1);
    }

    return expr;
}

static void prepare_comparison_operands(
    struct definition *def,
    struct block *block,
    struct var *l,
    struct var *r)
{
    const struct typetree *type;

    /* Normalize by putting most specific pointer as left argument. */
    if (is_pointer(r->type)
        && (!is_pointer(l->type)
            || (is_void(l->type->next) && !is_void(r->type->next))))
    {
        prepare_comparison_operands(def, block, r, l);
        return;
    }

    if (is_arithmetic(l->type) && is_arithmetic(r->type)) {
        type = usual_arithmetic_conversion(l->type, r->type);
        *l = eval_cast(def, block, *l, type);
        *r = eval_cast(def, block, *r, type);
    } else if (is_pointer(l->type)) {
        if (is_pointer(r->type)) {
            if (!is_compatible(l->type, r->type) &&
                !(is_void(l->type->next) && size_of(r->type->next)) &&
                !(is_void(r->type->next) && size_of(l->type->next)))
            {
                warning("Comparison between incompatible types '%t' and '%t'.",
                    l->type, r->type);
            }
        } else if (!is_nullptr(*r)) {
            warning("Comparison between pointer and non-zero integer.");
        }

        /* Left operand has the most specific type. */
        *r = eval_cast(def, block, *r, l->type);
    } else {
        error("Illegal comparison between types '%t' and '%t'.",
            l->type, r->type);
        exit(1);
    }

    assert(type_equal(l->type, r->type));
}

static struct expression cmp_eq(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;

    prepare_comparison_operands(def, block, &l, &r);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_immediate_compare(l.type, l, ==, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_EQ, l, r);
    }

    return expr;
}

static struct expression cmp_ne(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;

    prepare_comparison_operands(def, block, &l, &r);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_immediate_compare(l.type, l, !=, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_NE, l, r);
    }

    return expr;
}

static const struct typetree *common_compare_type(
    const struct typetree *left,
    const struct typetree *right)
{
    const struct typetree *type = NULL;

    if (is_arithmetic(left) && is_arithmetic(right)) {
        type = usual_arithmetic_conversion(left, right);
    } else if (is_pointer(left)
        && is_pointer(right)
        && size_of(left->next)
        && size_of(left->next) == size_of(right->next))
    {
        type = left;
    } else {
        error("Invalid operands in relational expression.");
        exit(1);
    }

    return type;
}

static struct expression cmp_ge(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    type = common_compare_type(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_immediate_compare(type, l, >=, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_GE, l, r);
    }

    return expr;
}

static struct expression cmp_gt(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    type = common_compare_type(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_immediate_compare(type, l, >, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_GT, l, r);
    }

    return expr;
}

static struct expression or(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise or must have integer type.");
        exit(1);
    }

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, |, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_OR, l, r);
    }

    return expr;
}

static struct expression xor(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise xor must have integer type.");
        exit(1);
    }

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, ^, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_XOR, l, r);
    }

    return expr;
}

static struct expression and(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise and must have integer type.");
        exit(1);
    }

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, &, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_AND, l, r);
    }

    return expr;
}

static struct expression shiftl(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Shift operands must have integer type.");
        exit(1);
    }

    type = promote_integer(l.type);
    l = eval_cast(def, block, l, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, <<, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_SHL, l, r);
    }

    return expr;
}

static struct expression shiftr(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    const struct typetree *type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Shift operands must have integer type.");
        exit(1);
    }

    type = promote_integer(l.type);
    l = eval_cast(def, block, l, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, >>, r);
        expr = as_expr(l);
    } else {
        expr = create_expr(IR_OP_SHR, l, r);
    }

    return expr;
}

static struct expression not(
    struct definition *def,
    struct block *block,
    struct var var)
{
    struct expression expr;
    const struct typetree *type;

    if (!is_integer(var.type)) {
        error("Bitwise complement operand must have integer type.");
        exit(1);
    }

    type = promote_integer(var.type);
    var = eval_cast(def, block, var, type);
    if (var.kind == IMMEDIATE) {
        if (is_signed(type)) {
            var = imm_signed(type, ~var.imm.i);
        } else {
            assert(is_unsigned(type));
            var = imm_unsigned(type, ~var.imm.u);
        }
        expr = as_expr(var);
    } else {
        expr = create_expr(IR_OP_NOT, var);
    }

    return expr;
}

#define cast_immediate(v, T) ( \
    is_float((v).type) ? (T) (v).imm.f : \
    is_double((v).type) ? (T) (v).imm.d : \
    is_signed((v).type) ? (T) (v).imm.i : (T) (v).imm.u)

#define is_float_above(v, n) \
    ((is_float((v).type) && (v).imm.f > n) \
            || (is_double((v).type) && (v).imm.d > n))

#define is_float_below(v, n) \
    ((is_float((v).type) && (v).imm.f < n) \
        || (is_double((v).type) && (v).imm.d < n))

/*
 * All immediate conversions must be evaluated compile time. Also handle
 * conversion which can be done by reinterpreting memory.
 */
static struct expression cast(
    struct definition *def,
    struct block *block,
    struct var var,
    const struct typetree *type)
{
    struct expression expr;

    if (is_void(type)) {
        return as_expr(var_void());
    } else if (!is_scalar(var.type) || !is_scalar(type)) {
        error(
            "Invalid type parameters to cast expression, "
            "cannot convert from %t to %t.",
            var.type, type);
        exit(1);
    }

    if (var.kind == IMMEDIATE) {
        if (is_float(type)) {
            var.imm.f =
                (is_double(var.type)) ? (float) var.imm.d :
                (is_signed(var.type)) ? (float) var.imm.i :
                (is_unsigned(var.type)) ? (float) var.imm.u : var.imm.f;
        } else if (is_double(type)) {
            var.imm.d =
                (is_float(var.type)) ? (double) var.imm.f :
                (is_signed(var.type)) ? (double) var.imm.i :
                (is_unsigned(var.type)) ? (double) var.imm.u : var.imm.d;
        } else if (is_unsigned(type) || is_pointer(type)) {
            if (is_float_below(var, 0)) {
                var.imm.u = 0;
            } else if (size_of(type) == 1) {
                var.imm.u = is_float_above(var, UCHAR_MAX) ? UCHAR_MAX
                    : cast_immediate(var, unsigned char);
            } else if (size_of(type) == 2) {
                var.imm.u = is_float_above(var, USHRT_MAX) ? USHRT_MAX
                    : cast_immediate(var, unsigned short);
            } else if (size_of(type) == 4) {
                var.imm.u = is_float_above(var, UINT_MAX) ? UINT_MAX
                    : cast_immediate(var, unsigned int);
            } else {
                var.imm.u = is_float_above(var, ULONG_MAX) ? ULONG_MAX
                    : cast_immediate(var, unsigned long);
            }
        } else {
            assert(is_signed(type));
            if (size_of(type) == 1) {
                var.imm.i = is_float_below(var, CHAR_MIN) ? CHAR_MIN
                    : is_float_above(var, CHAR_MAX) ? CHAR_MAX
                    : cast_immediate(var, signed char);
            } else if (size_of(type) == 2) {
                var.imm.i = is_float_below(var, SHRT_MIN) ? SHRT_MIN
                    : is_float_above(var, SHRT_MAX) ? SHRT_MAX
                    : cast_immediate(var, signed short);
            } else if (size_of(type) == 4) {
                var.imm.i = is_float_below(var, INT_MIN) ? INT_MIN
                    : is_float_above(var, INT_MAX) ? INT_MAX
                    : cast_immediate(var, signed int);
            } else {
                var.imm.i = is_float_below(var, LONG_MIN) ? LONG_MIN
                    : is_float_above(var, LONG_MAX) ? LONG_MAX
                    : cast_immediate(var, signed long);
            }
        }
        var.type = type;
        expr = as_expr(var);
    } else if (size_of(var.type) == size_of(type)
        && (is_pointer(var.type) || is_pointer(type)))
    {
        var.type = type;
        expr = as_expr(var);
    } else if (!type_equal(var.type, type)) {
        expr = create_expr(IR_OP_CAST, var, type);
    } else {
        assert(type_equal(var.type, type));
        expr = as_expr(var);
    }

    assert(type_equal(expr.type, type));
    return expr;
}

static struct expression call(
    struct definition *def,
    struct block *block,
    struct var var)
{
    if (!is_pointer(var.type) || !is_function(var.type->next)) {
        error("Calling non-function type %t.", var.type);
        exit(1);
    }

    return create_expr(IR_OP_CALL, var);
}

static struct expression eval_va_arg(
    struct definition *def,
    struct block *block,
    struct var var,
    const struct typetree *type)
{
    assert(is_pointer(var.type));
    return create_expr(IR_OP_VA_ARG, var, type);
}

/*
 * Convert variables of type ARRAY or FUNCTION to addresses when used
 * in expressions, and normalize bit-field values to whole integers.
 * 'array of T' is converted (decay) to pointer to T. Not the same as
 * taking the address of an array, which would give 'pointer to array
 * of T'.
 */
static struct var rvalue(
    struct definition *def,
    struct block *block,
    struct var var)
{
    int bits;

    if (is_function(var.type)) {
        var = eval_addr(def, block, var);
    } else if (is_array(var.type)) {
        if (var.kind == IMMEDIATE) {
            assert(var.symbol);
            assert(var.symbol->symtype == SYM_STRING_VALUE);
            /*
             * Immediate references to strings retain the same
             * representation, only changing type from array to
             * pointer.
             */
            var.type = type_init(T_POINTER, var.type->next);
        } else {
            /*
             * References to arrays decay into pointer to the first
             * element. Change type before doing regular address
             * evaluation, this way backend does not need to handle
             * address of array in any special way. The memory location
             * represented by var can also be seen as the first element.
             */
            var.type = var.type->next;
            var = eval_addr(def, block, var);
        }
    } else if (is_field(var)) {
        assert(
            type_equal(var.type, &basic_type__int) ||
            type_equal(var.type, &basic_type__unsigned_int));
        /*
         * Bit field is loaded, and if needed sign extended, into a full
         * width integer value. Set width = 0 to make nested calls to
         * eval methods not recursively call rvalue.
         */
        if (var.width < size_of(&basic_type__int) * 8) {
            bits = var.width;
            var.width = 0;
            var = eval(def, block,
                and(def, block, var, var_int((1 << bits) - 1)));
            if (is_signed(var.type)) {
                bits = size_of(var.type) * 8 - bits;
                var = eval(def, block, shiftl(def, block, var, var_int(bits)));
                var = eval(def, block, shiftr(def, block, var, var_int(bits)));
            } else {
                /*
                 * Fields of unsigned type are promoted to signed int
                 * if the signed type can represent all values of the
                 * unsigned type.
                 */
                var.type = &basic_type__int;
            }
        } else {
            assert(var.width == size_of(&basic_type__int) * 8);
        }
    }

    assert(!var.width);
    return var;
}

struct expression eval_expr(
    struct definition *def,
    struct block *block,
    enum optype optype,
    struct var l, ...)
{
    va_list args;
    struct var r;
    const struct typetree *type = NULL;

    l = rvalue(def, block, l);
    va_start(args, l);
    if (optype == IR_OP_CAST || optype == IR_OP_VA_ARG) {
        type = va_arg(args, const struct typetree *);
    } else if (optype != IR_OP_NOT && optype != IR_OP_CALL) {
        r = va_arg(args, struct var);
        r = rvalue(def, block, r);
    }
    va_end(args);

    switch (optype) {
    default: assert(0);
    case IR_OP_CAST: return cast(def, block, l, type);
    case IR_OP_CALL: return call(def, block, l);
    case IR_OP_VA_ARG: return eval_va_arg(def, block, l, type);
    case IR_OP_NOT:  return not(def, block, l);
    case IR_OP_MOD:  return mod(def, block, l, r);
    case IR_OP_MUL:  return mul(def, block, l, r);
    case IR_OP_DIV:  return ediv(def, block, l, r);
    case IR_OP_ADD:  return add(def, block, l, r);
    case IR_OP_SUB:  return sub(def, block, l, r);
    case IR_OP_EQ:   return cmp_eq(def, block, l, r);
    case IR_OP_NE:   return cmp_ne(def, block, l, r);
    case IR_OP_GE:   return cmp_ge(def, block, l, r);
    case IR_OP_GT:   return cmp_gt(def, block, l, r);
    case IR_OP_AND:  return and(def, block, l, r);
    case IR_OP_XOR:  return xor(def, block, l, r);
    case IR_OP_OR:   return or(def, block, l, r);
    case IR_OP_SHL:  return shiftl(def, block, l, r);
    case IR_OP_SHR:  return shiftr(def, block, l, r);
    }
}

struct expression eval_unary_minus(
    struct definition *def,
    struct block *block,
    struct var val)
{
    struct expression expr;

    if (!is_arithmetic(val.type)) {
        error("Unary (-) operand must be of arithmetic type.");
        exit(1);
    }

    if (is_float(val.type)) {
        val.type = &basic_type__unsigned_int;
        expr = xor(def, block, imm_unsigned(val.type, 1u << 31), val);
        val = eval(def, block, expr);
        val.type = &basic_type__float;
        expr = as_expr(val);
    } else if (is_double(val.type)) {
        val.type = &basic_type__unsigned_long;
        expr = xor(def, block, imm_unsigned(val.type, 1ul << 63), val);
        val = eval(def, block, expr);
        val.type = &basic_type__double;
        expr = as_expr(val);
    } else {
        expr = sub(def, block, var_int(0), val);
    }

    return expr;
}

struct var eval_addr(
    struct definition *def,
    struct block *block,
    struct var var)
{
    struct var tmp, ptr;

    if (is_field(var)) {
        error("Cannot take address of bit-field.");
        exit(1);
    }

    switch (var.kind) {
    case IMMEDIATE:
        if (!var.symbol || var.symbol->symtype != SYM_STRING_VALUE) {
            error("Cannot take address of immediate of type '%t'.", var.type);
            exit(1);
        }
        /*
         * Address of string literal can be done without evaluation,
         * just decay the variable to pointer.
         */
        if (is_array(var.type)) {
            var = rvalue(def, block, var);
        }
        break;
    case DIRECT:
        /*
         * Address of *(&sym + offset) is available directly by setting
         * flag, resulting in (&sym + offset). No special handling of
         * offset needed.
         */
        if (var.lvalue) {
            var.kind = ADDRESS;
            var.type = type_init(T_POINTER, var.type);
            break;
        }
    case ADDRESS:
        assert(!var.lvalue);
        error("Cannot take address of r-value.");
        exit(1);
        break;
    case DEREF:
        if (!var.symbol) {
            /*
             * Address of *(const + offset) is just the constant.
             * Convert to immediate, adding the extra offset.
             */
            var.kind = IMMEDIATE;
            var.type = type_init(T_POINTER, var.type);
            var.imm.u += var.offset;
            var.offset = 0;
            var.lvalue = 0;
        } else {
            /*
             * Address of *(sym + offset) is not *(&sym + offset), so
             * not possible to just convert to DIRECT. Offset must be
             * applied after converting to direct pointer.
             */
            assert(is_pointer(&var.symbol->type));
            tmp = var_direct(var.symbol);
            if (var.offset) {
                ptr = eval_cast(def, block, tmp,
                    type_init(T_POINTER, &basic_type__char));
                tmp = eval(def, block,
                    eval_expr(def, block, IR_OP_ADD, ptr, var_int(var.offset)));
            }
            tmp.type = type_init(T_POINTER, var.type);
            var = tmp;
        }
        break;
    }

    assert(is_pointer(var.type));
    return var;
}

struct var eval_deref(
    struct definition *def,
    struct block *block,
    struct var var)
{
    var = rvalue(def, block, var);
    if (!is_pointer(var.type)) {
        error("Dereferencing non-pointer type '%t'.", var.type);
        exit(1);
    }

    assert(!is_tagged(var.type));
    switch (var.kind) {
    case DEREF:
        /*
         * Dereferencing *(sym + offset) must evaluate pointer into a
         * new temporary, before marking that as DEREF var.
         */
        var = eval_assign(def, block, create_var(def, var.type), as_expr(var));
        break;
    case DIRECT:
        if (var.offset != 0 || !is_pointer(&var.symbol->type)) {
            /*
             * Cannot immediately dereference a pointer which is at a
             * direct offset from another symbol. Also, pointers that
             * are the result of indexing into a structure must be
             * evaluated, as DEREF variables assume symbol to be of
             * pointer type.
             */
            var = eval_assign(def, block,
                create_var(def, var.type), as_expr(var));
        }
        break;
    case ADDRESS:
        /*
         * Dereferencing (&sym + offset) is a DIRECT reference to sym,
         * with the same offset.
         */
        var.kind = DIRECT;
        var.type = type_deref(var.type);
        var.lvalue = 1;
        return var;
    case IMMEDIATE:
        /*
         * Dereferencing constant which has been cast to pointer. This
         * is a special case of deref, identified by symbol being NULL.
         * Handled in backend.
         */
        var.kind = DEREF;
        var.type = type_deref(var.type);
        var.lvalue = 1;
        assert(!var.symbol);
        return var;
    }

    assert(var.kind == DIRECT);
    assert(!var.offset);
    assert(is_pointer(var.type));
    var.kind = DEREF;
    var.type = type_deref(var.type);
    var.lvalue = 1;
    return var;
}

static struct var mask_bitfield_immediate(struct var v, int w)
{
    assert(v.kind == IMMEDIATE);
    assert(is_integer(v.type));
    assert(w > 0);
    assert(w < size_of(v.type) * 8);

    v.imm.u = (v.imm.u & 0xFFFFFFFFFFFFFFFFul >> (64 - w));
    if (is_signed(v.type) && (v.imm.u & (1 << (w - 1)))) {
        v.imm.u |= (0xFFFFFFFFFFFFFFFFul << w);
    }

    return v;
}

/*
 * Special case char [] = string in initializers. In this case we do
 * nothing here, but handle it in backend.
 */
struct var eval_assign(
    struct definition *def,
    struct block *block,
    struct var target,
    struct expression expr)
{
    struct var var;
    enum qualifier cv;

    if (!target.lvalue) {
        error("Target of assignment must be l-value.");
        exit(1);
    }

    if (is_identity(expr) && !is_array(target.type)) {
        var = eval(def, block, expr);
        var = rvalue(def, block, var);
        expr = as_expr(var);
    }

    if (is_array(target.type)) {
        if (!type_equal(target.type, expr.type)
            || !is_identity(expr)
            || expr.l.kind != IMMEDIATE)
        {
            error("Invalid initializer assignment, was %s :: %t = %t.",
                str_raw(target.symbol->name),
                target.type,
                expr.type);
            exit(1);
        }
        assert(expr.l.symbol);
        assert(expr.l.symbol->symtype == SYM_STRING_VALUE);
    } else if (is_pointer(target.type) && is_pointer(expr.type)) {
        cv = target.type->next->qualifier | expr.type->next->qualifier;
        if (!is_compatible(target.type, expr.type)) {
            if (!((is_identity(expr) && is_nullptr(expr.l))
                || (is_void(target.type->next) && is_object(expr.type->next))
                || (is_object(target.type->next) && is_void(expr.type->next))))
            {
                warning("Incompatible type in pointer assignment; %t = %t.",
                    target.type,
                    expr.type);
            }
        }
        if (!(is_identity(expr) && is_nullptr(expr.l))
            && cv != target.type->next->qualifier)
        {
            warning("Target of assignment lacks qualifiers; %t = %t.",
                target.type,
                expr.type);
        }
    } else if (is_pointer(target.type) && is_integer(expr.type)) {
        if (!(is_identity(expr) && is_nullptr(expr.l))) {
            warning("Assigning non-zero number to pointer.",
                target.type,
                expr.type);
        }
    } else if (is_arithmetic(target.type) && is_arithmetic(expr.type)) {
        if (is_identity(expr) && expr.l.kind == IMMEDIATE) {
            var = eval_cast(def, block, expr.l, target.type);
            if (is_field(target)) {
                var = mask_bitfield_immediate(var, target.width);
            }
            expr = as_expr(var);
        }
    } else if (
        !(is_struct_or_union(target.type)
            && is_compatible(target.type, expr.type)))
    {
        error("Incompatible operands to assignment expression, %s :: %t = %t.",
            str_raw(target.symbol->name), target.type, expr.type);
        exit(1);
    }

    if (!type_equal(target.type, expr.type)) {
        var = eval(def, block, expr);
        expr = eval_expr(def, block, IR_OP_CAST, var, target.type);
    }

    emit_ir(block, IR_ASSIGN, target, expr);
    if (is_identity(expr) && expr.l.kind == IMMEDIATE) {
        target = expr.l;
    } else {
        target.lvalue = 0;
    }

    return target;
}

struct var eval_copy(
    struct definition *def,
    struct block *block,
    struct var var)
{
    struct var cpy;

    assert(!is_function(var.type));
    assert(!is_array(var.type));

    var = rvalue(def, block, var);
    cpy = create_var(def, var.type);
    return eval_assign(def, block, cpy, as_expr(var));
}

struct expression eval_return(
    struct definition *def,
    struct block *block)
{
    struct var val;
    const struct typetree *type;

    type = def->symbol->type.next;
    assert(is_object(type));
    assert(!is_array(type));

    if (is_identity(block->expr)) {
        val = rvalue(def, block, block->expr.l);
        block->expr = as_expr(val);
    }

    if (!type_equal(type, block->expr.type)) {
        val = eval(def, block, block->expr);
        block->expr = eval_expr(def, block, IR_OP_CAST, val, type);
    } else if (!is_scalar(type) || block->expr.op == IR_OP_VA_ARG) {
        val = eval(def, block, block->expr);
        block->expr = as_expr(val);
    }

    block->has_return_value = 1;
    return block->expr;
}

struct expression eval_conditional(
    struct definition *def,
    struct expression a,
    struct block *b,
    struct block *c)
{
    struct var res, bval, cval;
    const struct typetree *t1, *t2, *type;

    if (!is_scalar(a.type)) {
        error("Conditional must be of scalar type.");
        exit(1);
    }

    bval = rvalue(def, b, eval(def, b, b->expr));
    cval = rvalue(def, c, eval(def, c, c->expr));

    t1 = bval.type;
    t2 = cval.type;

    if (is_arithmetic(t1) && is_arithmetic(t2)) {
        type = usual_arithmetic_conversion(t1, t2);
    } else if (
        (is_void(t1) && is_void(t2)) ||
        (is_struct_or_union(t1) && type_equal(t1, t2)) ||
        (is_pointer(t1) && is_pointer(t2) && is_compatible(t1, t2)) ||
        (is_pointer(t1) && is_nullptr(cval)))
    {
        type = t1;
    } else if (is_pointer(t2) && is_nullptr(bval)) {
        type = t2;
    } else {
        /* The rules are more complex than this, revisit later. */
        error("Unsupported types in conditional operator.");
        exit(1);
    }

    if (is_void(type)) {
        res = var_void();
    } else {
        if (is_immediate(a)) {
            res = (a.l.imm.i) ? bval : cval;
        } else {
            res = create_var(def, type);
            b->expr = as_expr(eval_assign(def, b, res, as_expr(bval)));
            c->expr = as_expr(eval_assign(def, c, res, as_expr(cval)));
            res.lvalue = 0;
        }
    }

    return as_expr(res);
}

/*
 * Result is integer type, assigned in true and false branches to
 * numeric constant 1 or 0.
 */
static struct block *eval_logical_expression(
    struct definition *def,
    int is_and,
    struct block *left,
    struct block *right_top,
    struct block *right)
{
    struct var res;
    struct block
        *t = cfg_block_init(def),
        *f = cfg_block_init(def),
        *r = cfg_block_init(def);

    res = create_var(def, &basic_type__int);
    left->expr =
        eval_expr(def, left, IR_OP_EQ,
            eval(def, left, left->expr), var_int(0));
    if (is_and) {
        left->jump[0] = right_top;
        left->jump[1] = f;
    } else {
        left->jump[0] = t;
        left->jump[1] = right_top;
    }

    right->expr =
        eval_expr(def, right, IR_OP_EQ,
            eval(def, right, right->expr), var_int(0));
    if (is_immediate_true(right->expr)) {
        right->jump[0] = f;
    } else if (is_immediate_false(right->expr)) {
        right->jump[0] = t;
    } else {
        right->jump[0] = t;
        right->jump[1] = f;
    }

    res.lvalue = 1;
    eval_assign(def, t, res, as_expr(var_int(1)));
    eval_assign(def, f, res, as_expr(var_int(0)));
    res.lvalue = 0;

    t->jump[0] = r;
    f->jump[0] = r;
    r->expr = as_expr(res);
    return r;
}

static int is_logical_immediate(
    struct block *left,
    struct block *top,
    struct block *bottom)
{
    return top == bottom
        && !array_len(&top->code)
        && is_identity(top->expr) && top->expr.l.kind == IMMEDIATE
        && is_identity(left->expr) && left->expr.l.kind == IMMEDIATE;
}

struct block *eval_logical_or(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right)
{
    int res;
    struct var value;

    if (!is_scalar(left->expr.type) || !is_scalar(right->expr.type)) {
        error("Operands to logical or must be of scalar type.");
        exit(1);
    }

    if (is_logical_immediate(left, right_top, right)) {
        res = is_immediate_true(left->expr) || is_immediate_true(right->expr);
        left->expr = as_expr(var_int(res));
    } else if (is_immediate_true(left->expr)) {
        left->expr = as_expr(var_int(1));
    } else if (is_immediate_false(left->expr)) {
        left->jump[0] = right_top;
        left = right;
        if (!is_comparison(left->expr)) {
            value = eval(def, left, left->expr);
            left->expr = eval_expr(def, left, IR_OP_NE, value, var_int(0));
        }
    } else {
        left = eval_logical_expression(def, 0, left, right_top, right);
    }

    return left;
}

struct block *eval_logical_and(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right)
{
    int res;
    struct var value;

    if (!is_scalar(left->expr.type) || !is_scalar(right->expr.type)) {
        error("Operands to logical and must be of scalar type.");
        exit(1);
    }

    if (is_logical_immediate(left, right_top, right)) {
        res = is_immediate_true(left->expr) && is_immediate_true(right->expr);
        left->expr = as_expr(var_int(res));
    } else if (is_immediate_false(left->expr)) {
        left->expr = as_expr(var_int(0));
    } else if (is_immediate_true(left->expr)) {
        left->jump[0] = right_top;
        left = right;
        if (!is_comparison(left->expr)) {
            value = eval(def, left, left->expr);
            left->expr = eval_expr(def, left, IR_OP_NE, value, var_int(0));
        }
    } else {
        left = eval_logical_expression(def, 1, left, right_top, right);
    }

    return left;
}

struct expression eval_param(
    struct definition *def,
    struct block *block,
    struct expression expr)
{
    struct var var;

    var = eval(def, block, expr);
    var = rvalue(def, block, var);
    expr = as_expr(var);
    if (!is_identity(expr)) {
        var = create_var(def, expr.type);
        var = eval_assign(def, block, var, expr);
        expr = as_expr(var);
    }

    return expr;
}

void param(
    struct definition *def,
    struct block *block,
    struct expression expr)
{
    emit_ir(block, IR_PARAM, expr);
}

void eval__builtin_va_start(
    struct block *block,
    struct expression arg)
{
    emit_ir(block, IR_VA_START, arg);
}
