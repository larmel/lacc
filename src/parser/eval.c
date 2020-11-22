#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "eval.h"
#include "declaration.h"
#include "parse.h"
#include "symtab.h"
#include "typetree.h"
#include <lacc/context.h>
#include <lacc/ir.h>

#include <assert.h>
#include <limits.h>
#include <stdarg.h>
#include <stdlib.h>

static int immediate_bool_value(union value value, Type type)
{
    assert(is_scalar(type));
    switch (type_of(type)) {
    case T_FLOAT:
        return value.f != 0.0f;
    case T_DOUBLE:
        return value.d != 0.0;
    case T_LDOUBLE:
        return value.ld != 0.0L;
    default:
        return value.u != 0l;
    }
}

static int extract_literal_char(struct var v)
{
    const char *raw;
    String str;

    assert(v.kind == DIRECT);
    assert(v.symbol->symtype == SYM_LITERAL);

    str = v.symbol->value.string;
    raw = str_raw(str);
    if (v.offset >= str_len(str)) {
        error("Access outside bounds of string literal.");
        exit(1);
    }

    return raw[v.offset];
}

INTERNAL int immediate_bool(struct expression expr)
{
    assert(is_scalar(expr.type));
    if (!is_identity(expr))
        return -1;

    switch (expr.l.kind) {
    case IMMEDIATE:
        return immediate_bool_value(expr.l.imm, expr.type);
    case DIRECT:
        if (expr.l.symbol->symtype == SYM_LITERAL)
            return extract_literal_char(expr.l) != 0;
        break;
    case ADDRESS:
        if (expr.l.symbol->symtype == SYM_LITERAL
            || is_function(expr.l.symbol->type))
            return 1;
    default:
        break;
    }

    return -1;
}

static int is_nullptr(struct var val)
{
    return (is_integer(val.type) || is_pointer(val.type))
        && immediate_bool(as_expr(val)) == 0;
}

INTERNAL struct var var_void(void)
{
    struct var var = {0};

    var.kind = IMMEDIATE;
    var.type = basic_type__void;
    return var;
}

INTERNAL struct var create_var(struct definition *def, Type type)
{
    struct symbol *tmp;
    struct var res;
    assert(def);
    assert(!is_void(type));

    tmp = sym_create_temporary(type);
    res = var_direct(tmp);
    array_push_back(&def->locals, tmp);
    res.lvalue = 1;
    return res;
}

INTERNAL struct var var_direct(const struct symbol *sym)
{
    struct var var = {0};

    assert(sym);
    var.type = sym->type;
    var.symbol = sym;

    switch (sym->symtype) {
    case SYM_CONSTANT:
        var.kind = IMMEDIATE;
        var.imm = sym->value.constant;
        break;
    default:
        assert(sym->symtype != SYM_LABEL);
        var.kind = DIRECT;
        var.lvalue = !is_temporary(sym);
        break;
    }

    return var;
}

INTERNAL struct var var_int(int value)
{
    struct var var = {0};
    var.kind = IMMEDIATE;
    var.type = basic_type__int;
    var.imm.i = value;
    return var;
}

INTERNAL struct var var_numeric(Type type, union value val)
{
    struct var var = {0};
    var.kind = IMMEDIATE;
    var.type = type;
    var.imm = val;
    return var;
}

static struct var imm_signed(Type type, long n)
{
    union value val = {0};
    assert(is_signed(type));
    val.i = n;
    return var_numeric(type, val);
}

INTERNAL struct var imm_unsigned(Type type, unsigned long n)
{
    union value val = {0};

    assert(is_unsigned(type));
    val.u = n;
    if (size_of(type) < 8) {
        assert(size_of(type) == 4
            || size_of(type) == 2
            || size_of(type) == 1);
        val.u &= (0xFFFFFFFFu >> ((4 - size_of(type)) * 8));
    }

    return var_numeric(type, val);
}

static struct var imm_float(float n)
{
    union value val = {0};
    val.f = n;
    return var_numeric(basic_type__float, val);
}

static struct var imm_double(double n)
{
    union value val = {0};
    val.d = n;
    return var_numeric(basic_type__double, val);
}

static struct var imm_long_double(long double n)
{
    union value val = {0};
    val.ld = n;
    return var_numeric(basic_type__long_double, val);
}

INTERNAL struct expression as_expr(struct var val)
{
    struct expression expr = {0};

    expr.op = IR_OP_CAST;
    expr.type = val.type;
    expr.l = val;
    assert(is_identity(expr));
    return expr;
}

#define eval_arithmetic_immediate(t, l, op, r) ( \
    is_signed(t)   ? imm_signed(t, (l).imm.i op (r).imm.i) : \
    is_unsigned(t) || is_pointer(t) \
                   ? imm_unsigned(t, (l).imm.u op (r).imm.u) : \
    is_float(t)    ? imm_float((l).imm.f op (r).imm.f) : \
    is_double(t)   ? imm_double((l).imm.d op (r).imm.d) \
                   : imm_long_double((l).imm.ld op (r).imm.ld))

#define eval_integer_immediate(t, l, op, r) ( \
    is_signed(t)   ? imm_signed(t, (l).imm.i op (r).imm.i) \
                   : imm_unsigned(t, (l).imm.u op (r).imm.u))

#define eval_immediate_compare(t, l, op, r) var_int( \
    is_signed(t)   ? (l).imm.i op (r).imm.i : \
    is_unsigned(t) || is_pointer(t) \
                   ? (l).imm.u op (r).imm.u : \
    is_float(t)    ? (l).imm.f op (r).imm.f : \
    is_double(t)   ? (l).imm.d op (r).imm.d \
                   : (l).imm.ld op (r).imm.ld)

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
    default: assert(0);
    case IR_OP_CAST:
    case IR_OP_VA_ARG:
        expr.type = va_arg(args, Type );
        break;
    case IR_OP_CALL:
        assert(is_pointer(l.type) && is_function(type_next(l.type)));
        expr.type = type_next(type_next(l.type));
        break;
    case IR_OP_NOT:
        assert(is_integer(l.type));
        expr.type = l.type;
        break;
    case IR_OP_NEG:
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
        expr.type = basic_type__int;
        break;
    }

    va_end(args);
    return expr;
}

/*
 * Add a statement to the list of ir operations in the block. Parameters
 * are given by the statement opcode.
 *
 * Current block can be NULL when parsing an expression that should not
 * be evaluated, for example argument to sizeof.
 */
INTERNAL void emit_ir(struct block *block, enum sttype st, ...)
{
    va_list args;
    struct statement stmt = {0};

    if (block) {
        va_start(args, st);
        stmt.st = st;
        stmt.t = var_void();
        switch (st) {
        case IR_ASSIGN:
        case IR_VLA_ALLOC:
            stmt.t = va_arg(args, struct var);
        case IR_EXPR:
        case IR_PARAM:
        case IR_VA_START:
            stmt.expr = va_arg(args, struct expression);
            break;
        case IR_ASM:
            stmt.asm_index = va_arg(args, int);
            break;
        }

        array_push_back(&block->code, stmt);
        va_end(args);
    }
}

/*
 * Evaluate standalone expressions which are not assigned to a variable.
 *
 * Unless the expression is a function call, it can be ignored. As a
 * special case, function calls returning non-primitive values are
 * explicitly evaluated to a new temporary. This is to easier support
 * call convention in x86_64, where the callee writes the result object
 * and we have to provide some valid storage.
 */
INTERNAL struct expression eval_expression_statement(
    struct definition *def,
    struct block *block,
    struct expression expr)
{
    struct var res;

    if (has_side_effects(expr)) {
        if (!is_struct_or_union(expr.type)) {
            emit_ir(block, IR_EXPR, expr);
        } else {
            res = create_var(def, expr.type);
            emit_ir(block, IR_ASSIGN, res, expr);
        }
    }

    return as_expr(var_void());
}

INTERNAL struct var eval(
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

#define cast_immediate(v, t, T) ( \
    is_signed(t) ? (T) (v).i : \
    is_unsigned(t) || is_pointer(t) ? (T) (v).u : \
    is_float(t) ? (T) (v).f : \
    is_double(t) ? (T) (v).d : (T) (v).ld)

#define is_float_above(v, t, n) \
    ((is_float(t) && (v).f > n) \
        || (is_double(t) && (v).d > n) \
        || (is_long_double(t) && (v).ld > n))

#define is_float_below(v, t, n) \
    ((is_float(t) && (v).f < n) \
        || (is_double(t) && (v).d < n) \
        || (is_long_double(t) && (v).ld < n))

INTERNAL union value convert(union value val, Type type, Type to)
{
    switch (type_of(to)) {
    case T_FLOAT:
        val.f = is_double(type) ? (float) val.d
              : is_signed(type) ? (float) val.i
              : is_unsigned(type) ? (float) val.u
              : is_long_double(type) ? (float) val.ld
              : val.f;
        break;
    case T_DOUBLE:
        val.d = is_float(type) ? (double) val.f
              : is_signed(type) ? (double) val.i
              : is_unsigned(type) ? (double) val.u
              : is_long_double(type) ? (double) val.ld
              : val.d;
        break;
    case T_LDOUBLE:
        val.ld = is_float(type) ? (long double) val.f
              : is_double(type) ? (long double) val.d
              : is_signed(type) ? (long double) val.i
              : is_unsigned(type) ? (long double) val.u
              : val.ld;
        break;
    case T_BOOL:
        val.u = immediate_bool_value(val, type);
        break;
    case T_CHAR:
        if (is_signed(to)) {
            val.i = is_float_below(val, type, CHAR_MIN) ? CHAR_MIN
                  : is_float_above(val, type, CHAR_MAX) ? CHAR_MAX
                  : cast_immediate(val, type, signed char);
        } else {
            val.u = is_float_below(val, type, 0) ? 0
                  : is_float_above(val, type, UCHAR_MAX) ? UCHAR_MAX
                  : cast_immediate(val, type, unsigned char);
        }
        break;
    case T_SHORT:
        if (is_signed(to)) {
            val.i = is_float_below(val, type, SHRT_MIN) ? SHRT_MIN
                  : is_float_above(val, type, SHRT_MAX) ? SHRT_MAX
                  : cast_immediate(val, type, signed short);
        } else {
            val.u = is_float_below(val, type, 0) ? 0
                  : is_float_above(val, type, USHRT_MAX) ? USHRT_MAX
                  : cast_immediate(val, type, unsigned short);
        }
        break;
    case T_INT:
        if (is_signed(to)) {
            val.i = is_float_below(val, type, INT_MIN) ? INT_MIN
                  : is_float_above(val, type, INT_MAX) ? INT_MAX
                  : cast_immediate(val, type, signed int);
        } else {
            val.u = is_float_below(val, type, 0) ? 0
                  : is_float_above(val, type, UINT_MAX) ? UINT_MAX
                  : cast_immediate(val, type, unsigned int);
        }
        break;
    case T_LONG:
        if (is_signed(to)) {
            val.i = is_float_below(val, type, LONG_MIN) ? LONG_MIN
                  : is_float_above(val, type, LONG_MAX) ? LONG_MAX
                  : cast_immediate(val, type, signed long);
        } else {
    case T_POINTER:
            val.u = is_float_below(val, type, 0) ? 0
                  : is_float_above(val, type, ULONG_MAX) ? ULONG_MAX
                  : cast_immediate(val, type, unsigned long);
        }
        break;
    default: assert(0);
    }

    return val;
}

/*
 * All immediate conversions must be evaluated compile time. Also handle
 * conversion which can be done by reinterpreting memory.
 */
static struct expression cast(struct var var, Type type)
{
    struct expression expr;

    if (is_void(type))
        return as_expr(var_void());

    if (!is_scalar(var.type) || !is_scalar(type)) {
        error("Cannot cast %t to %t.", var.type, type);
        exit(1);
    }

    if (is_pointer(var.type) && (is_float(type) || is_double(type))) {
        error("Cannot cast pointer to %t", type);
        exit(1);
    }

    if (var.kind == IMMEDIATE) {
        assert(!var.offset);
        var = var_numeric(type, convert(var.imm, var.type, type));
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

static struct var eval_cast(
    struct definition *def,
    struct block *block,
    struct var var,
    Type type)
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
    Type type;

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_arithmetic_immediate(type, l, *, r);
        expr = cast(l, type);
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
    Type type;

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_arithmetic_immediate(type, l, /, r);
        expr = cast(l, type);
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
    Type type;

    type = usual_arithmetic_conversion(l.type, r.type);
    if (!is_integer(type)) {
        error("Operands of modulo operator must be of integer type.");
        exit(1);
    }

    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, %, r);
        expr = cast(l, type);
    } else {
        expr = create_expr(IR_OP_MOD, l, r);
    }

    return expr;
}

/*
 * Evaluate arithmetic addition.
 *
 * Special case for immediate string literal + constant, and ADDRESS +
 * constant. These can be evaluated immediately. If r is immediate zero,
 * no evaluation is necessary.
 */
static struct expression add(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    size_t size;
    struct expression expr;
    Type type;

    if (is_integer(l.type) && is_pointer(r.type)) {
        return add(def, block, r, l);
    }

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        type = usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(def, block, l, type);
        r = eval_cast(def, block, r, type);
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = eval_arithmetic_immediate(type, l, +, r);
            expr = cast(l, type);
        } else if (l.kind == ADDRESS && r.kind == IMMEDIATE) {
            l.offset += r.imm.i;
            expr = as_expr(l);
        } else if (l.kind == IMMEDIATE && r.kind == ADDRESS) {
            r.offset += l.imm.i;
            expr = as_expr(r);
        } else {
            expr = create_expr(IR_OP_ADD, l, r);
        }
    } else if (is_pointer(l.type) && is_integer(r.type)) {
        type = type_deref(l.type);
        size = size_of(type);
        r = eval_cast(def, block, r, basic_type__long);
        if (is_vla(type)) {
            expr = eval_vla_size(def, block, type);
            expr = eval_expr(def, block, IR_OP_MUL, eval(def, block, expr), r);
            type = l.type;
            l.type = basic_type__long;
            expr = create_expr(IR_OP_ADD, l, eval(def, block, expr));
            expr.type = type;
        } else if (!size) {
            error("Pointer arithmetic on incomplete type %t.", l.type);
            exit(1);
        } else if (l.kind == ADDRESS && r.kind == IMMEDIATE) {
            l.offset += r.imm.i * size;
            expr = as_expr(l);
        } else if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l.imm.i += r.imm.i * size;
            expr = as_expr(l);
        } else if (r.kind != IMMEDIATE || r.imm.i) {
            type = l.type;
            l = eval_cast(def, block, l, basic_type__long);
            if (size != 1) {
                r = eval(def, block,
                        eval_expr(def, block, IR_OP_MUL,
                            imm_signed(basic_type__long, size), r));
            }
            expr = create_expr(IR_OP_ADD, l, r);
            expr.type = type;
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

/*
 * Evaluate arithmetic subtraction.
 *
 * Special case for immediate string literal - constant, and ADDRESS -
 * constant. These can be evaluated immediately. If r is immediate zero,
 * no evaluation is necessary.
 *
 * Result of pointer subtraction is ptrdiff_t, which will be signed 64-
 * bit integer.
 */
static struct expression sub(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    size_t size;
    struct expression expr;
    Type type, t1, t2;

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        type = usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(def, block, l, type);
        r = eval_cast(def, block, r, type);
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = eval_arithmetic_immediate(type, l, -, r);
            expr = cast(l, type);
        } else if (l.kind == ADDRESS && r.kind == IMMEDIATE) {
            l.offset -= r.imm.i;
            expr = as_expr(l);
        } else {
            expr = create_expr(IR_OP_SUB, l, r);
        }
    } else if (is_pointer(l.type) && is_integer(r.type)) {
        type = type_deref(l.type);
        size = size_of(type);
        r = eval_cast(def, block, r, basic_type__long);
        if (is_vla(type)) {
            expr = eval_vla_size(def, block, type);
            expr = eval_expr(def, block, IR_OP_MUL, eval(def, block, expr), r);
            type = l.type;
            l.type = basic_type__long;
            expr = create_expr(IR_OP_SUB, l, eval(def, block, expr));
            expr.type = type;
        } else if (!size) {
            error("Pointer arithmetic on incomplete type.");
            exit(1);
        } else if (l.kind == ADDRESS && r.kind == IMMEDIATE) {
            l.offset -= r.imm.i * size;
            expr = as_expr(l);
        } else if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l.imm.i -= r.imm.i * size;
            expr = as_expr(l);
        } else if (is_nullptr(r)) {
            expr = as_expr(l);
        } else {
            if (size > 1) {
                r = eval(def, block,
                    eval_expr(def, block, IR_OP_MUL,
                        imm_unsigned(basic_type__unsigned_long, size), r));
            }
            expr = create_expr(IR_OP_SUB, l, r);
        }
    } else if (is_pointer(l.type) && is_pointer(r.type)) {
        t1 = type_deref(l.type);
        t2 = type_deref(r.type);
        size = size_of(t2);
        l = eval_cast(def, block, l, basic_type__long);
        r = eval_cast(def, block, r, basic_type__long);
        expr = eval_expr(def, block, IR_OP_SUB, l, r);
        if (is_vla(t1)) {
            l = eval(def, block, expr);
            expr = eval_vla_size(def, block, t1);
            expr = eval_expr(def, block, IR_OP_DIV, l, eval(def, block, expr));
        } else if (size > 1) {
            l = eval(def, block, expr);
            r = imm_signed(basic_type__long, size);
            expr = eval_expr(def, block, IR_OP_DIV, l, r);
        } else if (!size || size_of(t1) != size) {
            error("Referenced type is incomplete.");
            exit(1);
        }
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
    Type type, t1, t2;

    /* Normalize by putting most specific pointer as left argument. */
    if (is_pointer(r->type)
        && (!is_pointer(l->type)
            || (is_void(type_next(l->type)) && !is_void(type_next(r->type)))))
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
            t1 = type_deref(l->type);
            t2 = type_deref(r->type);
            if (!is_compatible_unqualified(t1, t2)
                && !(is_void(t1) && size_of(t2))
                && !(is_void(t2) && size_of(t1)))
            {
                warning("Comparison between incompatible types '%t' and '%t'.",
                    l->type, r->type);
            }
        } else if (!is_nullptr(*r)) {
            warning("Comparison between pointer and non-zero integer.");
        }

        *l = eval_cast(def, block, *l, basic_type__unsigned_long);
        *r = eval_cast(def, block, *r, basic_type__unsigned_long);
    } else {
        error("Illegal comparison between types '%t' and '%t'.",
            l->type, r->type);
        exit(1);
    }
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

static Type common_compare_type(Type left, Type right)
{
    if (is_arithmetic(left) && is_arithmetic(right)) {
        return usual_arithmetic_conversion(left, right);
    }

    if (is_pointer(left) && is_pointer(right)
        && is_compatible_unqualified(type_next(left), type_next(right)))
    {
        return basic_type__unsigned_long;
    }

    error("Invalid operands in relational expression: %t and %t", left, right);
    exit(1);
}

static struct expression cmp_ge(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    struct expression expr;
    Type type;

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
    Type type;

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
    Type type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise or must have integer type.");
        exit(1);
    }

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, |, r);
        expr = cast(l, type);
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
    Type type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise xor must have integer type.");
        exit(1);
    }

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, ^, r);
        expr = cast(l, type);
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
    Type type;

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
    Type type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Shift operands must have integer type.");
        exit(1);
    }

    type = promote_integer(l.type);
    l = eval_cast(def, block, l, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, <<, r);
        expr = cast(l, type);
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
    Type type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Shift operands must have integer type.");
        exit(1);
    }

    type = promote_integer(l.type);
    l = eval_cast(def, block, l, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_integer_immediate(type, l, >>, r);
        expr = cast(l, type);
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
    Type type;

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

static struct expression neg(
    struct definition *def,
    struct block *block,
    struct var var)
{
    struct expression expr;

    if (!is_arithmetic(var.type)) {
        error("Unary (-) operand must be of arithmetic type.");
        exit(1);
    }

    if (is_float(var.type)) {
        if (var.kind == IMMEDIATE) {
            var.imm.f = -var.imm.f;
            expr = as_expr(var);
        } else {
            expr = create_expr(IR_OP_NEG, var);
        }
    } else if (is_double(var.type)) {
        if (var.kind == IMMEDIATE) {
            var.imm.d = -var.imm.d;
            expr = as_expr(var);
        } else {
            expr = create_expr(IR_OP_NEG, var);
        }
    } else {
        expr = sub(def, block, var_int(0), var);
    }

    return expr;
}

static struct expression call(struct var var)
{
    if (!is_pointer(var.type) || !is_function(type_next(var.type))) {
        error("Calling non-function type %t.", var.type);
        exit(1);
    }

    return create_expr(IR_OP_CALL, var);
}

static struct expression eval_va_arg(struct var var, Type type)
{
    assert(is_pointer(var.type));
    return create_expr(IR_OP_VA_ARG, var, type);
}

/*
 * Convert variables of type ARRAY or FUNCTION to addresses when used
 * in expressions.
 *
 * Array of T is converted (decay) into pointer to T. Not the same as
 * taking the address of an array, which would give 'pointer to array
 * of T'.
 *
 * Fields of unsigned type are promoted to signed int if the signed
 * type can represent all values of the unsigned type.
 */
INTERNAL struct var rvalue(
    struct definition *def,
    struct block *block,
    struct var var)
{
    if (is_function(var.type)) {
        var = eval_addr(def, block, var);
    } else if (is_array(var.type)) {
        if (is_vla(var.type)) {
            if (var.kind == DIRECT) {
                assert(is_vla(var.symbol->type));
                var = var_direct(var.symbol->value.vla_address);
            } else {
                assert(var.kind == DEREF);
                assert(!var.offset);
                var.kind = DIRECT;
                var.type = type_create_pointer(type_next(var.type));
            }
        } else {
            var.type = type_next(var.type);
            var = eval_addr(def, block, var);
        }
    } else if (is_field(var) && is_unsigned(var.type)) {
        if (var.field_width < size_of(basic_type__int) * 8) {
            var = eval(def, block, cast(var, basic_type__int));
        }
    }

    return var;
}

/*
 * Ensure expression has scalar type.
 *
 * Expression is used directly in branching statements, which do not
 * support va_arg directly in backend. Avoid this by evaluating to a
 * new variable.
 */
INTERNAL struct block *scalar(
    struct definition *def,
    struct block *block,
    const char *entity)
{
    struct var tmp;

    if (!is_scalar(block->expr.type) && is_identity(block->expr)) {
        block->expr = as_expr(rvalue(def, block, block->expr.l));
    }

    if (!is_scalar(block->expr.type)) {
        error("%s must be scalar, was %t", entity, block->expr.type);
        exit(1);
    }

    if (block->expr.op == IR_OP_VA_ARG) {
        tmp = create_var(def, block->expr.type);
        tmp = eval_assign(def, block, tmp, block->expr);
        block->expr = as_expr(tmp);
    }

    return block;
}

INTERNAL struct expression eval_expr(
    struct definition *def,
    struct block *block,
    enum optype optype,
    struct var l, ...)
{
    va_list args;
    struct var r;
    Type type;

    l = rvalue(def, block, l);
    va_start(args, l);
    switch (optype) {
    case IR_OP_CAST:
    case IR_OP_VA_ARG:
        type = va_arg(args, Type);
        break;
    case IR_OP_NOT:
    case IR_OP_NEG:
    case IR_OP_CALL:
        break;
    default:
        r = va_arg(args, struct var);
        r = rvalue(def, block, r);
        break;
    }

    va_end(args);
    switch (optype) {
    default: assert(0);
    case IR_OP_CAST:   return cast(l, type);
    case IR_OP_CALL:   return call(l);
    case IR_OP_VA_ARG: return eval_va_arg(l, type);
    case IR_OP_NOT:    return not(def, block, l);
    case IR_OP_NEG:    return neg(def, block, l);
    case IR_OP_MOD:    return mod(def, block, l, r);
    case IR_OP_MUL:    return mul(def, block, l, r);
    case IR_OP_DIV:    return ediv(def, block, l, r);
    case IR_OP_ADD:    return add(def, block, l, r);
    case IR_OP_SUB:    return sub(def, block, l, r);
    case IR_OP_EQ:     return cmp_eq(def, block, l, r);
    case IR_OP_NE:     return cmp_ne(def, block, l, r);
    case IR_OP_GE:     return cmp_ge(def, block, l, r);
    case IR_OP_GT:     return cmp_gt(def, block, l, r);
    case IR_OP_AND:    return and(def, block, l, r);
    case IR_OP_XOR:    return xor(def, block, l, r);
    case IR_OP_OR:     return or(def, block, l, r);
    case IR_OP_SHL:    return shiftl(def, block, l, r);
    case IR_OP_SHR:    return shiftr(def, block, l, r);
    }
}

INTERNAL struct expression eval_unary_plus(struct var val)
{
    Type type;

    if (!is_arithmetic(val.type)) {
        error("Unary (+) operand must be of arithmetic type.");
        exit(1);
    }

    if (is_integer(val.type)) {
        type = promote_integer(val.type);
        return cast(val, type);
    }

    val.lvalue = 0;
    return as_expr(val);
}

INTERNAL struct var eval_addr(
    struct definition *def,
    struct block *block,
    struct var var)
{
    struct var tmp, ptr;

    if (is_field(var)) {
        error("Cannot take address of bit-field.");
        exit(1);
    }

    if (is_vla(var.type) && var.kind == DIRECT) {
        assert(is_vla(var.symbol->type));
        assert(var.symbol);
        var = var_direct(var.symbol->value.vla_address);
        return var;
    }

    switch (var.kind) {
    case IMMEDIATE:
        if (!var.symbol || var.symbol->symtype != SYM_LITERAL) {
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
            var.type = type_create_pointer(var.type);
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
            var.type = type_create_pointer(var.type);
            var.imm.u += var.offset;
            var.offset = 0;
            var.lvalue = 0;
        } else {
            /*
             * Address of *(sym + offset) is not *(&sym + offset), so
             * not possible to just convert to DIRECT. Offset must be
             * applied after converting to direct pointer.
             */
            assert(is_pointer(var.symbol->type));
            tmp = var_direct(var.symbol);
            if (var.offset) {
                ptr = eval_cast(def, block, tmp,
                    type_create_pointer(basic_type__char));
                tmp = eval(def, block,
                    eval_expr(def, block, IR_OP_ADD, ptr, var_int(var.offset)));
            }
            tmp.type = type_create_pointer(var.type);
            var = tmp;
        }
        break;
    }

    assert(is_pointer(var.type));
    return var;
}

INTERNAL struct var eval_deref(
    struct definition *def,
    struct block *block,
    struct var var)
{
    var = rvalue(def, block, var);
    if (!is_pointer(var.type)) {
        error("Dereferencing non-pointer type '%t'.", var.type);
        exit(1);
    }

    switch (var.kind) {
    case DEREF:
        /*
         * Dereferencing *(sym + offset) must evaluate pointer into a
         * new temporary, before marking that as DEREF var.
         */
        var = eval_assign(def, block, create_var(def, var.type), as_expr(var));
        break;
    case DIRECT:
        if (var.offset != 0 || !is_pointer(var.symbol->type)) {
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
         * Also the case when indexing into string literals, in which
         * case symbol is not NULL.
         */
        var.kind = DEREF;
        var.type = type_deref(var.type);
        var.lvalue = 1;
        assert(!var.symbol || var.symbol->symtype == SYM_LITERAL);
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

/*
 * Special case char [] = string in initializers.
 *
 * Variables with incomplete type gets a size assigned directly on the
 * the symbol.
 *
 *  char foo[] = "Hello"
 *
 * Incomplete initialization leaves the resulting variables offset
 * pointing to the first element that was not initialized. In the
 * following example, target.offset = 4 on return.
 *
 *  char bar[6] = "Hei"
 *
 */
static struct var eval_assign_string_literal(
    struct block *block,
    struct var target,
    struct expression expr)
{
    Type type;

    assert(is_identity(expr));
    assert(is_array(target.type));
    assert(expr.l.symbol->symtype == SYM_LITERAL);
    if (!is_char(type_next(target.type))) {
        error("Assigning string literal to non-char array.");
        exit(1);
    }

    if (!size_of(target.type)) {
        assert(target.kind == DIRECT);
        assert(target.offset == 0);
        assert(size_of(target.symbol->type) == 0);
        set_array_length(target.symbol->type, size_of(expr.type));
        target.type = expr.type;
        emit_ir(block, IR_ASSIGN, target, expr);
    } else {
        if (size_of(expr.type) == size_of(target.type) + 1) {
            expr.type = target.type;
            expr.l.type = target.type;
        } else if (size_of(expr.type) > size_of(target.type)) {
            error("Length of string literal exceeds target array size.");
            exit(1);
        }

        type = target.type;
        target.type = expr.type;
        emit_ir(block, IR_ASSIGN, target, expr);
        target.type = type;
    }

    target.offset += size_of(expr.type);
    return target;
}

/*
 * Assign value to field.
 *
 * Immediate values are masked to remove bits not within field width,
 * and returned directly as result of the assignment.
 */
static struct var assign_field(
    struct definition *def,
    struct block *block,
    struct var target,
    struct expression expr)
{
    Type type;
    long mask;

    assert(is_field(target));
    assert(is_integer(target.type));
    switch (size_of(target.type)) {
    case 1:
        type = basic_type__bool;
        break;
    case 2:
        type = basic_type__short;
        break;
    case 4:
        type = basic_type__int;
        break;
    default:
        assert(size_of(target.type) == 8);
        type = basic_type__long;
        break;
    }

    if (is_immediate(expr)) {
        mask = (1l << target.field_width) - 1;
        expr.l.imm.i &= mask;
        if (is_signed(target.type)
            && (expr.l.imm.i & (1l << (target.field_width - 1))))
        {
            expr.l.imm.i |= ~mask;
        }
        expr.l.type = type;
        expr.type = type;
    } else if (!type_equal(type, expr.type)) {
        expr = eval_expr(def, block, IR_OP_CAST, eval(def, block, expr), type);
    }

    emit_ir(block, IR_ASSIGN, target, expr);
    if (is_immediate(expr)) {
        target = expr.l;
    } else {
        target.lvalue = 0;
    }

    return target;
}

static int has_qualifiers(Type a, Type b)
{
    return is_const(a) >= is_const(b) && is_volatile(a) >= is_volatile(b);
}

static struct var eval_assign_pointer(
    struct definition *def,
    struct block *block,
    struct var target,
    struct expression expr)
{
    Type l, r;
    assert(is_pointer(target.type));

    if (is_pointer(expr.type)) {
        l = type_deref(target.type);
        r = type_deref(expr.type);
        if (!((is_compatible_unqualified(l, r) && has_qualifiers(l, r))
            || (is_object(l) && is_void(r) && has_qualifiers(l, r))
            || (is_void(l) && is_object(r) && has_qualifiers(l, r))
            || (is_identity(expr) && is_nullptr(expr.l))))
        {
            warning("Incompatible pointer assignment between %t and %t.",
                target.type, expr.type);
        }
    } else if (is_integer(expr.type)) {
        if (!is_identity(expr) || !is_nullptr(expr.l)) {
            warning("Assigning non-zero number to pointer.");
        }
    } else {
        error("Incompatible pointer assignment between %t and %t.",
            target.type, expr.type);
        exit(1);
    }

    if (is_identity(expr)) {
        expr.l.type = target.type;
    }

    expr.type = target.type;
    emit_ir(block, IR_ASSIGN, target, expr);
    if (is_immediate(expr)) {
        target = expr.l;
    } else {
        target.lvalue = 0;
    }

    return target;
}

/*
 * Convert expression to boolean before assignment. Numeric values are
 * compared to zero.
 */
static struct expression eval_bool(
    struct definition *def,
    struct block *block,
    struct expression expr)
{
    struct var val;

    if (!is_bool(expr.type)) {
        val = eval(def, block, expr);
        expr = eval_expr(def, block, IR_OP_NE,
            imm_signed(basic_type__long, 0), val);
    }

    return expr;
}

INTERNAL struct var eval_assign(
    struct definition *def,
    struct block *block,
    struct var target,
    struct expression expr)
{
    struct var var;

    if (!target.lvalue) {
        error("Target of assignment must be l-value.");
        exit(1);
    }

    if (is_array(target.type)) {
        return eval_assign_string_literal(block, target, expr);
    } else if (is_identity(expr)) {
        var = rvalue(def, block, expr.l);
        expr = as_expr(var);
    }

    if (is_bool(target.type)) {
        expr = eval_bool(def, block, expr);
    }

    if (is_pointer(target.type)) {
        return eval_assign_pointer(def, block, target, expr);
    }

    if (is_arithmetic(target.type) && is_arithmetic(expr.type)) {
        if (is_field(target)) {
            return assign_field(def, block, target, expr);
        } else if (is_identity(expr) && expr.l.kind == IMMEDIATE) {
            var = eval_cast(def, block, expr.l, target.type);
            expr = as_expr(var);
        } else if (!type_equal(target.type, expr.type)) {
            var = eval(def, block, expr);
            expr = eval_expr(def, block, IR_OP_CAST, var, target.type);
        }
    } else if (
        !(is_struct_or_union(target.type)
            && is_compatible_unqualified(target.type, expr.type)))
    {
        error("Incompatible value of type %t assigned to variable of type %t.",
            expr.type, target.type);
        exit(1);
    }

    emit_ir(block, IR_ASSIGN, target, expr);
    if (is_immediate(expr)) {
        target = expr.l;
    } else {
        target.lvalue = 0;
    }

    return target;
}

INTERNAL struct var eval_copy(
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

INTERNAL struct expression eval_return(
    struct definition *def,
    struct block *block)
{
    struct var val;
    Type type;

    type = type_next(def->symbol->type);
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

INTERNAL Type eval_conditional(
    struct definition *def,
    struct block *left,
    struct block *right)
{
    struct var lval, rval;
    Type t1, t2, p1, p2, type;

    lval = rvalue(def, left, eval(def, left, left->expr));
    rval = rvalue(def, right, eval(def, right, right->expr));

    t1 = lval.type;
    t2 = rval.type;
    left->expr = as_expr(lval);
    right->expr = as_expr(rval);

    if (is_arithmetic(t1) && is_arithmetic(t2)) {
        type = usual_arithmetic_conversion(t1, t2);
    } else if (is_pointer(t1) && is_pointer(t2)) {
        p1 = type_deref(t1);
        p2 = type_deref(t2);
        if (is_compatible_unqualified(p1, p2) || is_void(p1)) {
            type = type_apply_qualifiers(p1, p2);
        } else if (is_void(p2)) {
            type = type_apply_qualifiers(p2, p1);
        } else {
            error("Incompatible pointer types in conditional expression.");
            exit(1);
        }

        type = type_create_pointer(type);
        type = type_apply_qualifiers(type, t1);
        type = type_apply_qualifiers(type, t2);
    } else if (is_pointer(t1) && is_nullptr(rval)) {
        type = t1;
    } else if (is_pointer(t2) && is_nullptr(lval)) {
        type = t2;
    } else if (is_void(t1) && is_void(t2)) {
        type = t1;
    } else if (is_struct_or_union(t1) && type_equal(t1, t2)) {
        type = t1;
    } else {
        error("Incompatible types (%t, %t) in conditional operator.", t1, t2);
        exit(1);
    }

    return type;
}

/*
 * Connect basic blocks in a logical OR or AND operation.
 *
 * left:      Left hand side of the expression, left->expr holds the
 *            value to be compared.
 * right_top: First block of the right hand side expression. This should
 *            be jump target on AND if left->expr is false.
 * right:     Last block of the right hand side expression. The value to
 *            compare is in right->expr.
 *
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
    int b;
    struct var res;
    struct block
        *t = cfg_block_init(def),
        *f = cfg_block_init(def),
        *r = cfg_block_init(def);

    if (is_and) {
        left->jump[0] = f;
        left->jump[1] = right_top;
    } else {
        left->jump[0] = right_top;
        left->jump[1] = t;
    }

    b = immediate_bool(right->expr);
    if (b == 1) {
        right->jump[0] = t;
    } else if (b == 0) {
        right->jump[0] = f;
    } else {
        assert(b == -1);
        right->jump[0] = f;
        right->jump[1] = t;
    }

    res = create_var(def, basic_type__int);
    res.lvalue = 1;
    eval_assign(def, t, res, as_expr(var_int(1)));
    eval_assign(def, f, res, as_expr(var_int(0)));
    res.lvalue = 0;

    t->jump[0] = r;
    f->jump[0] = r;
    r->expr = as_expr(res);
    return r;
}

INTERNAL void eval_vla_alloc(
    struct definition *def,
    struct block *block,
    const struct symbol *sym)
{
    assert(is_vla(sym->type));
    block->expr = eval_vla_size(def, block, sym->type);
    emit_ir(block, IR_VLA_ALLOC, var_direct(sym), block->expr);
}

INTERNAL struct expression eval_vla_size(
    struct definition *def,
    struct block *block,
    Type type)
{
    const struct symbol *len;
    struct expression expr;
    struct var size;
    Type base;

    assert(is_vla(type));
    len = type_vla_length(type);
    base = type_next(type);
    if (len) {
        if (is_vla(base)) {
            size = eval(def, block, eval_vla_size(def, block, base));
            expr = eval_expr(def, block, IR_OP_MUL, var_direct(len), size);
        } else if (size_of(base) > 1) {
            size = imm_unsigned(basic_type__unsigned_long, size_of(base));
            expr = eval_expr(def, block, IR_OP_MUL, var_direct(len), size);
        } else {
            assert(size_of(base) == 1);
            expr = as_expr(var_direct(len));
        }
    } else {
        size = imm_unsigned(basic_type__unsigned_long, type_array_len(type));
        expr = eval_vla_size(def, block, base);
        expr = eval_expr(def, block, IR_OP_MUL, size, eval(def, block, expr));
    }

    return expr;
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

INTERNAL struct block *eval_logical_or(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right)
{
    int res, b;
    struct var value;

    left = scalar(def, left, "Left operand of logical or");
    right = scalar(def, right, "Right operand of logical or");

    b = immediate_bool(left->expr);
    if (is_logical_immediate(left, right_top, right)) {
        res = b == 1 || immediate_bool(right->expr) == 1;
        left->expr = as_expr(var_int(res));
    } else if (b == 1) {
        left->expr = as_expr(var_int(1));
    } else if (b == 0) {
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

INTERNAL struct block *eval_logical_and(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right)
{
    int res, b;
    struct var value;

    left = scalar(def, left, "Left operand of logical and");
    right = scalar(def, right, "Right operand of logical and");

    b = immediate_bool(left->expr);
    if (is_logical_immediate(left, right_top, right)) {
        res = b == 1 && immediate_bool(right->expr) == 1;
        left->expr = as_expr(var_int(res));
    } else if (b == 0) {
        left->expr = as_expr(var_int(0));
    } else if (b == 1) {
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

INTERNAL struct expression eval_param(
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

INTERNAL void param(struct block *block, struct expression expr)
{
    emit_ir(block, IR_PARAM, expr);
}

INTERNAL void eval__builtin_va_start(
    struct block *block,
    struct expression arg)
{
    emit_ir(block, IR_VA_START, arg);
}
