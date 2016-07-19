#include "eval.h"
#include "declaration.h"
#include "parse.h"
#include "symtab.h"
#include "type.h"
#include <lacc/context.h>
#include <lacc/ir.h>

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>

int is_immediate_true(struct var e)
{
    if (e.kind == IMMEDIATE) {
        assert(is_scalar(e.type));
        return
            (is_signed(e.type)) ? e.imm.i != 0l :
            (is_unsigned(e.type) || is_pointer(e.type)) ? e.imm.u != 0ul :
            (is_float(e.type)) ? e.imm.f != 0.0f : e.imm.d != 0.0;
    }

    return 0;
}

int is_immediate_false(struct var e)
{
    if (e.kind == IMMEDIATE) {
        assert(is_scalar(e.type));
        return
            is_signed(e.type) ? e.imm.i == 0l :
            is_unsigned(e.type) || is_pointer(e.type) ? e.imm.u == 0ul :
            is_float(e.type) ? e.imm.f == 0.0f : e.imm.d == 0.0;
    }

    return 0;
}

static int is_nullptr(struct var val)
{
    return (is_integer(val.type) || is_pointer(val.type))
        && is_immediate_false(val);
}

static int is_string(struct var val)
{
    return val.kind == IMMEDIATE && val.symbol
        && val.symbol->symtype == SYM_STRING_VALUE;
}

static int is_field(struct var val)
{
    return val.width;
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

static void emit_ir(struct block *block, enum optype optype, struct var a, ...)
{
    va_list args;
    struct op op;

    /* Current block can be NULL when parsing an expression that should
     * not be evaluated, for example argument to sizeof. */
    if (block) {
        op.type = optype;
        op.a = a;
        if (OPERAND_COUNT(optype) > 1) {
            va_start(args, a);
            op.b = va_arg(args, struct var);
            if (OPERAND_COUNT(optype) == 3) {
                op.c = va_arg(args, struct var);
            }
            va_end(args);
        }
        array_push_back(&block->code, op);
    }
}

static struct var evaluate(
    struct definition *def,
    struct block *block,
    enum optype optype,
    const struct typetree *type,
    struct var left, ...)
{
    va_list args;
    struct var result;

    result = create_var(def, type);
    result.lvalue = 0;
    if (OPERAND_COUNT(optype) == 3) {
        va_start(args, left);
        emit_ir(block, optype, result, left, va_arg(args, struct var));
        va_end(args);
    } else {
        emit_ir(block, optype, result, left);
    }

    return result;
}

static struct var eval_mul(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_arithmetic_immediate(type, l, *, r);
    } else {
        l = evaluate(def, block, IR_OP_MUL, type, l, r);
    }

    return l;
}

static struct var eval_div(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);
    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        l = eval_arithmetic_immediate(type, l, /, r);
    } else {
        l = evaluate(def, block, IR_OP_DIV, type, l, r);
    }

    return l;
}

static struct var eval_mod(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
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
    } else {
        l = evaluate(def, block, IR_OP_MOD, type, l, r);
    }

    return l;
}

static struct var eval_add(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        type = usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(def, block, l, type);
        r = eval_cast(def, block, r, type);
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = eval_arithmetic_immediate(type, l, +, r);
        } else {
            l = evaluate(def, block, IR_OP_ADD, type, l, r);
        }
    } else if (is_integer(l.type) && is_pointer(r.type)) {
        /* Make sure pointer is left, and integer right. */
        l = eval_add(def, block, r, l);
    } else if (is_pointer(l.type) && is_integer(r.type)) {
        if (!size_of(l.type->next)) {
            error("Pointer arithmetic on incomplete type.");
            exit(1);
        }
        /* Special case for immediate string literal + constant, and
         * ADDRESS + constant. These can be evaluated immediately. */
        if ((is_string(l) || l.kind == ADDRESS)
            && r.kind == IMMEDIATE
            && is_integer(r.type))
        {
            assert(!is_tagged(l.type));
            l.offset += r.imm.i * size_of(l.type->next);
        }
        /* Evaluate unless r is immediate zero. */
        else if (r.kind != IMMEDIATE || r.imm.i) {
            r = eval_expr(def, block, IR_OP_MUL,
                    var_int(size_of(l.type->next)), r);
            l = evaluate(def, block, IR_OP_ADD, l.type, l, r);
        }
    } else {
        error("Incompatible arguments to addition operator, was '%t' and '%t'.",
            l.type, r.type);
        exit(1);
    }

    return l;
}

static struct var eval_sub(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        type = usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(def, block, l, type);
        r = eval_cast(def, block, r, type);
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = eval_arithmetic_immediate(type, l, -, r);
        } else {
            l = evaluate(def, block, IR_OP_SUB, type, l, r);
        }
    } else if (is_pointer(l.type) && is_integer(r.type)) {
        if (!size_of(l.type->next)) {
            error("Pointer arithmetic on incomplete type.");
            exit(1);
        }
        /* Special case for immediate string literal - constant, and
         * ADDRESS - constant. These can be evaluated immediately. */
        if ((is_string(l) || l.kind == ADDRESS)
            && r.kind == IMMEDIATE
            && is_integer(r.type))
        {
            assert(!is_tagged(l.type));
            l.offset -= r.imm.i * size_of(l.type->next);
        }
        /* Evaluate unless r is immediate zero. */
        else if (r.kind != IMMEDIATE || r.imm.i) {
            r = eval_expr(def, block, IR_OP_MUL,
                    var_int(size_of(l.type->next)), r);
            l = evaluate(def, block, IR_OP_SUB, l.type, l, r);
        }
    } else if (is_pointer(l.type) && is_pointer(r.type)) {
        size_t elem_size = size_of(r.type->next);

        if (!size_of(l.type->next) ||
            size_of(l.type->next) != size_of(r.type->next))
        {
            error("Referenced type is incomplete.");
            exit(1);
        }

        /* Result is ptrdiff_t, which will be signed 64 bit integer.
         * Reinterpret cast both pointers to unsigned long (no codegen),
         * and store result as signed long. Then divide by element size
         * to get diff. */
        l = eval_cast(def, block, l, &basic_type__unsigned_long);
        r = eval_cast(def, block, r, &basic_type__unsigned_long);
        l = eval_expr(def, block, IR_OP_SUB, l, r);
        l = eval_expr(def, block, IR_OP_DIV, l, var_int(elem_size));
    } else {
        error("Incompatible arguments to subtraction operator, was %t and %t.",
            l.type, r.type);
        exit(1);
    }

    return l;
}

static struct var eval_eq(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    /* Normalize by putting most specific pointer type as left argument.
       If both sides are pointer, but only one is void *, move the void
       pointer to the right. */
    if (is_pointer(r.type)
        && (!is_pointer(l.type)
            || (is_void(l.type->next) && !is_void(r.type->next))))
    {
        return eval_eq(def, block, r, l);
    }

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        type = usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(def, block, l, type);
        r = eval_cast(def, block, r, type);
    } else if (is_pointer(l.type)) {
        if (is_pointer(r.type)) {
            if (!is_compatible(l.type, r.type) &&
                !(is_void(l.type->next) && size_of(r.type->next)) &&
                !(is_void(r.type->next) && size_of(l.type->next)))
            {
                warning("Comparison between incompatible types '%t' and '%t'.",
                    l.type, r.type);
            }
        } else if (!is_nullptr(r)) {
            warning("Comparison between pointer and non-zero integer.");
        }

        /* Left operand has the most specific type, cast to that to have
           the same type on each side. */
        r = eval_cast(def, block, r, l.type);
    } else {
        error("Illegal comparison between types '%t' and '%t'.",
            l.type, r.type);
        exit(1);
    }

    assert(type_equal(l.type, r.type));
    return (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
        ? eval_immediate_compare(l.type, l, ==, r)
        : evaluate(def, block, IR_OP_EQ, &basic_type__int, l, r);
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

/* Intermediate language is simplified to handle only greater than (>)
 * and greater than or equal (>=).
 */
static struct var eval_cmp_ge(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    type = common_compare_type(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);

    return (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
        ? eval_immediate_compare(l.type, l, >=, r)
        : evaluate(def, block, IR_OP_GE, &basic_type__int, l, r);
}

static struct var eval_cmp_gt(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    type = common_compare_type(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);

    return (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
        ? eval_immediate_compare(l.type, l, >, r)
        : evaluate(def, block, IR_OP_GT, &basic_type__int, l, r);
}

static struct var eval_or(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise or must have integer type.");
        exit(1);
    }

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);

    return (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
        ? eval_integer_immediate(type, l, |, r)
        : evaluate(def, block, IR_OP_OR, type, l, r);
}

static struct var eval_xor(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise xor must have integer type.");
        exit(1);
    }

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);

    return (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
        ? eval_integer_immediate(type, l, ^, r)
        : evaluate(def, block, IR_OP_XOR, type, l, r);
}

static struct var eval_and(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise and must have integer type.");
        exit(1);
    }

    type = usual_arithmetic_conversion(l.type, r.type);
    l = eval_cast(def, block, l, type);
    r = eval_cast(def, block, r, type);

    return (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
        ? eval_integer_immediate(type, l, &, r)
        : evaluate(def, block, IR_OP_AND, type, l, r);
}

static struct var eval_shiftl(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Shift operands must have integer type.");
        exit(1);
    }

    type = promote_integer(l.type);
    return (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
        ? eval_integer_immediate(type, l, <<, r)
        : evaluate(def, block, IR_OP_SHL, type, l, r);
}

static struct var eval_shiftr(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type;

    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Shift operands must have integer type.");
        exit(1);
    }

    type = promote_integer(l.type);
    return (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
        ? eval_integer_immediate(type, l, >>, r)
        : evaluate(def, block, IR_OP_SHR, type, l, r);
}

static struct var eval_not(
    struct definition *def,
    struct block *block,
    struct var var)
{
    const struct typetree *type;

    if (!is_integer(var.type)) {
        error("Bitwise complement operand must have integer type.");
        exit(1);
    }

    type = promote_integer(var.type);
    if (var.kind == IMMEDIATE) {
        if (is_signed(type)) {
            var = imm_signed(type, ~var.imm.i);
        } else {
            assert(is_unsigned(type));
            var = imm_unsigned(type, ~var.imm.u);
        }
    } else {
        var = evaluate(def, block, IR_NOT, type, var);
    }

    return var;
}

/* Convert variables of type ARRAY or FUNCTION to addresses when used
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

            /* Immediate references to strings retain the same
             * representation, only changing type from array to
             * pointer. */
            var.type = type_init(T_POINTER, var.type->next);
        } else {
            /* References to arrays decay into pointer to the first
             * element. Change type before doing regular address
             * evaluation, this way backend does not need to handle
             * address of array in any special way. The memory location
             * represented by var can also be seen as the first
             * element. */
            var.type = var.type->next;
            var = eval_addr(def, block, var);
        }
    } else if (is_field(var)) {
        assert(
            type_equal(var.type, &basic_type__int) ||
            type_equal(var.type, &basic_type__unsigned_int));

        /* Bit field is loaded, and if needed sign extended, into a full
           width integer value. Set width = 0 to make nested calls to
           eval methods not recursively call rvalue. */
        if (var.width < size_of(&basic_type__int) * 8) {
            bits = var.width;
            var.width = 0;
            var = eval_and(def, block, var, var_int((1 << bits) - 1));
            if (is_signed(var.type)) {
                bits = size_of(var.type) * 8 - bits;
                var = eval_shiftl(def, block, var, var_int(bits));
                var = eval_shiftr(def, block, var, var_int(bits));
            } else {
                /* Fields of unsigned type are promoted to signed int
                   if the signed type can represent all values of the
                   unsigned type. */
                var.type = &basic_type__int;
            }
        } else {
            assert(var.width == size_of(&basic_type__int) * 8);
        }
    }

    assert(!var.width);
    return var;
}

struct var eval_expr(
    struct definition *def,
    struct block *block,
    enum optype optype,
    struct var l, ...)
{
    va_list args;
    struct var r;

    l = rvalue(def, block, l);
    if (OPERAND_COUNT(optype) == 3) {
        va_start(args, l);
        r = va_arg(args, struct var);
        r = rvalue(def, block, r);
        va_end(args);
    }

    switch (optype) {
    case IR_NOT:    return eval_not(def, block, l);
    case IR_OP_MOD: return eval_mod(def, block, l, r);
    case IR_OP_MUL: return eval_mul(def, block, l, r);
    case IR_OP_DIV: return eval_div(def, block, l, r);
    case IR_OP_ADD: return eval_add(def, block, l, r);
    case IR_OP_SUB: return eval_sub(def, block, l, r);
    case IR_OP_EQ:  return eval_eq(def, block, l, r);
    case IR_OP_GE:  return eval_cmp_ge(def, block, l, r);
    case IR_OP_GT:  return eval_cmp_gt(def, block, l, r);
    case IR_OP_AND: return eval_and(def, block, l, r);
    case IR_OP_XOR: return eval_xor(def, block, l, r);
    case IR_OP_OR:  return eval_or(def, block, l, r);
    case IR_OP_SHL: return eval_shiftl(def, block, l, r);
    default:
        assert(optype == IR_OP_SHR);
        return eval_shiftr(def, block, l, r);
    }
}

struct var eval_addr(
    struct definition *def,
    struct block *block,
    struct var var)
{
    struct var tmp;

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
        /* Address of string literal can be done without evaluation,
           just decay the variable to pointer. */
        if (is_array(var.type)) {
            var = rvalue(def, block, var);
        }
        break;
    case DIRECT:
        /* Address of *(&sym + offset) is available directly by setting
           flag, resulting in (&sym + offset). No special handling of
           offset needed. */
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
            /* Address of *(const + offset) is just the constant.
               Convert to immediate, adding the extra offset. */
            var.kind = IMMEDIATE;
            var.type = type_init(T_POINTER, var.type);
            var.imm.u += var.offset;
            var.offset = 0;
            var.lvalue = 0;
        } else {
            /* Address of *(sym + offset) is not *(&sym + offset), so
               not possible to just convert to DIRECT. Offset must be
               applied after converting to direct pointer. */
            assert(is_pointer(&var.symbol->type));
            tmp = var_direct(var.symbol);
            if (var.offset) {
                tmp = eval_cast(def, block, tmp,
                    type_init(T_POINTER, &basic_type__char));
                tmp = eval_expr(
                    def, block, IR_OP_ADD, tmp, var_int(var.offset));
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
        /* Dereferencing *(sym + offset) must evaluate pointer into a
           new temporary, before marking that as DEREF var. */
        var = eval_assign(def, block, create_var(def, var.type), var);
        break;
    case DIRECT:
        if (var.offset != 0 || !is_pointer(&var.symbol->type)) {
            /* Cannot immediately dereference a pointer which is at a
               direct offset from another symbol. Also, pointers that
               are the result of indexing into a structure must be
               evaluated, as DEREF variables assume symbol to be of
               pointer type. */
            var = eval_assign(def, block, create_var(def, var.type), var);
        }
        break;
    case ADDRESS:
        /* Dereferencing (&sym + offset) is a DIRECT reference to sym,
           with the same offset. */
        var.kind = DIRECT;
        var.type = type_deref(var.type);
        var.lvalue = 1;
        return var;
    case IMMEDIATE:
        /* Dereferencing constant which has been cast to pointer. This
           is a special case of deref, identified by symbol being NULL.
           Handled in backend. */
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

struct var eval_assign(
    struct definition *def,
    struct block *block,
    struct var target,
    struct var var)
{
    enum qualifier cv;

    if (!target.lvalue) {
        error("Target of assignment must be l-value.");
        exit(1);
    }

    if (!is_array(target.type)) {
        var = rvalue(def, block, var);
    }

    if (is_array(target.type)) {
        /* Special case char [] = string in initializers. In this case
         * we do nothing here, but handle it in backend. */
        if (!type_equal(target.type, var.type) || var.kind != IMMEDIATE) {
            error("Invalid initializer assignment, was %s :: %t = %t.",
                str_raw(target.symbol->name),
                target.type,
                var.type);
            exit(1);
        }
        assert(var.symbol);
        assert(var.symbol->symtype == SYM_STRING_VALUE);
    } else if (is_pointer(target.type) && is_pointer(var.type)) {
        cv = target.type->next->qualifier | var.type->next->qualifier;
        if (!is_compatible(target.type, var.type)) {
            if (!(is_nullptr(var)
                || (is_void(target.type->next) && is_object(var.type->next))
                || (is_object(target.type->next) && is_void(var.type->next))))
            {
                warning("Incompatible type in pointer assignment; %t = %t.",
                    target.type,
                    var.type);
            }
        }
        if (!is_nullptr(var) && cv != target.type->next->qualifier) {
            warning("Target of assignment lacks qualifiers; %t = %t.",
                target.type,
                var.type);
        }
    } else if (is_pointer(target.type) && is_integer(var.type)) {
        if (!is_nullptr(var)) {
            warning("Assigning non-zero number to pointer.",
                target.type,
                var.type);
        }
    } else if (
        /* The left operand has atomic, qualified, or unqualified
           arithmetic type, and the right has arithmetic type. */
        !(is_arithmetic(target.type) && is_arithmetic(var.type)) &&
        /* The left operand has an atomic, qualified, or unqualified
           version of a structure or union type compatible with the
           type of the right. */
        !(is_struct_or_union(target.type)
            && is_compatible(target.type, var.type)))
    {
        error("Incompatible operands to assignment expression, %s :: %t = %t.",
            str_raw(target.symbol->name), target.type, var.type);
        exit(1);
    }

    /* Assignment has implicit conversion for basic types when
       evaluating the IR operation, meaning var will be sign extended
       to size of target.type. */
    emit_ir(block, IR_ASSIGN, target, var);
    target.lvalue = 0;

    return target;
}

struct var eval_call(
    struct definition *def,
    struct block *block,
    struct var var)
{
    struct var res;
    const struct typetree *type = var.type;

    /* In principle, the type of the expression being called should
     * always be pointer to function. Accept call also to direct
     * reference of function type, to avoid a lot of explicit address
     * evaluations. This special case must be handled in backend. */
    if (!is_function(var.type)) {
        assert(is_pointer(var.type) && is_function(var.type->next));
        type = var.type->next;
    } else
        assert(is_function(var.type));

    if (is_void(type->next)) {
        res = var_void();
    } else {
        res = create_var(def, type->next);
    }

    emit_ir(block, IR_CALL, res, var);
    return res;
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
    return eval_assign(def, block, cpy, var);
}

struct var eval_return(struct definition *def, struct block *block)
{
    const struct typetree *type = def->symbol->type.next;
    assert(!is_function(type));
    assert(!is_array(type));
    assert(!is_void(type));

    block->expr = rvalue(def, block, block->expr);
    if (!type_equal(type, block->expr.type)) {
        block->expr =
            eval_assign(def, block, create_var(def, type), block->expr);
    }

    block->has_return_value = 1;
    return block->expr;
}

struct var eval_cast(
    struct definition *def,
    struct block *block,
    struct var var,
    const struct typetree *type)
{
    var = rvalue(def, block, var);
    if (is_void(type)) {
        return var_void();
    } else if (!is_scalar(var.type) || !is_scalar(type)) {
        error(
            "Invalid type parameters to cast expression, "
            "cannot convert from %t to %t.",
            var.type, type);
        exit(1);
    }

    /* All immediate conversions must be evaluated compile time. Also
       handle conversion which can be done by reinterpreting memory. */
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
            var.imm.u =
                (is_float(var.type)) ? (unsigned long) var.imm.f :
                (is_double(var.type)) ? (unsigned long) var.imm.d :
                (is_signed(var.type)) ? (unsigned long) var.imm.i : var.imm.u;
            if (size_of(type) < size_of(var.type)) {
                assert(size_of(type) == 4
                    || size_of(type) == 2
                    || size_of(type) == 1);
                var.imm.u &= (0xFFFFFFFFu >> ((4 - size_of(type)) * 8));
            }
        } else if (is_signed(type)) {
            var.imm.i =
                (is_float(var.type)) ? (long) var.imm.f :
                (is_double(var.type)) ? (long) var.imm.d :
                (is_unsigned(var.type)) ? (long) var.imm.u : var.imm.i;
            if (size_of(type) == 4) {
                var.imm.i = (int) var.imm.i;
            } else if (size_of(type) == 2) {
                var.imm.i = (short) var.imm.i;
            } else if (size_of(type) == 1) {
                var.imm.i = (signed char) var.imm.i;
            } else {
                assert(size_of(type) == 8);
            }
        } else {
            assert(0);
        }
        var.type = type;
    } else if (size_of(var.type) == size_of(type)
        && (is_pointer(var.type) || is_pointer(type)))
    {
        var.type = type;
    } else if (!type_equal(var.type, type)) {
        var = evaluate(def, block, IR_CAST, type, var);
    }

    assert(type_equal(var.type, type));
    return var;
}

struct var eval_conditional(
    struct definition *def,
    struct var a,
    struct block *b,
    struct block *c)
{
    struct var result;
    const struct typetree *t1, *t2, *type = NULL;

    if (!is_scalar(a.type)) {
        error("Conditional must be of scalar type.");
    }

    b->expr = rvalue(def, b, b->expr);
    c->expr = rvalue(def, c, c->expr);

    t1 = b->expr.type;
    t2 = c->expr.type;

    if (is_arithmetic(t1) && is_arithmetic(t2)) {
        type = usual_arithmetic_conversion(t1, t2);
    } else if (
        (is_void(t1) && is_void(t2)) ||
        (is_struct_or_union(t1) && type_equal(t1, t2)) ||
        (is_pointer(t1) && is_pointer(t2) && is_compatible(t1, t2)) ||
        (is_pointer(t1) && is_nullptr(c->expr)))
    {
        type = t1;
    } else if (is_pointer(t2) && is_nullptr(b->expr)) {
        type = t2;
    } else {
        /* The rules are more complex than this, revisit later. */
        error("Unsupported types in conditional operator.");
        exit(1);
    }

    if (is_void(type)) {
        result = var_void();
    } else {
        if (a.kind == IMMEDIATE) {
            result = (a.imm.i) ? b->expr : c->expr;
        } else {
            result = create_var(def, type);
            b->expr = eval_assign(def, b, result, b->expr);
            c->expr = eval_assign(def, c, result, c->expr);
            result.lvalue = 0;
        }
    }

    return result;
}

static struct block *eval_logical_expression(
    struct definition *def,
    int is_and,
    struct block *left,
    struct block *right_top,
    struct block *right)
{
    struct block
        *t = cfg_block_init(def),
        *f = cfg_block_init(def),
        *r = cfg_block_init(def);

    /* Result is integer type, assigned in true and false branches to
     * numeric constant 1 or 0. */
    r->expr = create_var(def, &basic_type__int);
    left->expr = eval_expr(def, left, IR_OP_EQ, left->expr, var_int(0));
    if (is_and) {
        left->jump[0] = right_top;
        left->jump[1] = f;
    } else {
        left->jump[0] = t;
        left->jump[1] = right_top;
    }

    right->expr = eval_expr(def, right, IR_OP_EQ, right->expr, var_int(0));
    right->jump[0] = t;
    right->jump[1] = f;

    r->expr.lvalue = 1;
    eval_assign(def, t, r->expr, var_int(1));
    eval_assign(def, f, r->expr, var_int(0));
    r->expr.lvalue = 0;

    t->jump[0] = r;
    f->jump[0] = r;
    return r;
}

struct block *eval_logical_or(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right)
{
    if (!is_scalar(left->expr.type) || !is_scalar(right->expr.type)) {
        error("Operands to logical or must be of scalar type.");
        exit(1);
    }

    if (left->expr.kind == IMMEDIATE && right->expr.kind == IMMEDIATE) {
        left->expr = var_int(
            is_immediate_true(left->expr) || is_immediate_true(right->expr));
    } else if (is_immediate_true(left->expr)) {
        left->expr = var_int(1);
    } else if (is_immediate_false(left->expr)) {
        left->jump[0] = right_top;
        /* Checking for value != 0 is expressed as (0 == (e == 0)). */
        right->expr = eval_expr(def, right, IR_OP_EQ, right->expr, var_int(0));
        right->expr = eval_expr(def, right, IR_OP_EQ, right->expr, var_int(0));
        left = right;
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
    if (!is_scalar(left->expr.type) || !is_scalar(right->expr.type)) {
        error("Operands to logical and must be of scalar type.");
        exit(1);
    }

    if (left->expr.kind == IMMEDIATE && right->expr.kind == IMMEDIATE) {
        left->expr = var_int(
            is_immediate_true(left->expr) && is_immediate_true(right->expr));
    } else if (is_immediate_false(left->expr)) {
        left->expr = var_int(0);
    } else if (is_immediate_true(left->expr)) {
        left->jump[0] = right_top;
        /* Checking for value != 0 is expressed as (0 == (e == 0)). */
        right->expr = eval_expr(def, right, IR_OP_EQ, right->expr, var_int(0));
        right->expr = eval_expr(def, right, IR_OP_EQ, right->expr, var_int(0));
        left = right;
    } else {
        left = eval_logical_expression(def, 1, left, right_top, right);
    }

    return left;
}

void param(struct definition *def, struct block *block, struct var arg)
{
    emit_ir(block, IR_PARAM, rvalue(def, block, arg));
}

struct var eval__builtin_va_start(struct block *block, struct var arg)
{
    emit_ir(block, IR_VA_START, arg);
    return var_void();
}

struct var eval__builtin_va_arg(
    struct definition *def,
    struct block *block,
    struct var arg,
    const struct typetree *type)
{
    return evaluate(def, block, IR_VA_ARG, type, arg);
}
