#include "eval.h"
#include "declaration.h"
#include "parse.h"
#include "symtab.h"
#include "type.h"
#include <lacc/cli.h>
#include <lacc/ir.h>

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>

static int is_nullptr(struct var val)
{
    return 
        (is_integer(val.type) || is_pointer(val.type)) &&
        (val.kind == IMMEDIATE && !val.imm.u);
}

static int is_string(struct var val)
{
    return
        val.kind == IMMEDIATE && val.symbol &&
        val.symbol->symtype == SYM_STRING_VALUE;
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
    struct symbol *temp = sym_create_tmp(type);
    struct var res = var_direct(temp);

    list_push_back(&def->locals, temp);
    res.lvalue = 1;
    return res;
}

struct var var_direct(const struct symbol *sym)
{
    struct var var = {0};

    var.type = &sym->type;
    var.symbol = sym;

    switch (sym->symtype) {
    case SYM_ENUM_VALUE:
        var.kind = IMMEDIATE;
        var.imm.i = sym->enum_value;
        break;
    case SYM_STRING_VALUE:
        var.kind = IMMEDIATE;
        break;
    default:
        assert(sym->symtype != SYM_LABEL);
        var.kind = DIRECT;
        var.lvalue = sym->name[0] != '.';
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
    const struct typetree *type = usual_arithmetic_conversion(l.type, r.type);

    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        return var_int(l.imm.i * r.imm.i);
    }

    return evaluate(def, block, IR_OP_MUL, type, l, r);
}

static struct var eval_div(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type = usual_arithmetic_conversion(l.type, r.type);

    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        return var_int(l.imm.i / r.imm.i);
    }

    return evaluate(def, block, IR_OP_DIV, type, l, r);
}

static struct var eval_mod(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    const struct typetree *type = usual_arithmetic_conversion(l.type, r.type);
    if (!is_integer(type)) {
        error("Operands of modulo operator must be of integer type.");
        exit(1);
    }

    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        return var_int(l.imm.i % r.imm.i);
    }

    return evaluate(def, block, IR_OP_MOD, type, l, r);
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
            l = var_int(l.imm.i + r.imm.i);
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
        /* Special case for string literal + constant. This is
         * represented as an immediate with offset. */
        if (is_string(l) && r.kind == IMMEDIATE && is_integer(r.type)) {
            l.offset += r.imm.i;
        }
        /* Evaluate unless r is immediate zero. */
        else if (r.kind != IMMEDIATE || r.imm.i) {
            r = eval_expr(def, block, IR_OP_MUL, var_int(size_of(l.type->next)), r);
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
    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = var_int(l.imm.i - r.imm.i);
        } else {
            const struct typetree *type
                = usual_arithmetic_conversion(l.type, r.type);
            l = eval_cast(def, block, l, type);
            r = eval_cast(def, block, r, type);
            l = evaluate(def, block, IR_OP_SUB, type, l, r);
        }
    } else if (is_pointer(l.type) && is_integer(r.type)) {
        if (!size_of(l.type->next)) {
            error("Pointer arithmetic on incomplete type.");
            exit(1);
        }
        /* Special case for string literal - constant. This is
         * represented as an immediate with offset. */
        if (is_string(l) && r.kind == IMMEDIATE && is_integer(r.type)) {
            l.offset -= r.imm.i;
        }
        /* Evaluate unless r is immediate zero. */
        else if (r.kind != IMMEDIATE || r.imm.i) {
            r = eval_expr(def, block, IR_OP_MUL, var_int(size_of(l.type->next)), r);
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
    if (!is_pointer(l.type)) {
        struct var tmp = l;
        l = r;
        r = tmp;
    }

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        const struct typetree *type =
            usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(def, block, l, type);
        r = eval_cast(def, block, r, type);
    } else if (is_pointer(l.type)) {
        /* Covers pointer to compatible types, including (void *, void *), 
         * pointer to object type and void *, pointer and null constant. */
        if (is_pointer(r.type)) {
            if (!is_compatible(l.type, r.type) &&
                !(is_void(l.type->next) && size_of(r.type->next)) &&
                !(is_void(r.type->next) && size_of(l.type->next)))
            {
                error("Comparison between incompatible types '%t' and '%t'.",
                    l.type, r.type);
                exit(1);
            }
        } else {
            if (!is_integer(r.type) || r.kind != IMMEDIATE || r.imm.i != 0) {
                error("Numerical comparison must be null constant.");
                exit(1);
            }
        }
    } else {
        error("Illegal comparison between types '%t' and '%t'.",
            l.type, r.type);
        exit(1);
    }

    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        return var_int(l.imm.i == r.imm.i);
    }

    return evaluate(def, block, IR_OP_EQ, &basic_type__int, l, r);
}

static int validate_cmp_args(
    struct definition *def,
    struct block *block,
    struct var *l,
    struct var *r)
{
    if (is_arithmetic(l->type) && is_arithmetic(r->type)) {
        const struct typetree *type =
            usual_arithmetic_conversion(l->type, r->type);

        *l = eval_cast(def, block, *l, type);
        *r = eval_cast(def, block, *r, type);
        return 0;
    }

    return 
        !(is_pointer(l->type) && is_pointer(r->type) &&
            size_of(l->type->next) &&
            size_of(l->type->next) == size_of(r->type->next));
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
    if (validate_cmp_args(def, block, &l, &r)) {
        error("Invalid operands in relational expression.");
        exit(1);
    }

    return evaluate(def, block, IR_OP_GE, &basic_type__int, l, r);
}

static struct var eval_cmp_gt(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    if (validate_cmp_args(def, block, &l, &r)) {
        error("Invalid operands in relational expression.");
        exit(1);
    }

    return evaluate(def, block, IR_OP_GT, &basic_type__int, l, r);
}

static struct var eval_or(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise or must have integer type.");
        exit(1);
    }

    return
        (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
            ? var_int(l.imm.i | r.imm.i)
            : evaluate(def, block, IR_OP_OR,
                usual_arithmetic_conversion(l.type, r.type), l, r);
}

static struct var eval_xor(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise xor must have integer type.");
        exit(1);
    }

    return
        (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
            ? var_int(l.imm.i ^ r.imm.i)
            : evaluate(def, block, IR_OP_XOR,
                usual_arithmetic_conversion(l.type, r.type), l, r);
}

static struct var eval_and(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Operands to bitwise and must have integer type.");
        exit(1);
    }

    return
        (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
            ? var_int(l.imm.i & r.imm.i)
            : evaluate(def, block, IR_OP_AND,
                usual_arithmetic_conversion(l.type, r.type), l, r);
}

static struct var eval_shiftl(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Shift operands must have integer type.");
        exit(1);
    }

    return
        (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
            ? var_int(l.imm.i << r.imm.i)
            : evaluate(def, block, IR_OP_SHL, promote_integer(l.type), l, r);
}

static struct var eval_shiftr(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r)
{
    if (!is_integer(l.type) || !is_integer(r.type)) {
        error("Shift operands must have integer type.");
        exit(1);
    }

    return
        (l.kind == IMMEDIATE && r.kind == IMMEDIATE)
            ? var_int(l.imm.i >> r.imm.i)
            : evaluate(def, block, IR_OP_SHR, promote_integer(l.type), l, r);
}

static struct var eval_not(
    struct definition *def,
    struct block *block,
    struct var var)
{
    if (!is_integer(var.type)) {
        error("Bitwise complement operand must have integer type.");
        exit(1);
    }

    return
        (var.kind == IMMEDIATE)
            ? var_int(~var.imm.i)
            : evaluate(def, block, IR_NOT, promote_integer(var.type), var);
}

/* Convert variables of type ARRAY or FUNCTION to addresses when used in
 * expressions. 'array of T' is converted (decay) to pointer to T. Not
 * the same as taking the address of an array, which would give 'pointer
 * to array of T'.
 */
static struct var array_or_func_to_addr(
    struct definition *def,
    struct block *block,
    struct var var)
{
    if (is_array(var.type)) {
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
    } else if (is_function(var.type)) {
        var = eval_addr(def, block, var);
    }

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

    l = array_or_func_to_addr(def, block, l);
    if (OPERAND_COUNT(optype) == 3) {
        va_start(args, l);
        r = va_arg(args, struct var);
        r = array_or_func_to_addr(def, block, r);
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

    switch (var.kind) {
    case IMMEDIATE:
        if (!var.symbol || var.symbol->symtype != SYM_STRING_VALUE) {
            error("Cannot take address of immediate of type '%t'.", var.type);
            exit(1);
        }
        /* Address of string literal can be done without evaluation,
         * just decay the variable to pointer. */
        if (is_array(var.type))
            var = array_or_func_to_addr(def, block, var);
        assert(is_pointer(var.type));
        break;
    case DIRECT:
        var =
            evaluate(def, block, IR_ADDR, type_init(T_POINTER, var.type), var);
        break;
    case DEREF:
        assert(is_pointer(&var.symbol->type));
        tmp = var_direct(var.symbol);
        if (var.offset) {
            /* Address of *(sym + offset) is (sym + offset), but
             * without pointer arithmetic applied in addition. Cast to
             * char pointer temporarily to avoid trouble calling
             * eval_expr. */
            tmp = eval_cast(def, block, tmp,
                type_init(T_POINTER, &basic_type__char));
            tmp = eval_expr(def, block, IR_OP_ADD, tmp, var_int(var.offset));
        }
        tmp.type = type_init(T_POINTER, var.type);
        var = tmp;
        break;
    }

    return var;
}

struct var eval_deref(
    struct definition *def,
    struct block *block,
    struct var var)
{
    struct var ptr;

    var = array_or_func_to_addr(def, block, var);
    if (var.kind == DEREF) {
        assert(is_pointer(&var.symbol->type));

        /* Cast to char pointer temporarily to avoid pointer arithmetic
         * calling eval_expr. No actual evaluation is performed. */
        ptr = var_direct(var.symbol);
        ptr = eval_cast(def, block, ptr, type_init(T_POINTER, &basic_type__char));
        ptr = eval_expr(def, block, IR_OP_ADD, ptr, var_int(var.offset));
        ptr.type = &var.symbol->type;

        var = evaluate(def, block, IR_DEREF, var.type, ptr);
    } else if (
        var.kind == DIRECT &&
        (var.offset || !is_pointer(&var.symbol->type)))
    {
        /* Cannot immediately dereference a pointer which is at a direct
         * offset from another symbol. Also, pointers that are the
         * result of indexing into a structure must be evaluated, as
         * DEREF variables assume symbol to be of pointer type. */
        var = eval_assign(def, block, create_var(def, var.type), var);
    }

    assert(var.kind == DIRECT && !var.offset);
    assert(is_pointer(var.type) && is_pointer(&var.symbol->type));

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
    if (!target.lvalue) {
        error("Target of assignment must be l-value.");
        exit(1);
    }

    if (!is_array(target.type)) {
        var = array_or_func_to_addr(def, block, var);
    }

    if (is_array(target.type)) {
        /* Special case char [] = string in initializers. In this case
         * we do nothing here, but handle it in backend. */
        if (!type_equal(target.type, var.type) || var.kind != IMMEDIATE) {
            error("Invalid initializer assignment, was %s :: %t = %t.",
                target.symbol->name,
                target.type,
                var.type);
            exit(1);
        }

        assert(var.symbol);
        assert(var.symbol->symtype == SYM_STRING_VALUE);
    } else if (
        /* The left operand has atomic, qualified, or unqualified
         * arithmetic type, and the right has arithmetic type. */
        !(is_arithmetic(target.type) && is_arithmetic(var.type)) &&
        /* The left operand has an atomic, qualified, or unqualified
         * version of a structure or union type compatible with the
         * type of the right. */
        !(is_struct_or_union(target.type)
            && is_compatible(target.type, var.type)) &&
        /* The left operand has atomic, qualified, or unqualified
         * pointer type, and (considering the type the left operand
         * would have after lvalue conversion) both operands are
         * pointers to qualified or unqualified versions of compatible
         * types, and the type pointed to by the left has all the
         * qualifiers of the type pointed to by the right. */
        !(is_pointer(target.type) && is_pointer(var.type)
            && is_compatible(target.type->next, var.type->next)
            && (target.type->next->qualifier | var.type->next->qualifier)
                == target.type->next->qualifier) &&
        /* The left operand has atomic, qualified, or unqualified
         * pointer type, and (considering the type the left operand
         * would have after lvalue conversion) one operand is a pointer
         * to an object type, and the other is a pointer to a qualified
         * or unqualified version of void, and the type pointed to by
         * the left has all the qualifiers of the type pointed to by
         * the right. */
        !(is_pointer(target.type) && is_pointer(var.type)
            && ((is_void(target.type->next) && is_object(var.type->next))
                || (is_object(target.type->next) && is_void(var.type->next)))
            && (target.type->next->qualifier | var.type->next->qualifier)
                == target.type->next->qualifier) &&
        /* The left operand is an atomic, qualified, or unqualified
         * pointer, and the right is a null pointer constant. */
        !(is_pointer(target.type) && is_nullptr(var)))
    {
        error("Incompatible operands to assignment expression, %s :: %t = %t.",
            target.symbol->name, target.type, var.type);
        exit(1);
    }

    /* Assignment has implicit conversion for basic types when
     * evaluating the IR operation, meaning var will be sign extended
     * to size of target.type. */
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
    struct var copy = create_var(def, var.type);
    return eval_assign(def, block, copy, var);
}

struct var eval_return(struct definition *def, struct block *block)
{
    const struct typetree *type = def->symbol->type.next;
    assert(!is_void(type));

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
    if (is_void(type)) {
        var = var_void();
    } else if (is_scalar(var.type) && is_scalar(type)) {
        if (var.kind == IMMEDIATE || size_of(var.type) == size_of(type)) {
            var.type = type;
        } else {
            var = evaluate(def, block, IR_CAST, type, var);
        }
    } else {
        error(
            "Invalid type parameters to cast expression, "
            "cannot convert from %t to %t.",
            var.type, type);
        exit(1);
    }

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

    b->expr = array_or_func_to_addr(def, b, b->expr);
    c->expr = array_or_func_to_addr(def, c, c->expr);

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

    result = create_var(def, type);
    b->expr = eval_assign(def, b, result, b->expr);
    c->expr = eval_assign(def, c, result, c->expr);
    result.lvalue = 0;

    if (a.kind == IMMEDIATE) {
        return (a.imm.i) ? b->expr : c->expr;
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
    } else if (left->expr.kind == IMMEDIATE && right->expr.kind == IMMEDIATE) {
        left->expr = var_int(left->expr.imm.i || right->expr.imm.i);
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
    } else if (left->expr.kind == IMMEDIATE && right->expr.kind == IMMEDIATE) {
        left->expr = var_int(left->expr.imm.i && right->expr.imm.i);
    } else {
        left = eval_logical_expression(def, 1, left, right_top, right);
    }

    return left;
}

void param(struct definition *def, struct block *block, struct var arg)
{
    emit_ir(block, IR_PARAM, array_or_func_to_addr(def, block, arg));
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
