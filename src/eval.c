#include "eval.h"
#include "error.h"
#include "string.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

/* Current declaration from parser. Need to add symbols to list whenever new
 * temporary ones are created during evaluation.
 */
extern struct decl *decl;

struct var var_direct(const struct symbol *sym)
{
    struct var var = {0};

    var.type = sym->type;
    if (sym->symtype == SYM_ENUM_VALUE) {
        var.kind = IMMEDIATE;
        var.value.integer = sym->enum_value;
    } else {
        var.kind = DIRECT;
        var.symbol = sym;
        var.lvalue = sym->name[0] != '.';
    }

    return var;
}

struct var var_string(const char *str)
{
    struct var var = {0};
    unsigned int length = strlen(str) + 1;

    var.kind = IMMEDIATE;
    var.type = type_init_string(length);
    var.string = str;
    return var;
}

struct var var_int(int value)
{
    struct var var = {0};

    var.kind = IMMEDIATE;
    var.type = type_init_integer(4);
    var.value.integer = value;
    return var;
}

struct var var_zero(int size)
{
    struct var var = {0};
    var.kind = IMMEDIATE;
    var.type = type_init_integer(size);
    var.value.integer = 0;
    return var;
}

static struct var var_void()
{
    struct var var = {0};

    var.kind = IMMEDIATE;
    var.type = type_init_void();
    return var;
}

static int is_nullptr(struct var val)
{
    return 
        (val.type->type == INTEGER || val.type->type == POINTER) &&
        (val.kind == IMMEDIATE && !val.value.integer);
}

static struct var
evaluate(struct block *block, enum optype op, const struct typetree *t, ...)
{
    va_list args;
    struct var res, l, r;
    struct symbol *sym;
    struct op irop;

    va_start(args, t);
    l = va_arg(args, struct var);
    if (NOPERANDS(op) == 2) {
        r = va_arg(args, struct var);
    }

    va_end(args);
    sym = sym_temp(&ns_ident, t);
    res = var_direct(sym);
    assert(res.kind == DIRECT);

    sym_list_push_back(&decl->locals, sym);

    irop.type = op;
    irop.a = res;
    irop.b = l;
    irop.c = r;
    cfg_ir_append(block, irop);
    return res;
}

/* 6.5.5 Multiplicative Operators.
 */
static struct var eval_expr_mul(struct block *block, struct var l, struct var r)
{
    const struct typetree *type = usual_arithmetic_conversion(l.type, r.type);

    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        return var_int(l.value.integer * r.value.integer);
    }

    return evaluate(block, IR_OP_MUL, type, l, r);
}

static struct var eval_expr_div(struct block *block, struct var l, struct var r)
{
    const struct typetree *type = usual_arithmetic_conversion(l.type, r.type);

    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        return var_int(l.value.integer / r.value.integer);
    }

    return evaluate(block, IR_OP_DIV, type, l, r);
}

static struct var eval_expr_mod(struct block *block, struct var l, struct var r)
{
    const struct typetree *type = usual_arithmetic_conversion(l.type, r.type);
    if (!is_integer(type)) {
        error("Operands of mod must be integer type.");
    }

    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        return var_int(l.value.integer % r.value.integer);
    }

    return evaluate(block, IR_OP_MOD, type, l, r);
}

/* 6.5.6 Additive Operators.
 */
static struct var eval_expr_add(struct block *block, struct var l, struct var r)
{
    const struct typetree *type;

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        type = usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(block, l, type);
        r = eval_cast(block, r, type);
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = var_int(l.value.integer + r.value.integer);
        } else {
            l = evaluate(block, IR_OP_ADD, type, l, r);
        }
    } else if (is_integer(l.type) && is_pointer(r.type)) {
        /* Make sure pointer is left, and integer right. */
        l = eval_expr_add(block, r, l);
    } else if (is_pointer(l.type) && is_integer(r.type)) {
        if (!l.type->next->size) {
            error("Pointer arithmetic on incomplete type.");
            exit(1);
        }
        /* Special case for string literal + constant. This is represented as an
         * immediate with offset. */
        if (l.kind == IMMEDIATE && l.string &&
            r.kind == IMMEDIATE && is_integer(r.type))
        {
            l.offset += r.value.integer;
        }
        /* No evaluation if r is zero. */
        else if (r.kind != IMMEDIATE || r.value.integer) {
            r = eval_expr(block, IR_OP_MUL, var_int(l.type->next->size), r);
            l = evaluate(block, IR_OP_ADD, l.type, l, r);
        }
    } else {
        error("Incompatible arguments to addition operator, was %s and %s.",
            typetostr(l.type), typetostr(r.type));
    }

    return l;
}

static struct var eval_expr_sub(struct block *block, struct var l, struct var r)
{
    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = var_int(l.value.integer - r.value.integer);
        } else {
            const struct typetree *type
                = usual_arithmetic_conversion(l.type, r.type);
            l = eval_cast(block, l, type);
            r = eval_cast(block, r, type);
            l = evaluate(block, IR_OP_SUB, type, l, r);
        }
    } else if (is_pointer(l.type) && is_integer(r.type)) {
        if (!l.type->next->size) {
            error("Pointer arithmetic on incomplete type.");
            exit(1);
        }
        /* Special case for string literal - constant. This is represented as an
         * immediate with offset. */
        if (l.kind == IMMEDIATE && l.string &&
            r.kind == IMMEDIATE && is_integer(r.type))
        {
            l.offset -= r.value.integer;
        }
        /* No evaluation if r is zero. */
        else if (r.kind != IMMEDIATE || r.value.integer) {
            r = eval_expr(block, IR_OP_MUL, var_int(l.type->next->size), r);
            l = evaluate(block, IR_OP_SUB, l.type, l, r);
        }
    } else if (is_pointer(l.type) && is_pointer(r.type)) {
        struct typetree *type = type_init_unsigned(8);

        if (!l.type->next->size || l.type->next->size != r.type->next->size) {
            error("Referenced type is incomplete.");
        }

        /* Result is ptrdiff_t, which will be signed 64 bit integer. Reinterpret
         * cast both pointers to unsigned long (no codegen), and store result as
         * signed long. Then divide by element size to get diff. */
        l = eval_cast(block, l, type);
        r = eval_cast(block, r, type);
        l = eval_expr(block, IR_OP_SUB, l, r);
        l = eval_expr(block, IR_OP_DIV, l, var_int(r.type->next->size));
    } else {
        error("Incompatible arguments to subtraction operator, was %s and %s.",
            typetostr(l.type), typetostr(r.type));
    }

    return l;
}

/* 6.5.9 Equality operators.
 */
static struct var eval_expr_eq(struct block *block, struct var l, struct var r)
{
    /* Normalize by preferring pointer on left side. */
    if (!is_pointer(l.type)) {
        struct var tmp = l;
        l = r;
        r = tmp;
    }

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        const struct typetree *type =
            usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(block, l, type);
        r = eval_cast(block, r, type);
    } else if (is_pointer(l.type)) {
        /* Covers pointer to compatible types, including (void *, void *), 
         * pointer to object type and void *, pointer and null constant. */
        if (is_pointer(r.type)) {
            if (   !is_compatible(l.type, r.type)
                && !(l.type->next->type == NONE && r.type->next->size)
                && !(r.type->next->type == NONE && l.type->next->size)
            ) {
                error("Comparison between incompatible types '%s' and '%s'.",
                    typetostr(l.type), typetostr(r.type));
                exit(1);
            }
        } else {
            if (  !is_integer(r.type)
                || r.kind != IMMEDIATE || r.value.integer != 0
            ) {
                error("Numerical comparison must be null constant.");
                exit(1);
            }
        }
    } else {
        error("Illegal comparison between types '%s' and '%s'.",
            typetostr(l.type), typetostr(r.type));
        exit(1);
    }

    return evaluate(block, IR_OP_EQ, type_init_integer(4), l, r);
}

static struct block *eval_logical_expression(
    int is_and, /* Logical AND if true, otherwise logical OR. */
    struct block *left,
    struct block *right_top,
    struct block *right)
{
    struct block
        *t = cfg_block_init(decl),
        *f = cfg_block_init(decl),
        *r = cfg_block_init(decl);

    /* Result is integer type, assigned in true and false branches to numeric
     * constant 1 or 0. */
    r->expr = var_direct(sym_temp(&ns_ident, type_init_integer(4)));
    sym_list_push_back(&decl->locals, (struct symbol *) r->expr.symbol);

    left->expr = eval_expr(left, IR_OP_EQ, left->expr, var_int(0));
    if (is_and) {
        left->jump[0] = right_top;
        left->jump[1] = f;
    } else {
        left->jump[0] = t;
        left->jump[1] = right_top;
    }

    right->expr = eval_expr(right, IR_OP_EQ, right->expr, var_int(0));
    right->jump[0] = t;
    right->jump[1] = f;

    r->expr.lvalue = 1;
    eval_assign(t, r->expr, var_int(1));
    eval_assign(f, r->expr, var_int(0));
    r->expr.lvalue = 0;

    t->jump[0] = r;
    f->jump[0] = r;
    return r;
}

/* 6.5.8 Relational operators. Simplified to handle only greater than (>) and
 * greater than or equal (>=), toggled by flag.
 */
static struct var
eval_expr_cmp(struct block *block, struct var l, struct var r, int e)
{
    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        const struct typetree *type =
            usual_arithmetic_conversion(l.type, r.type);

        l = eval_cast(block, l, type);
        r = eval_cast(block, r, type);
    } else if (!(
        is_pointer(l.type) && is_pointer(r.type) &&
        l.type->next->size && l.type->next->size == r.type->next->size)
    ) {
        error("Incompatible operand types for relational expression.");
    }

    return evaluate(block, e ? IR_OP_GE : IR_OP_GT, type_init_integer(4), l, r);
}

/* Convert variables of type ARRAY or FUNCTION to addresses when used in
 * expressions. 'array of T' is converted (decay) to pointer to T. Not the same
 * as taking the address of an array, which would give 'pointer to array of T'.
 */
static struct var array_or_func_to_addr(struct block *block, struct var var)
{
    /* Convert IMMEDIATE string values of type char [n] to char *, adding the
     * string to strings table and generating a unique label. */
    if (var.string && var.type->type == ARRAY) {
        assert(var.kind == IMMEDIATE);
        var.string = strlabel(var.string);
        var.type = type_init_pointer(var.type->next);
    }

    /* References to arrays decay into pointer to the first element. Change type
     * before doing regular address evaluation, this way backend does not need
     * to handle address of array in any special way. The memory location
     * represented by var can also be seen as the first element. */
    else if (var.type->type == ARRAY) {
        var.type = var.type->next;
        var = eval_addr(block, var);
    }

    /* It is unclear what this does, never tested function pointers :p */
    else if (var.type->type == FUNCTION) {
        var = eval_addr(block, var);
    }

    return var;
}

/* Evaluate a = b <op> c, or unary expression a = <op> b
 *
 * Returns a DIRECT reference to a new temporary, or an immediate value.
 */
struct var eval_expr(struct block *block, enum optype op, ...)
{
    va_list args;
    struct var l, r;

    va_start(args, op);
    l = va_arg(args, struct var);
    l = array_or_func_to_addr(block, l);
    if (NOPERANDS(op) == 2) {
        r = va_arg(args, struct var);
        r = array_or_func_to_addr(block, r);
    }
    va_end(args);

    switch (op) {
    case IR_OP_MOD:
        l = eval_expr_mod(block, l, r);
        break;
    case IR_OP_MUL:
        l = eval_expr_mul(block, l, r);
        break;
    case IR_OP_DIV:
        l = eval_expr_div(block, l, r);
        break;
    case IR_OP_ADD:
        l = eval_expr_add(block, l, r);
        break;
    case IR_OP_SUB:
        l = eval_expr_sub(block, l, r);
        break;
    case IR_OP_EQ:
        l = eval_expr_eq(block, l, r);
        break;
    case IR_OP_GE:
        l = eval_expr_cmp(block, l, r, 1);
        break;
    case IR_OP_GT:
        l = eval_expr_cmp(block, l, r, 0);
        break;
    case IR_OP_AND:
    case IR_OP_XOR:
    case IR_OP_OR:
        /* Ignore immediate evaluation. */
        if (!is_integer(l.type) || !is_integer(r.type)) {
            error("Operands must have integer type.");
        }
        l = evaluate(block, op,
            usual_arithmetic_conversion(l.type, r.type), l, r);
        break;
    default:
        internal_error("%s", "Invalid opcode.");
        break;
    }

    return l;
}

/* Evaluate &a.
 */
struct var eval_addr(struct block *block, struct var var)
{
    struct op op;
    struct var tmp;

    switch (var.kind) {
    case IMMEDIATE:
        if (!var.string) {
            error("Address of immediate other than string, was '%s'.",
                typetostr(var.type));
            exit(1);
        }
        /* Address of string literal can be done without evaluation, just decay
         * the variable to pointer. */
        if (var.type->type == ARRAY) {
            var = array_or_func_to_addr(block, var);
            assert(var.kind == IMMEDIATE);
            break;
        }
        assert(is_pointer(var.type) && var.string);
    case DIRECT:
        tmp = var_direct(sym_temp(&ns_ident, type_init_pointer(var.type)));
        sym_list_push_back(&decl->locals, (struct symbol *) tmp.symbol);
        op.type = IR_ADDR;
        op.a = tmp;
        op.b = var;
        cfg_ir_append(block, op);
        var = tmp;
        break;
    case DEREF:
        assert(is_pointer(var.symbol->type));
        tmp = var_direct(var.symbol);
        if (var.offset) {
            /* Address of *(sym + offset) is (sym + offset), but without pointer
             * arithmetic applied in addition. Cast to char pointer temporarily
             * to avoid trouble calling eval_expr. */
            tmp = eval_cast(block, tmp,
                type_init_pointer(type_init_integer(1)));
            tmp = eval_expr(block, IR_OP_ADD, tmp, var_int(var.offset));
        }
        tmp.type = type_init_pointer(var.type);
        var = tmp;
        break;
    }

    return var;
}

/* Evaluate *var.
 * If DEREF: *(*var'), evaluate the deref of var' first.
 * If DIRECT: Convert to deref variable, evaluate if offset or non-pointer.
 */
struct var eval_deref(struct block *block, struct var var)
{
    struct var ptr;
    struct op ir_op;

    /* Dereferenced arrays should decay into pointer. */
    var = array_or_func_to_addr(block, var);

    if (var.kind == DEREF) {
        assert(is_pointer(var.symbol->type));

        /* Cast to char pointer temporarily to avoid pointer arithmetic calling
         * eval_expr. No actual evaluation is performed by this. */
        ptr = var_direct(var.symbol);
        ptr = eval_cast(block, ptr, type_init_pointer(type_init_integer(1)));
        ptr = eval_expr(block, IR_OP_ADD, ptr, var_int(var.offset));
        ptr.type = var.symbol->type;

        /* Result is a new symbol. */
        var = var_direct(sym_temp(&ns_ident, var.type));
        sym_list_push_back(&decl->locals, (struct symbol *) var.symbol);

        /* Perform the dereferencing. */
        ir_op.type = IR_DEREF;
        ir_op.a = var;
        ir_op.b = ptr;
        cfg_ir_append(block, ir_op);

    } else if (
        var.kind == DIRECT &&
        (var.offset || !is_pointer(var.symbol->type)))
    {
        /* Cannot immediately dereference a pointer which is at a direct offset
         * from another symbol. Also, pointers that are the result of indexing
         * into a structure must be evaluated, as DEREF variables assume symbol
         * to be of pointer type. */
        ptr = var_direct(sym_temp(&ns_ident, var.type));
        sym_list_push_back(&decl->locals, (struct symbol *) ptr.symbol);
        ptr.lvalue = 1;
        eval_assign(block, ptr, var);
        var = ptr;
    }

    /* Simply convert a direct reference to a deref. */
    assert(var.kind == DIRECT && !var.offset);
    assert(is_pointer(var.type) && is_pointer(var.symbol->type));
    var.kind = DEREF;
    var.type = type_deref(var.type);
    var.lvalue = 1;
    return var;
}

/* Evaluate simple assignment (6.5.16.1). 
 *
 *      a = b
 *
 * The type of the expression is typeof(a) after l-to-r-value conversion. The
 * result is an r-value. Operand b is converted to typeof(A) before assignment.
 */
struct var eval_assign(struct block *block, struct var target, struct var var)
{
    struct op op;

    if (!target.lvalue) {
        error("Target of assignment must be l-value.");
        exit(1);
    }

    if (target.type->type != ARRAY) {
        var = array_or_func_to_addr(block, var);
    }

    /* Special case char [] = string in initializers. BUT: This is not enough,
     * also uses foo = zero constant to fill up static initialization... */
    if (target.type->type == ARRAY) {
        if (!type_equal(target.type, var.type) || var.kind != IMMEDIATE) {
            error("Invalid initializer assignment, was %s :: %s = %s.",
                target.symbol->name,
                typetostr(target.type),
                typetostr(var.type));
            exit(1);
        }
    }
    /* Rules for simple assignment. */
    else if (
        /* The left operand has atomic, qualified, or unqualified arithmetic
         * type, and the right has arithmetic type. */
        !(is_arithmetic(target.type) && is_arithmetic(var.type)) &&
        /* The left operand has an atomic, qualified, or unqualified version of
         * a structure or union type compatible with the type of the right. */
        !(is_struct_or_union(target.type)
            && is_compatible(target.type, var.type)) &&
        /* The left operand has atomic, qualified, or unqualified pointer type,
         * and (considering the type the left operand would have after lvalue
         * conversion) both operands are pointers to qualified or unqualified
         * versions of compatible types, and the type pointed to by the left has
         * all the qualifiers of the type pointed to by the right. */
        !(is_pointer(target.type) && is_pointer(var.type)
            && is_compatible(target.type->next, var.type->next)
            && (target.type->next->qualifier | var.type->next->qualifier)
                == target.type->next->qualifier) &&
        /* The left operand has atomic, qualified, or unqualified pointer type,
         * and (considering the type the left operand would have after lvalue
         * conversion) one operand is a pointer to an object type, and the other
         * is a pointer to a qualified or unqualified version of void, and the
         * type pointed to by the left has all the qualifiers of the type
         * pointed to by the right. */
        !(is_pointer(target.type) && is_pointer(var.type)
            && ((is_void(target.type->next) && is_object(var.type->next))
                || (is_object(target.type->next) && is_void(var.type->next)))
            && (target.type->next->qualifier | var.type->next->qualifier)
                == target.type->next->qualifier) &&
        /* The left operand is an atomic, qualified, or unqualified pointer, and
         * the right is a null pointer constant. */
        !(is_pointer(target.type) && is_nullptr(var)))
    {
        error("Incompatible operands to assignment expression, %s :: %s = %s.",
            target.symbol->name, typetostr(target.type), typetostr(var.type));
        exit(1);
    }

    /* Assignment has implicit conversion for basic types when evaluating the IR
     * operation, meaning var will be sign extended to size of target.type. */
    op.type = IR_ASSIGN;
    op.a = target;
    op.b = var;
    cfg_ir_append(block, op);
    target.lvalue = 0;
    return target;
}

/* Evaluate a = copy(b). Create a new temporary variable to hold the result,
 * circumventing the l-value restriction for temporaries to do the assignment.
 */
struct var eval_copy(struct block *block, struct var var)
{
    struct var res;
    struct symbol *temp;

    temp = sym_temp(&ns_ident, var.type);
    res = var_direct(temp);
    assert(res.kind != IMMEDIATE);
    res.lvalue = 1;

    sym_list_push_back(&decl->locals, temp);

    eval_assign(block, res, var);

    res.lvalue = 0;
    return res;
}

/* Evaluate a = func ().
 */
struct var eval_call(struct block *block, struct var var)
{
    struct op op;
    struct var res;

    if (var.type->next->type == NONE) {
        res = var_void();
    } else {
        struct symbol *temp = sym_temp(&ns_ident, var.type->next);
        res = var_direct(temp);
        sym_list_push_back(&decl->locals, temp);
    }

    op.type = IR_CALL;
    op.a = res;
    op.b = var;
    cfg_ir_append(block, op);
    return res;
}

/* Evaluate return (expr)
 * If expr has a different type than return value T, a conversion equivalent to
 * assignment is made:
 *
 *      T a = expr;
 *      return a;
 *
 */
void eval_return(struct block *block)
{
    const struct typetree *ret = get_return_type(decl->fun->type);
    assert(ret->type != NONE);

    if (!type_equal(ret, block->expr.type)) {
        struct var tmp = var_direct(sym_temp(&ns_ident, ret));
        sym_list_push_back(&decl->locals, (struct symbol *) tmp.symbol);
        tmp.lvalue = 1;
        eval_assign(block, tmp, block->expr);
        tmp.lvalue = 0;
        block->expr = tmp;
    }

    block->has_return_value = 1;
}

/* 6.5.4 Cast operators.
 *
 *      (long) a
 */
struct var eval_cast(struct block *b, struct var v, const struct typetree *t)
{
    if (t->type == NONE) {
        v = var_void();
    } else if (is_scalar(v.type) && is_scalar(t)) {
        if (v.type->size == t->size) {
            v.type = t;
        } else {
            v = evaluate(b, IR_CAST, t, v);
        }
    } else {
        error(
            "Invalid type parameters to cast expression,"
            " cannot convert %s to %s.",
            typetostr(v.type), typetostr(t));
    }

    return v;
}

/* 6.5.15 Conditional operator.
 * 
 *      a ? b : c
 */
struct var
eval_conditional(struct var a, struct block *b, struct block *c)
{
    struct var result;
    const struct typetree *t1, *t2, *type = NULL;

    if (!is_scalar(a.type)) {
        error("Conditional must be scalar type.");
    }

    /* Arrays and functions decay into pointers. */
    b->expr = array_or_func_to_addr(b, b->expr);
    c->expr = array_or_func_to_addr(c, c->expr);

    t1 = b->expr.type;
    t2 = c->expr.type;

    /* Determine type of the result based on type of b and c. */
    if (is_arithmetic(t1) && is_arithmetic(t2)) {
        type = usual_arithmetic_conversion(t1, t2);
    } else if (
        (t1->type == NONE && t2->type == NONE) ||
        (t1->type == OBJECT && type_equal(t1, t2)) ||
        (t1->type == POINTER && t2->type == POINTER && is_compatible(t1, t2)) ||
        (t1->type == POINTER && is_nullptr(c->expr)))
    {
        type = t1;
    } else if (t2->type == POINTER && is_nullptr(b->expr)) {
        type = t2;
    } else {
        extern int VERBOSE;

        /* The rules are more complex than this, revisit later. */
        error("Unsupported types in conditional operator.");
        if (VERBOSE) {
            error("Operands were '%s' and '%s'.", typetostr(t1), typetostr(t2));
        }
        exit(1);
    }

    /* Assign variable in end of each block. */
    result = var_direct(sym_temp(&ns_ident, type));
    sym_list_push_back(&decl->locals, (struct symbol *) result.symbol);
    result.lvalue = 1;
    b->expr = eval_assign(b, result, b->expr);
    c->expr = eval_assign(c, result, c->expr);

    /* Return immediate values if possible. */
    if (a.kind == IMMEDIATE) {
        return (a.value.integer) ? b->expr : c->expr;
    }

    result.lvalue = 0;
    return result;
}

/* 6.5.13 Logical AND operator.
 */
struct block *eval_logical_or(
    struct block *left,
    struct block *right_top,
    struct block *right)
{
    if (!is_scalar(left->expr.type) || !is_scalar(right->expr.type)) {
        error("Operands to logical or must be of scalar type.");
    } else if (left->expr.kind == IMMEDIATE && right->expr.kind == IMMEDIATE) {
        left->expr =
            var_int(left->expr.value.integer || right->expr.value.integer);
    } else {
        left = eval_logical_expression(0, left, right_top, right);
    }

    return left;
}

/* 6.5.14 Logical OR operator.
 */
struct block *eval_logical_and(
    struct block *left,
    struct block *right_top,
    struct block *right)
{
    if (!is_scalar(left->expr.type) || !is_scalar(right->expr.type)) {
        error("Operands to logical and must be of scalar type.");
    } else if (left->expr.kind == IMMEDIATE && right->expr.kind == IMMEDIATE) {
        left->expr =
            var_int(left->expr.value.integer && right->expr.value.integer);
    } else {
        left = eval_logical_expression(1, left, right_top, right);
    }

    return left;
}

void param(struct block *block, struct var p)
{
    struct op op = { IR_PARAM };

    op.a = array_or_func_to_addr(block, p);
    cfg_ir_append(block, op);
}

struct var eval__builtin_va_start(struct block *block, struct var arg)
{
    struct op op = { IR_VA_START };

    op.a = arg;
    cfg_ir_append(block, op);
    return var_void();
}

struct var eval__builtin_va_arg(
    struct block *block,
    struct var arg,
    const struct typetree *type)
{
    struct op op = { IR_VA_ARG };
    struct var result;

    result = var_direct(sym_temp(&ns_ident, type));
    sym_list_push_back(&decl->locals, (struct symbol *) result.symbol);

    op.a = result;
    op.b = arg;
    cfg_ir_append(block, op);
    return result;
}
