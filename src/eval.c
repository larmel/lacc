#include "eval.h"
#include "error.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>

struct var var_direct(const struct symbol *symbol)
{
    struct var var = {0};
    var.type = symbol->type;
    if (symbol->symtype == SYM_ENUM_VALUE) {
        var.kind = IMMEDIATE;
        var.value.integer = symbol->enum_value;
    } else {
        var.kind = DIRECT;
        var.symbol = symbol;
        var.lvalue = symbol->name[0] != '.';
    }
    return var;
}

struct var var_direct_ref(struct block *block, const struct symbol *symbol)
{
    struct var var;

    assert(symbol->type->type == ARRAY || symbol->type->type == FUNCTION);

    var = var_direct(symbol);
    if (symbol->type->type == ARRAY) {
        var.type = symbol->type->next;
    }

    return eval_addr(block, var);
}

struct var var_deref(const struct symbol *symbol, int offset)
{
    struct var var = {0};
    assert(symbol->type->type == POINTER);

    var.kind = DEREF;
    var.symbol = symbol;
    var.type = type_deref(symbol->type);
    var.offset = offset;
    var.lvalue = 1;

    return var;
}

struct var var_string(const char *label, size_t length)
{
    struct var var = {0};

    var.kind = IMMEDIATE;
    var.type = type_init_string(length);
    var.value.string = label;

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

struct var var_void()
{
    struct var var = {0};

    var.kind = IMMEDIATE;
    var.type = type_init_void();
    return var;
}

int is_nullptr(struct var val)
{
    return 
        (val.type->type == INTEGER || val.type->type == POINTER) &&
        (val.kind == IMMEDIATE && !val.value.integer);
}

/* Current declaration from parser. Need to add symbols to list whenever new
 * ones are created with sym_temp. And no, that should not be in symtab.c. */
extern struct decl *decl;

static struct var
eval(   struct block *block, enum optype optype, const struct typetree *type,
        struct var l, struct var r)
{
    struct op op;
    struct symbol *sym;
    struct var res;

    sym = sym_temp(&ns_ident, type);
    res = var_direct(sym);
    assert(res.kind == DIRECT);

    sym_list_push_back(&decl->locals, sym);

    op.a = res;
    op.b = l;
    op.c = r;
    op.type = optype;
    cfg_ir_append(block, op);

    return res;
}

/* 6.5.5 Multiplicative Operators.
 */
static struct var
eval_expr_mul(struct block *block, struct var l, struct var r)
{
    const struct typetree *type = usual_arithmetic_conversion(l.type, r.type);

    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        return var_int(l.value.integer * r.value.integer);
    }

    return eval(block, IR_OP_MUL, type, l, r);
}

static struct var
eval_expr_div(struct block *block, struct var l, struct var r)
{
    const struct typetree *type = usual_arithmetic_conversion(l.type, r.type);

    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        return var_int(l.value.integer / r.value.integer);
    }

    return eval(block, IR_OP_DIV, type, l, r);
}

static struct var
eval_expr_mod(struct block *block, struct var l, struct var r)
{
    const struct typetree *type = usual_arithmetic_conversion(l.type, r.type);
    if (!is_integer(type)) {
        error("Operands of mod must be integer type.");
    }

    if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
        return var_int(l.value.integer % r.value.integer);
    }

    return eval(block, IR_OP_MOD, type, l, r);
}

/* 6.5.6 Additive Operators.
 */
static struct var
eval_expr_add(struct block *block, struct var l, struct var r)
{
    const struct typetree *type;

    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        type = usual_arithmetic_conversion(l.type, r.type);
        l = eval_cast(block, l, type);
        r = eval_cast(block, r, type);
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = var_int(l.value.integer + r.value.integer);
        } else {
            l = eval(block, IR_OP_ADD, type, l, r);
        }
    } else if (is_integer(l.type) && is_pointer(r.type)) {
        l = eval_expr_add(block, r, l);
    } else if (is_pointer(l.type) && l.type->next->size && is_integer(r.type)) {
        r = eval_expr(block, IR_OP_MUL, var_int(l.type->next->size), r);
        l = eval(block, IR_OP_ADD, l.type, l, r);
    } else {
        error("Incompatible arguments to addition operator, was %s and %s.",
            typetostr(l.type), typetostr(r.type));
    }

    return l;
}

static struct var
eval_expr_sub(struct block *block, struct var l, struct var r)
{
    if (is_arithmetic(l.type) && is_arithmetic(r.type)) {
        if (l.kind == IMMEDIATE && r.kind == IMMEDIATE) {
            l = var_int(l.value.integer - r.value.integer);
        } else {
            const struct typetree *type
                = usual_arithmetic_conversion(l.type, r.type);
            l = eval_cast(block, l, type);
            r = eval_cast(block, r, type);
            l = eval(block, IR_OP_SUB, type, l, r);
        }
    } else if (is_pointer(l.type) && is_integer(r.type)) {
        if (!l.type->next->size) {
            error("Pointer arithmetic on incomplete type.");
        }
        r = eval_expr(block, IR_OP_MUL, var_int(l.type->next->size), r);
        l = eval(block, IR_OP_SUB, l.type, l, r);
    } else if (is_pointer(l.type) && is_pointer(r.type)) {
        struct typetree *type = type_init_integer(8);
        type->is_unsigned = 1;

        if (!(l.type->next->size && l.type->next->size == r.type->next->size)) {
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
static struct var
eval_expr_eq(struct block *block, struct var l, struct var r)
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

    return eval(block, IR_OP_EQ, type_init_integer(4), l, r);
}

/* 6.5.13-14 Logical AND/OR operator.
 */
static struct var
eval_logical_and(struct block *block, struct var left, struct var right)
{
    if (!is_scalar(left.type) || !is_scalar(right.type)) {
        error("Operands to logical and must be of scalar type.");
    }

    if (left.kind == IMMEDIATE && right.kind == IMMEDIATE) {
        return var_int(left.value.integer && right.value.integer);
    }

    return eval(block, IR_OP_LOGICAL_AND, type_init_integer(4), left, right);
}

static struct var
eval_logical_or(struct block *block, struct var left, struct var right)
{
    if (!is_scalar(left.type) || !is_scalar(right.type)) {
        error("Operands to logical or must be of scalar type.");
    }

    if ((left.kind == IMMEDIATE && left.value.integer) ||
        (right.kind == IMMEDIATE && right.value.integer)) {
        return var_int(1);
    } else if (left.kind == IMMEDIATE && right.kind == IMMEDIATE) {
        return var_int(left.value.integer || right.value.integer);
    }

    return eval(block, IR_OP_LOGICAL_OR, type_init_integer(4), left, right);
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

    return eval(block, e ? IR_OP_GE : IR_OP_GT, type_init_integer(4), l, r);
}

/* Extract core address-of evaluation to suit special cases with address of
 * functions and arrays. */
static struct var
eval_addr_internal(struct block *block, struct var var, struct typetree *type)
{
    struct op op;
    struct var res;
    struct symbol *temp;

    switch (var.kind) {
    case IMMEDIATE:
        /* Array constants are passed as immediate values with array type. Decay
         * into pointer on evaluation, for example as parameters. Should maybe
         * consider an extra kind ADDR to not have to generate all these
         * temporaries. */
        assert(var.type->type == ARRAY);
    case DIRECT:
        temp = sym_temp(&ns_ident, type);
        res = var_direct(temp);
        sym_list_push_back(&decl->locals, temp);
        op.type = IR_ADDR;
        op.a = res;
        op.b = var;
        cfg_ir_append(block, op);
        break;
    case DEREF:
        res = var_direct(var.symbol);
        if (var.offset) {
            /* Address of *(sym + offset) is (sym + offset), but without pointer
             * arithmetic applied in addition. Cast to char pointer temporarily
             * to avoid trouble calling eval_expr. */
            res = eval_cast(block, res,
                type_init_pointer(type_init_integer(1)));
            res = eval_expr(block, IR_OP_ADD, res, var_int(var.offset));
        }
        res.type = type;
        break;
    }

    return res;
}

/* Convert variables of type ARRAY or FUNCTION to addresses when used in
 * expressions. 'array of T' is converted (decay) to pointer to T. Not the same
 * as taking the address of an array, which would give 'pointer to array of T'.
 */
static struct var
array_or_func_to_addr(struct block *block, struct var var)
{
    if (var.type->type == ARRAY) {
        var = eval_addr_internal(block, var, type_init_pointer(var.type->next));  
    } else if (var.type->type == FUNCTION) {
        var = eval_addr(block, var);
    }

    return var;
}

/* Evaluate a = b <op> c, or unary expression a = <op> b
 *
 * Returns a DIRECT reference to a new temporary, or an immediate value.
 */
struct var eval_expr(struct block *block, enum optype optype, ...)
{
    va_list args;
    struct var l, r;

    va_start(args, optype);
    l = va_arg(args, struct var);
    l = array_or_func_to_addr(block, l);
    if (NOPERANDS(optype) == 2) {
        r = va_arg(args, struct var);
        r = array_or_func_to_addr(block, r);
    }
    va_end(args);

    switch (optype) {
    case IR_OP_MOD:         l = eval_expr_mod(block, l, r);         break;
    case IR_OP_MUL:         l = eval_expr_mul(block, l, r);         break;
    case IR_OP_DIV:         l = eval_expr_div(block, l, r);         break;
    case IR_OP_ADD:         l = eval_expr_add(block, l, r);         break;
    case IR_OP_SUB:         l = eval_expr_sub(block, l, r);         break;
    case IR_OP_EQ:          l = eval_expr_eq(block, l, r);          break;
    case IR_OP_GE:          l = eval_expr_cmp(block, l, r, 1);      break;
    case IR_OP_GT:          l = eval_expr_cmp(block, l, r, 0);      break;
    case IR_OP_LOGICAL_AND: l = eval_logical_and(block, l, r);      break;
    case IR_OP_LOGICAL_OR:  l = eval_logical_or(block, l, r);       break;
    case IR_OP_BITWISE_AND:
    case IR_OP_BITWISE_XOR:
    case IR_OP_BITWISE_OR:
        /* Ignore immediate evaluation. */
        if (!is_integer(l.type) || !is_integer(r.type)) {
            error("Operands must have integer type.");
        }
        l = eval(block, optype,
            usual_arithmetic_conversion(l.type, r.type), l, r);
        break;
    default:
        internal_error("%s", "Invalid opcode.");
        break;
    }

    return l;
}

/* Evaluate &a. Depending on var a:
 * If DEREF, create a new var with a DIRECT reference to the same symbol. Not
 *     even necessary to add any code for cases like &(*foo).
 * If DIRECT, create a temporary symbol with type pointer to a::type, and add
 *     an operation to the current block.
 * If IMMEDIATE: not implemented
 *
 * Result is always DIRECT.
 */
struct var eval_addr(struct block *block, struct var right)
{
    return eval_addr_internal(block, right, type_init_pointer(right.type));
}

/* Evaluate *a.
 * If DEREF: *(*a'), double deref, evaluate the deref of a', and create 
 *     a new deref variable.
 * If DIRECT: Create a new deref variable, no evaluation.
 * If IMMEDIATE: not implemented.
 */
struct var eval_deref(struct block *block, struct var var)
{
    struct op op;
    struct var res;
    struct symbol *temp;

    switch (var.kind) {
    case DIRECT:
        res = var_deref(var.symbol, 0);
        break;
    case DEREF:
        if (var.offset) {
            var = eval_expr(block, IR_OP_SUB, 
                var_direct(var.symbol),
                var_int((long) var.offset));
        }
        temp = sym_temp(&ns_ident, type_deref(var.symbol->type));
        res = var_deref(temp, 0);

        sym_list_push_back(&decl->locals, temp);

        op.type = IR_DEREF;
        op.a = res;
        op.b = var;

        cfg_ir_append(block, op);
        break;
    case IMMEDIATE:
        error("Dereferenced immediate is not supported.");
        exit(1);
        break;
    }

    return res;
}

/* Evaluate a = b.
 * Restrictions on a: DEREF or DIRECT l-value, not temporary.
 * Restrictions on b: None
 * 
 * Return value is the value of b.
 */
struct var eval_assign(struct block *block, struct var target, struct var var)
{
    struct op op;

    if (!target.lvalue) {
        error("Target of assignment must be l-value.");
        exit(1);
    }

    /* NB: missing type checking! */
    op.type = IR_ASSIGN;
    op.a = target;
    op.b = var;
    cfg_ir_append(block, op);

    return var;
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

struct var eval_call(struct block *block, struct var func)
{
    struct op op;
    struct var res;
    struct symbol *temp;

    if (func.type->next->type == NONE) {
        res = var_void();
    } else {
        temp = sym_temp(&ns_ident, func.type->next);
        res = var_direct(temp);

        sym_list_push_back(&decl->locals, temp);
    }

    op.type = IR_CALL;
    op.a = res;
    op.b = func;
    cfg_ir_append(block, op);

    return res;
}

struct var
eval_cast(struct block *block, struct var var, const struct typetree *type)
{
    struct op op;
    struct var res;
    struct symbol *temp;

    if (var.type->size == type->size) {
        var.type = type;
        res = var;
    } else {
        temp = sym_temp(&ns_ident, type);
        res = var_direct(temp);

        op.type = IR_CAST;
        op.a = res;
        op.b = var;
        cfg_ir_append(block, op);

        sym_list_push_back(&decl->locals, temp);
    }

    return res;
}

/* 6.5.15 Conditional operator.
 * 
 *      a ? b : c
 */
struct var
eval_conditional(struct var a, struct block *b, struct block *c)
{
    struct var result;
    const struct typetree
        *t1 = b->expr.type,
        *t2 = c->expr.type,
        *type = NULL;

    if (!is_scalar(a.type)) {
        error("Conditional must be scalar type.");
    }

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
        /* The rules are more complex than this, revisit later. */
        error("Unsupported types in conditional operator.");
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

void param(struct block *block, struct var p)
{
    struct op op;

    p = array_or_func_to_addr(block, p);

    op.type = IR_PARAM;
    op.a = p;
    cfg_ir_append(block, op);
}
