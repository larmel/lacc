#include "eval.h"
#include "error.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>

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
        l = eval_expr_add(block, r, l);
    } else if (is_pointer(l.type) && l.type->next->size && is_integer(r.type)) {
        r = eval_expr(block, IR_OP_MUL, var_int(l.type->next->size), r);
        l = evaluate(block, IR_OP_ADD, l.type, l, r);
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
        }
        r = eval_expr(block, IR_OP_MUL, var_int(l.type->next->size), r);
        l = evaluate(block, IR_OP_SUB, l.type, l, r);
    } else if (is_pointer(l.type) && is_pointer(r.type)) {
        struct typetree *type = type_init_unsigned(8);

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
static struct var array_or_func_to_addr(struct block *block, struct var var)
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

/* Evaluate &a. Depending on var a:
 * If DEREF, create a new var with a DIRECT reference to the same symbol. Not
 *     even necessary to add any code for cases like &(*foo).
 * If DIRECT, create a temporary symbol with type pointer to a::type, and add
 *     an operation to the current block.
 * If IMMEDIATE: not implemented
 *
 * Result is always DIRECT.
 */
struct var eval_addr(struct block *block, struct var var)
{
    return eval_addr_internal(block, var, type_init_pointer(var.type));
}

/* Evaluate *var.
 * If DEREF: *(*var'), double deref, evaluate the deref of var' first.
 * If DIRECT: Convert to deref variable, no evaluation.
 */
struct var eval_deref(struct block *block, struct var var)
{
    struct var ptr;
    struct op ir_op;

    if (var.kind == DEREF) {
        assert( var.symbol->type->type == POINTER );

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
    }

    assert( var.kind == DIRECT );
    assert( var.type->type == POINTER );

    /* Simply convert a direct reference to a deref. */
    var.kind = DEREF;
    var.type = type_deref(var.type);
    var.lvalue = 1;

    return var;
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
        error("Invalid type parameters to cast expression.");
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
