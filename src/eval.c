/* Construct op_t operations and temp variables based on symbols and optypes. */

#include "ir.h"
#include "type.h"
#include "error.h"

#include <assert.h>
#include <stdarg.h>
#include <stdlib.h>

/* Evaluate a = b <op> c, or unary expression a = <op> b
 *
 * Returns a DIRECT reference to a new temporary, or an immediate value.
 */
var_t
eval_expr(block_t *block, optype_t optype, ...)
{
    va_list args;

    op_t op;
    var_t res, left, right;
    const typetree_t *type;
    const symbol_t *temp;

    va_start(args, optype);
    left = va_arg(args, var_t);
    if (NOPERANDS(optype) == 2) {
        right = va_arg(args, var_t);
        if (left.kind == IMMEDIATE && right.kind == IMMEDIATE &&
            left.type->type == INTEGER && right.type->type == INTEGER)
        {
            switch (optype) {
                case IR_OP_LOGICAL_AND:
                    return var_long(left.value.integer && right.value.integer);
                case IR_OP_LOGICAL_OR:
                    return var_long(left.value.integer || right.value.integer);
                case IR_OP_BITWISE_OR:
                    return var_long(left.value.integer | right.value.integer);
                case IR_OP_BITWISE_XOR:
                    return var_long(left.value.integer ^ right.value.integer);
                case IR_OP_BITWISE_AND:
                    return var_long(left.value.integer & right.value.integer);
                case IR_OP_ADD:
                    return var_long(left.value.integer + right.value.integer);
                case IR_OP_SUB:
                    return var_long(left.value.integer - right.value.integer);
                case IR_OP_MUL:
                    return var_long(left.value.integer * right.value.integer);
                case IR_OP_DIV:
                    return var_long(left.value.integer / right.value.integer);
                case IR_OP_MOD:
                    return var_long(left.value.integer % right.value.integer);
                case IR_OP_EQ:
                    return var_long(left.value.integer == right.value.integer);
                case IR_OP_GE:
                    return var_long(left.value.integer >= right.value.integer);
                case IR_OP_GT:
                    return var_long(left.value.integer > right.value.integer);
                default:
                    assert(0);
            }
        }
        switch (optype) {
            case IR_OP_EQ:
            case IR_OP_GE:
            case IR_OP_GT:
            case IR_OP_LOGICAL_AND:
            case IR_OP_LOGICAL_OR:
                type = type_init(INTEGER);
                break;
            default:
                type = type_combine(left.type, right.type);
                break;
        }
    } else {
        if (left.kind == IMMEDIATE &&
            left.type->type == INTEGER)
        {
            switch (optype) {
                case IR_OP_NOT:
                    return var_long(!left.value.integer);
                default:
                    assert(0);
            }
        }
        type = type_init(INTEGER);
    }

    temp = sym_temp(&ns_ident, type);
    res = var_direct(temp);
    assert(res.kind != IMMEDIATE);

    op.a = res;
    op.b = left;
    op.c = right;
    op.type = optype;
    cfg_ir_append(block, op);

    va_end(args);
    return res;
}

/* Evaluate &a. Depending on var_t a:
 * If DEREF, create a new var_t with a DIRECT reference to the same symbol_t.
 *     Not even necessary to add any code for cases like &(*foo).
 * If DIRECT, create a temporary symbol with type pointer to a::type, and add
 *     an operation to the current block.
 * If IMMEDIATE: not implemented
 *
 * Result is always DIRECT.
 */
var_t
eval_addr(block_t *block, var_t right)
{
    op_t op;
    var_t res;
    const symbol_t *temp;
    typetree_t *type;

    type = type_init(POINTER);
    type->next = right.type;

    switch (right.kind) {
        case DIRECT:
            temp = sym_temp(&ns_ident, type);
            res = var_direct(temp);

            op.type = IR_ADDR;
            op.a = res;
            op.b = right;

            cfg_ir_append(block, op);
            break;
        case DEREF:
            res = var_direct(right.symbol);
            if (right.offset) {
                res = eval_expr(block, IR_OP_SUB, res, var_long((long) right.offset));
            }
            res.type = type;
            break;
        case IMMEDIATE:
            error("Address of immediate is not supported.");
            exit(1);
    }
    return res;
}

/* Evaluate *a.
 * If DEREF: *(*a'), double deref, evaluate the deref of a', and create 
 *     a new deref variable.
 * If DIRECT: Create a new deref variable, no evaluation.
 * If IMMEDIATE: not implemented.
 */
var_t
eval_deref(block_t *block, var_t var)
{
    op_t op;
    var_t res;
    const symbol_t *temp;

    switch (var.kind) {
        case DIRECT:
            res = var_deref(var.symbol, 0);
            break;
        case DEREF:
            if (var.offset) {
                var = eval_expr(block, IR_OP_SUB, 
                    var_direct(var.symbol),
                    var_long((long) var.offset));
            }
            temp = sym_temp(&ns_ident, type_deref(var.symbol->type));
            res = var_deref(temp, 0);

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
 * Restrictions on a: DEREF or DIRECT lvalue, not temporary.
 * Restrictions on b: None
 * 
 * Return value is the value of b.
 */
var_t
eval_assign(block_t *block, var_t target, var_t var)
{
    op_t op;

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
var_t
eval_copy(block_t *block, var_t var)
{
    var_t res;
    const symbol_t *sym;

    sym = sym_temp(&ns_ident, var.type);
    res = var_direct(sym);
    assert(res.kind != IMMEDIATE);
    res.lvalue = 1;

    eval_assign(block, res, var);

    res.lvalue = 0;
    return res;
}

var_t
eval_call(block_t *block, var_t func)
{
    op_t op;
    var_t res;
    const symbol_t *temp;

    if (func.type->next->type == NONE) {
        res = var_void();
    } else {
        temp = sym_temp(&ns_ident, func.type->next);
        res = var_direct(temp);
    }

    op.type = IR_CALL;
    op.a = res;
    op.b = func;
    cfg_ir_append(block, op);

    return res;
}

void
param(block_t *block, var_t p)
{
    op_t op;
    op.type = IR_PARAM;
    op.a = p;
    cfg_ir_append(block, op);
}
