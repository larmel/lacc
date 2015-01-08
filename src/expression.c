/* Construct op_t operations and temp variables based on symbols and optypes. */

#include "ir.h"
#include "symbol.h"
#include "error.h"

#include <stdlib.h>


/* Add a 3-address code operation to the block. Code is kept in a separate list
 * for each block. */
void ir_append(block_t *, op_t);

/* Evaluate a <op> b.
 *
 * Returns a DIRECT reference to a new temporary, or an immediate value.
 */
var_t
eval_expr(block_t *block, optype_t optype, var_t left, var_t right)
{
    op_t op;
    var_t res;
    const symbol_t *temp;

    switch (optype) {
        case IR_OP_LOGICAL_AND:
        case IR_OP_LOGICAL_OR:
        case IR_OP_BITWISE_OR:
        case IR_OP_BITWISE_XOR:
        case IR_OP_BITWISE_AND:
        case IR_OP_ADD:
            if (left.kind == IMMEDIATE && right.kind == IMMEDIATE &&
                left.type->type == INT64_T && right.type->type == INT64_T) {

                return var_long(left.value.v_long + right.value.v_long);
            }
        case IR_OP_SUB:
        case IR_OP_MUL:
            if (left.kind == IMMEDIATE && right.kind == IMMEDIATE &&
                left.type->type == INT64_T && right.type->type == INT64_T) {

                return var_long(left.value.v_long * right.value.v_long);
            }
        case IR_OP_DIV:
        case IR_OP_MOD:
            temp = sym_temp(type_combine(left.type, right.type));
            res = var_direct(temp);
            op.a = res;
            op.b = left;
            op.c = right;
            break;
        default:
            error("Wrong call of evaluate for non-arithmetic expression.");
            exit(1);
    }

    op.type = optype;
    ir_append(block, op);

    return res;
}

/* Evaluate &a. Depending on var_t a:
 * If OFFSET, create a new var_t with a DIRECT reference to the same symbol_t.
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

    switch (right.kind) {
        case DIRECT:
            type = type_init(POINTER);
            type->next = right.type;
            temp = sym_temp(type);
            res = var_direct(temp);

            op.type = IR_ADDR;
            op.a = res;
            op.b = right;

            ir_append(block, op);
            break;
        case OFFSET:
            res = var_direct(right.symbol);
            if (right.offset) {
                res = eval_expr(block, IR_OP_SUB, res, var_long((long) right.offset));
            }
            break;
        case IMMEDIATE:
            error("Address of immediate is not supported.");
            exit(1);
    }
    return res;
}

/* Evaluate *a.
 * If OFFSET: *(*a'), double deref, evaluate the deref of a', and create 
 *     a new offset variable.
 * If DIRECT: Create a new offset variable, no evaluation.
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
            res = var_offset(var.symbol, 0);
            break;
        case OFFSET:
            if (var.offset) {
                var = eval_expr(block, IR_OP_SUB, 
                    var_direct(var.symbol),
                    var_long((long) var.offset));
            }
            temp = sym_temp(type_deref(var.symbol->type));
            res = var_offset(temp, 0);

            op.type = IR_DEREF;
            op.a = res;
            op.b = var;

            ir_append(block, op);
            break;
        case IMMEDIATE:
            error("Dereferenced immediate is not supported.");
            exit(1);
            break;
    }
    return res;
}

/* Evaluate a = b.
 * Restrictions on a: OFFSET and DIRECT is ok, but IMMEDIATE is not.
 * Restrictions on b:
 *     DIRECT and IMMEDIATE is ok. For OFFSET, put in an explicit evaluation
 *     before the assignment, so that we don't get (*a') = (*b'), but instead
 *     t1 = *(b'); (*a') = t1;
 *     Use eval_deref, pretending to evaluate *(*a') and then override offset
 *     flag to make it a direct reference. This works because eval_deref
 *     returns an l-value that is not actually causing any ir ops to be added.
 * 
 * Resulting op_t always has a direct or immediate value as rhs. Return value
 * is the value of b.
 */
var_t
eval_assign(block_t *block, var_t target, var_t var)
{
    op_t op;

    if (target.kind == IMMEDIATE) {
        error("Cannot assign to immediate value.");
        exit(1);
    }
    if (var.kind == OFFSET) {
        var = eval_deref(block, var);
        var = var_direct(var.symbol);
    }
    /* NB: missing type checking! */
    op.type = IR_ASSIGN;
    op.a = target;
    op.b = var;

    ir_append(block, op);
    return var;
}

var_t
eval_call(block_t *block, var_t func)
{
    op_t op;
    var_t res;
    const symbol_t *temp;

    if (func.type->next->type == VOID_T) {
        res = var_void();
    } else {
        temp = sym_temp(func.type->next);
        res = var_direct(temp);
    }

    op.type = IR_CALL;
    op.a = res;
    op.b = func;
    ir_append(block, op);

    return res;
}

void
param(block_t *block, var_t p)
{
    op_t op;
    op.type = IR_PARAM;
    op.a = p;
    ir_append(block, op);
}
