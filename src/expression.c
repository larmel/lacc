/* Construct op_t operations and temp variables based on symbols and optypes. */

#include "ir.h"
#include "symbol.h"

const symbol_t *
evaluate(block_t *block, optype_t optype, const symbol_t *left, const symbol_t *right)
{
    op_t op;
    const symbol_t *res;

    switch (optype) {
        case IR_ASSIGN:
            op.a = left;
            op.b = right;
            break;
        case IR_OP_LOGICAL_AND:
        case IR_OP_LOGICAL_OR:
        case IR_OP_BITWISE_OR:
        case IR_OP_BITWISE_XOR:
        case IR_OP_BITWISE_AND:
        case IR_OP_ADD:
        case IR_OP_SUB:
        case IR_OP_MUL:
        case IR_OP_DIV:
        case IR_OP_MOD:
        default:
            res = sym_temp(type_combine(left->type, right->type));
            op.a = res;
            op.b = left;
            op.c = right;
            break;
    }

    op.type = optype;
    ir_append(block, op);

    return res;
}

/* Evaluate a[b]. */
const symbol_t *
evalindex(block_t *block, const symbol_t *array, const symbol_t *expr)
{
    const symbol_t *size;
    const symbol_t *offset;

    size = sym_number_init((long) array->type->size);

    offset = evaluate(block, IR_OP_MUL, expr, size);
    return evaluate(block, IR_OP_ADD, array, offset);
}
