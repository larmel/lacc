/* Construct op_t operations and temp variables based on symbols and optypes. */

#include "ir.h"
#include "symbol.h"

var_t
evaluate(block_t *block, optype_t optype, var_t left, var_t right)
{
    op_t op;
    var_t res;
    const symbol_t *temp;

    switch (optype) {
        case IR_ASSIGN:
            op.a = left;
            op.b = right;
            res = left;
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
            temp = sym_temp(type_combine(left.type, right.type));
            res = var_direct(temp);
            op.a = res;
            op.b = left;
            op.c = right;
            break;
    }

    op.type = optype;
    ir_append(block, op);

    return res;
}

/* Evaluate a[b] = *(a + b). */
var_t
evalindex(block_t *block, var_t array, var_t expr)
{
    var_t size;
    var_t offset;
    var_t address;

    /* For repeated indexes, a[b][c] => *(a + b)[c] => *(a + b + c), reset the 
     * l-value deref.
     * todo: Revisit the type reset, should preserve sub-arrays in temp vars. */
    if (array.kind == OFFSET) {
        array.kind = DIRECT;
        array.type = array.symbol->type;
    }

    size = var_long((long) array.type->size);

    offset  = evaluate(block, IR_OP_MUL, expr, size);
    address = evaluate(block, IR_OP_ADD, array, offset);

    address.kind = OFFSET;
    address.type = type_deref(address.symbol->type);
    address.offset = 0;

    return address;
}
