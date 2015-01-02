#ifndef IR_H
#define IR_H

#include "symbol.h"

#include <stddef.h>

typedef enum optype
{
    IR_ASSIGN,      /* a = b */
    IR_DEREF,       /* a = *b */
    IR_OP_ADD,
    IR_OP_SUB,
    IR_OP_MUL,
    IR_OP_DIV,
    IR_OP_MOD,
    IR_OP_LOGICAL_AND,
    IR_OP_LOGICAL_OR,
    IR_OP_BITWISE_AND,
    IR_OP_BITWISE_OR,
    IR_OP_BITWISE_XOR
} optype_t;

/* Three address code */
typedef struct op {
    enum optype type;

    var_t a;
    var_t b;
    var_t c;
} op_t;

/* CFG block */
typedef struct block
{
    /* A unique jump target label */
    const char *label;

    /* realloc-able list of 3-address code operations */
    struct op *code;
    unsigned n;

    /* Value to evaluate in branch conditions, or return value */
    var_t expr;

    /* Branch targets.
     * - (NULL, NULL): Terminal node, return expr from function.
     * - (x, NULL)   : Unconditional jump, f.ex break, goto, or bottom of loop.
     * - (x, y)      : Branch, false and true targets respectively, from evaluating expr.
     */
    const struct block *jump[2];
} block_t;

typedef struct function
{
    const struct symbol *symbol;

    /* Number of bytes to allocate to local variables on stack. */
    int locals_size;

    block_t *body;

    /* Store all associated nodes in a list to simplify deallocation. */
    block_t **nodes;
    size_t size;
    size_t capacity;
} function_t;


/* Release all resources related to the control flow graph. Calls free on all 
 * blocks and their labels, and finally the function itself. */
void cfg_finalize(function_t *);

/* Initialize a control flow graph. All following block_init invocations are 
 * associated with the last created cfg (function). */
function_t *cfg_create();

/* Initialize a CFG block with a unique jump label, and associate it with the
 * current (last created) function_t object. Blocks and functions have the same
 * lifecycle, and should only be freed by calling cfg_finalize. */
block_t *block_init();

/* Add a 3-address code operation to the block. Code is kept in a separate list
 * for each block. */
void ir_append(block_t *, op_t);

var_t evaluate(block_t *, optype_t, var_t, var_t);
var_t evalindex(block_t *block, var_t, var_t);

#endif
