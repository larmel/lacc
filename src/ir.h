#ifndef IR_H
#define IR_H

#include "type.h"
#include "symbol.h"

#include <stddef.h>

/* Immediate value.
 */
typedef union value
{
    long integer;
    double real;
    const char *string;
} value_t;

/* A reference to some storage location or direct value, used in intermediate
 * representation of expressions. There are three modes:
 *
 * DIRECT: l-value or r-value reference to symbol, which must have some
 *         storage location. For array or function types, a direct reference
 *         means the corresponding memory address of the symbol.
 *         Direct references can have offsets, which evaluates to 
 *         *(&symbol + offset).
 * DEREF:  l-value or r-value reference to *(symbol + offset). Symbol must 
 *         have pointer type.
 * IMMEDIATE:
 *         r-value immediate value, with the type specified. Symbol is NULL.
 */
typedef struct variable
{
    enum { DIRECT, DEREF, IMMEDIATE } kind;
    const typetree_t *type;
    const symbol_t *symbol;
    value_t value;
    int offset;
    int lvalue;
} var_t;

/* Three address code
 */
typedef enum optype
{
    IR_ASSIGN,      /* a = b */
    IR_DEREF,       /* a = *b */
    IR_ADDR,        /* a = &b */
    IR_PARAM,
    IR_CALL,        /* a = b() */
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

typedef struct op {
    enum optype type;

    var_t a;
    var_t b;
    var_t c;
} op_t;

/* CFG block
 */
typedef struct block
{
    /* A unique jump target label */
    const char *label;

    /* realloc-able list of 3-address code operations */
    op_t *code;
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

/* Represents an external declaration list or a function definition.
 */
typedef struct decl
{
    /* Function symbol and control flow graph, or NULL if list of declarations. */
    const symbol_t *fun;
    block_t *head;
    block_t *body;

    /* Number of bytes to allocate to local variables on stack. */
    int locals_size;

    /* Store all associated nodes in a list to simplify deallocation. */
    block_t **nodes;
    size_t size;
    size_t capacity;
} decl_t;

/* Initialize a new control flow graph structure. */
decl_t *cfg_create();

/* Initialize a CFG block with a unique jump label, and associate it with the
 * provided decl_t object. Blocks and functions have the same lifecycle, and 
 * should only be freed by calling cfg_finalize. */
block_t *cfg_block_init(decl_t *);

/* Add a 3-address code operation to the block. Code is kept in a separate list
 * for each block. */
void cfg_ir_append(block_t *, op_t);

/* Release all resources related to the control flow graph. Calls free on all
 * blocks and their labels, and finally the decl_t object itself. */
void cfg_finalize(decl_t *);

/* Interface used in parser to evaluate expressions and add operations to the
 * control flow graph. */
var_t eval_expr(block_t *, optype_t, var_t, var_t);
var_t eval_addr(block_t *, var_t);
var_t eval_deref(block_t *, var_t);
var_t eval_assign(block_t *, var_t, var_t);
var_t eval_copy(block_t *, var_t);
var_t eval_call(block_t *, var_t);
void param(block_t *, var_t);

/* Expression variables. */
var_t var_direct(const symbol_t *);
var_t var_deref(const symbol_t *, int);
var_t var_string(const char *, size_t);
var_t var_long(long);
var_t var_void();

#endif
