#ifndef IR_H
#define IR_H

#include "util.h"
#include "type.h"
#include "symbol.h"

#include <stddef.h>

DECLARE_LIST_IMPLEMENTATION(sym_list, struct symbol *)

/* A reference to some storage location or direct value, used in intermediate
 * representation of expressions.
 */
struct var
{
    const struct typetree *type;
    const struct symbol *symbol;

    enum {
        /* l-value or r-value reference to symbol, which must have some storage
         * location. Offset evaluate to *(&symbol + offset). Offset in bytes,
         * not pointer arithmetic. */
        DIRECT,
        /* l-value or r-value reference to *(symbol + offset). Symbol must have
         * pointer type. Offset in bytes, not pointer arithmetic. */
        DEREF,
        /* r-value immediate, with the type specified. Symbol is NULL. */
        IMMEDIATE
    } kind;

    union value {
        long integer;
        double real;
        const char *string;
    } value;

    int offset;
    int lvalue;
};

/* Three address code optypes. Use second least significant hex digit to mark
 * number of operands. 
 */
enum optype
{
    IR_PARAM = 0x00,    /* param a    */
    IR_ASSIGN = 0x10,   /* a = b      */
    IR_DEREF,           /* a = *b     */
    IR_ADDR,            /* a = &b     */
    IR_CALL,            /* a = b()    */
    IR_CAST,            /* a = (T) b  */
    IR_OP_ADD = 0x20,   /* a = b + c  */
    IR_OP_SUB,
    IR_OP_MUL,
    IR_OP_DIV,
    IR_OP_MOD,
    IR_OP_BITWISE_AND,
    IR_OP_BITWISE_OR,
    IR_OP_BITWISE_XOR,
    IR_OP_EQ,           /* a = b == c */
    IR_OP_GE,           /* a = b >= c */
    IR_OP_GT            /* a = b > c  */
};

#define NOPERANDS(t) ((int)(t) & 0x20 ? 2 : 1)

/* CFG block.
 */
struct block
{
    /* A unique jump target label. */
    const char *label;

    /* Realloc-able list of 3-address code operations. */
    struct op {
        enum optype type;
        struct var a;
        struct var b;
        struct var c;
    } *code;

    int n;

    /* Value to evaluate in branch conditions, or return value. Also used for
     * return value from expression parsing rules, as a convenience. The
     * decision on whether this block is a branch or not is done purely based
     * on the jump target list. */
    struct var expr;

    /* Branch targets.
     * - (NULL, NULL): Terminal node, return expr from function.
     * - (x, NULL)   : Unconditional jump, f.ex break, goto, or bottom of loop.
     * - (x, y)      : False and true branch targets, respectively.
     */
    const struct block *jump[2];
};

/* Represents an external declaration list or a function definition.
 */
struct decl
{
    /* Function symbol or NULL if list of declarations. */
    const struct symbol *fun;
    struct block *head;
    struct block *body;

    /* Number of bytes to allocate to local variables on stack. */
    int locals_size;

    /* Store all symbols associated with a function declaration. */
    struct sym_list params;
    struct sym_list locals;

    /* Store all associated nodes in a list to simplify deallocation. */
    struct block **nodes;
    size_t size;
    size_t capacity;
};

/* Initialize a new control flow graph structure. */
struct decl *cfg_create();

/* Initialize a CFG block with a unique jump label, and associate it with the
 * provided decl object. Blocks and functions have the same lifecycle, and 
 * should only be freed by calling cfg_finalize. */
struct block *cfg_block_init(struct decl *);

/* Add a 3-address code operation to the block. Code is kept in a separate list
 * for each block. */
void cfg_ir_append(struct block *, struct op);

/* Release all resources related to the control flow graph. Calls free on all
 * blocks and their labels, and finally the struct decl object itself. */
void cfg_finalize(struct decl *);

#endif
