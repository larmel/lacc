#ifndef IR_H
#define IR_H

struct symbol;

typedef enum optype {
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

typedef struct op {
    enum optype type;

    /* operands, three address code */
    const struct symbol *a;
    const struct symbol *b;
    const struct symbol *c;
} op_t;

/* CFG block */
typedef struct block {
    /* A unique jump target label */
    const char *label;

    /* realloc-able list of 3-address code operations */
    struct op *code;
    unsigned n;

    /* Value to evaluate in branch conditions, or return value */
    const struct symbol *expr;

    /* Branch targets.
     * - (NULL, NULL): Terminal node, return expr from function.
     * - (x, NULL)   : Unconditional jump, f.ex break, goto, or bottom of loop.
     * - (x, y)      : Branch, false and true targets respectively, from evaluating expr.
     */
    const struct block *jump[2];
} block_t;

typedef struct function {
    const struct symbol *symbol;

    /* Number of bytes to allocate to local variables on stack. */
    int locals_size;

    block_t *body;
} function_t;

struct block *block_init();

void ir_append(struct block *, struct op);


#endif
