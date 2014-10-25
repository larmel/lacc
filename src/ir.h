#ifndef IR_H
#define IR_H

struct symbol;

enum irtype {
    IR_ARITHMETIC,  /* a = b <op> c */
    IR_ASSIGN,      /* a = b */
    IR_DEREF,       /* a = *b */
    IR_RET          /* ret a */
};

enum iroptype {
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
};

struct block;

typedef struct irop {
    enum irtype type;
    enum iroptype optype;

    const struct symbol *a;
    const struct symbol *b;
    const struct symbol *c;

    struct block *target;
} irop_t;

/* A basic block representing a fork or join in the program control flow.
 * For example function entry points, for loops, if branches, etc. 
 * Initially, these are per function only, so not really basic blocks */
typedef struct block {
    const char *label;

    struct irop *ops;
    unsigned n;
} block_t;


block_t * mkblock(const char *);

void mkir_arithmetic(const struct symbol *, const struct symbol *, const struct symbol *, enum iroptype);
const struct symbol *ir_emit_arithmetic(enum iroptype, const struct symbol *, const struct symbol *);

void mkir_assign(const struct symbol *, const struct symbol *);

void mkir_deref(const struct symbol *, const struct symbol *);
const struct symbol *ir_emit_deref(const struct symbol *);

void mkir_ret(const struct symbol *);

#endif
