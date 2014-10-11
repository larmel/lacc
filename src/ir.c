#include "lcc.h"

#include <stdio.h>
#include <string.h>

enum irtype {
    IR_ADD,     /* t1 = t2 + t3 */
    IR_ASSIGN,  /* t1 = t2 */
    IR_RET      /* ret t1 */
};

struct block;

typedef struct {
    enum irtype type;

    symbol_t *a;
    symbol_t *b;
    symbol_t *c;

    struct block *target;
} irop_t;

/* A basic block representing a fork or join in the program control flow.
 * For example function entry points, for loops, if branches, etc. 
 * Initially, these are per function only, so not really basic blocks */
typedef struct block {
    const char *label;

    irop_t *ops;
    unsigned n;
} block_t;


/* Hold program representation as a list of blocks (functions)
 * Must store stable pointers (no realloc), as jump targets are
 * stored in ir operations */
static block_t **blocks;
static int length;
static int cap;

static irop_t *
allocirop()
{
    block_t *block;
    if (!length)  {
        fprintf(stderr, "No block to add to!\n");
        exit(0);
    }
    block = blocks[length - 1];
    block->n++;
    block->ops = realloc(block->ops, sizeof(irop_t) * block->n);
    return &block->ops[block->n - 1];
}

/* called from parsing, generate a new block */
block_t *
mkblock(const char *label)
{
    block_t *block = calloc(1, sizeof(block_t));
    block->label = label;
    if (length == cap) {
        cap += 32;
        blocks = realloc(blocks, sizeof(block_t*) * cap);
    }
    blocks[length++] = block;
    return block;
}

/* add new ir operations to the current block */
irop_t *
mkir_add(symbol_t *a, symbol_t *b, symbol_t *c)
{
    irop_t *op = allocirop();
    op->type = IR_ADD;
    op->a = a;
    op->b = b;
    op->c = c;
    return op;
}

irop_t *
mkir_assign(symbol_t *a, symbol_t *b)
{
    irop_t *op = allocirop();
    op->type = IR_ASSIGN;
    op->a = a;
    op->b = b;
    return op;
}

void
compile()
{
    node_t *declaration;
    push_scope();

    while ((declaration = parse()) != NULL) {
        ;
    }

    pop_scope();
}

/* should do this in dot format */
void
printir(FILE *file)
{
    int i, j;
    for (i = 0; i < length; ++i) {
        fprintf(file, "%s:\n", blocks[i]->label);
        for (j = 0; j < blocks[i]->n; ++j) {
            irop_t *op = &blocks[i]->ops[j];
            switch (op->type) {
                case IR_ADD:
                    fprintf(file, "%s = %s + %s\n", op->a->name, op->b->name, op->c->name);
                    break;
                case IR_ASSIGN:
                    fprintf(file, "%s = %s\n", op->a->name, op->b->name);
                    break;
                case IR_RET:
                    fprintf(file, "ret %s\n", op->a == NULL ? "" : op->a->name);
                    break;
            }
        }
    }
}
