#include "ir.h"
#include "symbol.h"

static const char *
mklabel()
{
    static int n;

    char *name = malloc(sizeof(char) * 16);
    snprintf(name, 12, ".L%d", n++);

    return name;
}

struct op *
ir_init(enum optype type, const struct symbol *a, const struct symbol *b, const struct symbol *c)
{
    struct op *op = malloc(sizeof(struct op));
    op->type = type;
    op->a = a;
    op->b = b;
    op->c = c;
    return op;
}

/* Initialize a CFG block, with a unique jump label. */
struct block *
block_init()
{
    struct block *block = calloc(1, sizeof(struct block));
    block->label = mklabel();
    return block;
}

/* Add a 3-address code operation to the block. */
void
ir_append(struct block* block, struct op op)
{
    block->n += 1;
    block->code = realloc(block->code, sizeof(struct op) * block->n);
    block->code[block->n - 1] = op;
    return;
}
