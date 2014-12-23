#include "ir.h"

#include <stdlib.h>
#include <stdio.h>

static const char *
mklabel()
{
    static int n;

    char *name = malloc(sizeof(char) * 16);
    snprintf(name, 12, ".L%d", n++);

    return name;
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
