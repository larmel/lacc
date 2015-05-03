#include "ir.h"
#include "error.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>


DEFINE_LIST_IMPLEMENTATION(sym_list, struct symbol *)

static const char *
mklabel()
{
    static int n;

    char *name = malloc(sizeof(char) * 16);
    snprintf(name, 12, ".L%d", n++);

    return name;
}

decl_t *
cfg_create()
{
    decl_t *decl = calloc(1, sizeof(decl_t));
    assert(decl);
    return decl;
}

block_t *
cfg_block_init(decl_t *decl)
{
    block_t *block;

    assert(decl);
    block = calloc(1, sizeof(block_t));
    block->label = mklabel();
    block->expr = var_void();

    if (decl->size == decl->capacity) {
        decl->capacity += 16;
        decl->nodes = realloc(decl->nodes, decl->capacity * sizeof(block_t*));
    }
    decl->nodes[decl->size++] = block;

    return block;
}

void
cfg_finalize(decl_t *decl)
{
    assert(decl);
    if (decl->capacity) {
        int i;
        for (i = 0; i < decl->size; ++i) {
            block_t *block = decl->nodes[i];
            if (block->n) free(block->code);
            if (block->label) free((void *) block->label);
            free(block);
        }
        free(decl->nodes);
    }
    free(decl);
}

void
cfg_ir_append(block_t *block, op_t op)
{
    if (block) {
        block->n += 1;
        block->code = realloc(block->code, sizeof(op_t) * block->n);
        block->code[block->n - 1] = op;
    }
}
