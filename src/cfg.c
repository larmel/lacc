#include "ir.h"
#include "error.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

DEFINE_LIST_IMPLEMENTATION(sym_list, struct symbol *)

const char *mklabel(void)
{
    static int n;

    char *name = malloc(sizeof(char) * 16);
    snprintf(name, 12, ".L%d", n++);

    return name;
}

struct decl *cfg_create()
{
    struct decl *decl = calloc(1, sizeof(*decl));
    assert(decl);
    return decl;
}

struct block *cfg_block_init(struct decl *decl)
{
    struct block *block;

    assert(decl);
    block = calloc(1, sizeof(*block));
    block->label = mklabel();

    if (decl->size == decl->capacity) {
        decl->capacity += 16;
        decl->nodes =
            realloc(decl->nodes, decl->capacity * sizeof(*decl->nodes));
    }
    decl->nodes[decl->size++] = block;

    return block;
}

void cfg_finalize(struct decl *decl)
{
    assert(decl);
    if (decl->capacity) {
        int i;
        for (i = 0; i < decl->size; ++i) {
            struct block *block = decl->nodes[i];
            if (block->n) free(block->code);
            if (block->label) free((void *) block->label);
            free(block);
        }
        free(decl->nodes);
    }
    free(decl);
}

void cfg_ir_append(struct block *block, struct op op)
{
    if (block) {
        block->n += 1;
        block->code = realloc(block->code, sizeof(struct op) * block->n);
        block->code[block->n - 1] = op;
    }
}
