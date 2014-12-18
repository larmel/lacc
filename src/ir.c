#include "ir.h"
#include "symbol.h"

static const char *
mklabel()
{
    static int n;

    /* NB: this currently does not care about collisions with function names */
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

/* Initialize a CFG block, with either some label for function name, or NULL
 * to generate a new jump label. */
struct block *
block_init(const char *label)
{
    struct block *block = calloc(1, sizeof(struct block));
    block->label = (label == NULL) ? mklabel() : label;
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


/* debug stuff */

void
output_block(const block_t *block)
{
    int i;
    if (block == NULL) {
        return;
    }
    printf("%s:\n", block->label);
    for (i = 0; i < block->n; ++i) {
        op_t op = block->code[i];
        switch (op.type) {
            case IR_ASSIGN:
                printf("\t%s = %s\n", op.a->name, op.b->name);
                break;
            case IR_DEREF:
                printf("\t%s = *%s\n", op.a->name, op.b->name);
                break;
            case IR_OP_ADD:
                printf("\t%s = %s + %s\n", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_SUB:
                printf("\t%s = %s - %s\n", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_MUL:
                printf("\t%s = %s * %s\n", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_DIV:
                printf("\t%s = %s / %s\n", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_MOD:
                printf("\t%s = %s %% %s\n", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_LOGICAL_AND:
                printf("\t%s = %s && %s\n", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_LOGICAL_OR:
                printf("\t%s = %s || %s\n", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_BITWISE_AND:
                printf("\t%s = %s & %s\n", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_BITWISE_OR:
                printf("\t%s = %s | %s\n", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_BITWISE_XOR:
                printf("\t%s = %s ^ %s\n", op.a->name, op.b->name, op.c->name);
                break;
        }
    }
    if (block->jump[0] == NULL && block->jump[1] == NULL) {
        printf("\treturn");
        if (block->expr != NULL) {
            printf(" %s", block->expr->name);
        }
        printf("\n");
    } else if (block->jump[1] != NULL) {
        printf("\tif %s goto %s\n", block->expr->name, block->jump[1]->label);
        output_block(block->jump[0]);
        output_block(block->jump[1]);
    } else {
        output_block(block->jump[0]);
    }
}
