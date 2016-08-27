#include "liveness.h"
#include "optimize.h"

#include <assert.h>

#define IN(block, i) (block)->flow[i].live.bits
#define OUT(block, i) IN(block, i + 1)

/*
 * Set bit in position of variable if referencing a symbol.
 */
static unsigned long set_var_bit(struct var var)
{
    switch (var.kind) {
    case DEREF:
        /* Assume pointer can point to anything. */
        return 0xFFFFFFFFFFFFFFFFul;
    case DIRECT:
    case ADDRESS:
        if (is_object(&var.symbol->type)) {
            assert(var.symbol->index);
            return 1ul << (var.symbol->index - 1);
        }
    default:
        return 0;
    }
}

/*
 * Set bits for symbols referenced through operation.
 */
static unsigned long use(const struct op *op)
{
    unsigned long r = 0;

    switch (OPERAND_COUNT(op->type)) {
    case 3:
        r |= set_var_bit(op->c);
    case 2:
        r |= set_var_bit(op->b);
        break;
    default:
        if (op->type == IR_PARAM || op->type == IR_VA_START) {
            r |= set_var_bit(op->a);
        }
    }

    return r;
}

/*
 * Set bit for symbol written through operation.
 */
static unsigned long def(const struct op *op)
{
    return (op->type != IR_PARAM && op->type != IR_VA_START)
        ? set_var_bit(op->a)
        : 0;
}

int live_variable_analysis(struct block *block)
{
    int i, n = operations(block);
    unsigned long top;
    struct op code;

    top = IN(block, 0);

    /* Transfer liveness from children. */
    if (block->jump[0]) {
        OUT(block, n - 1) |= IN(block->jump[0], 0);
        if (block->jump[1]) {
            OUT(block, n - 1) |= IN(block->jump[1], 0);
        }
    }

    /* if <expr> goto <label>, and return <expr>, get extra edge. */
    if (block->jump[1] || block->has_return_value) {
        IN(block, n - 1) |= set_var_bit(block->expr);
    }

    /* Go through normal ir operations. */
    for (i = array_len(&block->code) - 1; i >= 0; --i) {
        code = array_get(&block->code, i);
        IN(block, i) = (OUT(block, i) & ~def(&code)) | use(&code);
    }

    return top != IN(block, 0);
}

int is_live(const struct symbol *sym, const struct block *block, int n)
{
    if (block->flow) {
        assert(n >= 0);
        assert(sym->index);
        return (OUT(block, n) & (1ul << (sym->index - 1))) != 0;
    }

    return 1;
}
