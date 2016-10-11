#include "liveness.h"
#include "optimize.h"

#include <assert.h>

#define IN(block, i) (block)->flow[i].live.bits
#define OUT(block, i) IN(block, i + 1)

/* Set bit in position of variable if referencing a symbol. */
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

/* Set bits for symbols referenced in expression. */
static unsigned long use(struct expression expr)
{
    unsigned long r = 0ul;

    switch (expr.op) {
    default:
        r |= set_var_bit(expr.r);
    case IR_OP_CAST:
    case IR_OP_NOT:
    case IR_OP_CALL:
    case IR_OP_VA_ARG:
        r |= set_var_bit(expr.l);
        break;
    }

    return r;
}

/* Set bits for symbols used in statement. */
static unsigned long uses(struct statement s)
{
    unsigned long r = use(s.expr);

    if (s.st == IR_ASSIGN) {
        if (s.t.kind == DEREF && s.t.symbol) {
            s.t.kind = DIRECT;
            r |= set_var_bit(s.t);
        }
    }

    return r;
}

/* Set bit for symbol written through operation. */
static unsigned long def(struct statement s)
{
    return (s.st == IR_ASSIGN) ? set_var_bit(s.t) : 0ul;
}

int live_variable_analysis(struct block *block)
{
    int i, n = operations(block);
    unsigned long top;
    struct statement code;

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
        IN(block, n - 1) = OUT(block, n - 1) | use(block->expr);
    }

    /* Go through normal ir operations. */
    for (i = array_len(&block->code) - 1; i >= 0; --i) {
        code = array_get(&block->code, i);
        IN(block, i) = (OUT(block, i) & ~def(code)) | uses(code);
    }

    return top != IN(block, 0);
}

int is_live(const struct symbol *sym, const struct block *block, int n)
{
    if (block->flow) {
        assert(n >= 0);
        assert(sym->index);
        return (OUT(block, n) & (1ul << (sym->index - 1)))
            || sym->linkage != LINK_NONE;
    }

    return 1;
}
