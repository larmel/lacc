#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "liveness.h"
#include "optimize.h"

#include <assert.h>

/*
 * Set bit for symbol definitely written through operation. Unless used
 * in right hand side expression, this can be removed from in-liveness.
 *
 * Only safe to say object is written when the whole object is actually
 * overwritten. Consider only basic integral types.
 *
 * Pointers can point to anything, so we cannot say for sure what is
 * written.
 */
static unsigned long set_def_bit(struct var var)
{
    switch (var.kind) {
    case DIRECT:
        if (is_scalar(var.value.symbol->type) && var.value.symbol->index) {
            return 1ul << (var.value.symbol->index - 1);
        }
    default:
        return 0;
    }
}

/*
 * Set bit for symbol possibly read through operation. This set must be
 * part of in-liveness.
 *
 * Pointers can point to anything, so assume everything is touched.
 */
static unsigned long set_use_bit(struct var var)
{
    switch (var.kind) {
    case DEREF:
        return 0xFFFFFFFFFFFFFFFFul;
    case DIRECT:
    case ADDRESS:
        if (is_object(var.value.symbol->type)) {
            assert(var.value.symbol->index);
            return 1ul << (var.value.symbol->index - 1);
        }
        break;
    case IMMEDIATE:
        if (var.is_symbol) {
            assert(var.value.symbol->symtype == SYM_LITERAL
                || var.value.symbol->symtype == SYM_CONSTANT);
            assert(var.value.symbol->index);
            return 1ul << (var.value.symbol->index - 1);
        }
        break;
    }

    return 0;
}

static unsigned long use(const struct expression *expr)
{
    unsigned long r = 0ul;

    switch (expr->op) {
    default:
        r |= set_use_bit(expr->r);
    case IR_OP_CAST:
    case IR_OP_NOT:
    case IR_OP_NEG:
    case IR_OP_CALL:
    case IR_OP_VA_ARG:
        r |= set_use_bit(expr->l);
        break;
    }

    return r;
}

static int is_or_has_pointer(Type type)
{
    int i;
    struct member *m;

    if (is_pointer(type)) {
        return 1;
    }

    if (is_struct_or_union(type)) {
        for (i = 0; i < nmembers(type); ++i) {
            m = get_member(type, i);
            if (is_or_has_pointer(m->type)) {
                return 1;
            }
        }
    }

    return 0;
}

/*
 * Consider special case of sending a pointer into a function. Assume
 * then that anything can be used.
 */
static unsigned long uses(const struct statement *s)
{
    struct var t;
    unsigned long r;

    assert(s->st != IR_ASM);
    r = use(&s->expr);
    switch (s->st) {
    case IR_ASSIGN:
        if (s->t.kind == DEREF && s->t.is_symbol) {
            t = s->t;
            t.kind = DIRECT;
            r |= set_use_bit(t);
        }
        break;
    case IR_PARAM:
        if (is_or_has_pointer(s->expr.type)) {
            r |= 0xFFFFFFFFFFFFFFFFul;
        }
    default:
        break;
    }

    return r;
}

static unsigned long def(const struct statement *s)
{
    switch (s->st) {
    case IR_ASSIGN:
        return set_def_bit(s->t);
    case IR_ASM:
        assert(0);
    default:
        return 0ul;
    }
}

INTERNAL int live_variable_analysis(struct block *block)
{
    int i;
    unsigned long top;
    struct statement *prev, *next;

    top = block->in;

    /* Transfer liveness from children. */
    if (block->jump[0]) {
        block->out = block->jump[0]->in;
        if (block->jump[1]) {
            block->out |= block->jump[1]->in;
        }
    } else {
        block->out = 0l;
    }

    /* Go through all statements. Extra edge for branch and return. */
    if (array_len(&block->code)) {
        prev = &array_back(&block->code);
        prev->out = block->out;
        if (block->jump[1] || block->has_return_value) {
            prev->out |= use(&block->expr);
        }

        for (i = array_len(&block->code) - 2; i >= 0; --i) {
            next = prev;
            prev = &array_get(&block->code, i);
            prev->out = (next->out & ~def(next)) | uses(next);
        }

        block->in = (prev->out & ~def(prev)) | uses(prev);
    } else {
        block->in = block->out;
        if (block->jump[1] || block->has_return_value) {
            block->in |= use(&block->expr);
        }
    }

    return top != block->in;
}
