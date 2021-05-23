#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "transform.h"
#include "liveness.h"

#include <lacc/type.h>
#include <assert.h>

static int var_equal(struct var a, struct var b)
{
    return type_equal(a.type, b.type)
        && ((!a.is_symbol && !b.is_symbol) || a.value.symbol == b.value.symbol)
        && a.kind == b.kind
        && a.field_width == b.field_width
        && a.field_offset == b.field_offset
        /* no compare of immediate numeric value, or lvalue. */
        && a.offset == b.offset;
}

/*
 * Look at a pair of IR operations, and determine if they can be merged
 * to a single assignment:
 *
 *  s1: t1 = a + b
 *  s2: t2 = t1
 *
 * is replaces by:
 *
 *  s1: t2 = a + b
 *
 */
static int can_merge(
    const struct block *block,
    const struct statement s1,
    const struct statement s2)
{
    return s1.st == IR_ASSIGN
        && s2.st == IR_ASSIGN
        && is_identity(s2.expr)
        && var_equal(s1.t, s2.expr.l)
        && type_equal(s1.t.type, s2.t.type)
        && s1.t.kind == DIRECT
        && s1.t.value.symbol->linkage == LINK_NONE
        && !is_field(s1.t)
        && !is_live_after(s1.t.value.symbol, &s2);
}

INTERNAL int merge_chained_assignment(struct block *block)
{
    int i = 1;
    struct statement s1, s2;

    if (array_len(&block->code) > 1) {
        s1 = array_get(&block->code, 0);
        while (i < array_len(&block->code)) {
            s2 = array_get(&block->code, i);
            if (can_merge(block, s1, s2)) {
                s1.t = s2.t;
                array_get(&block->code, i - 1) = s1;
                array_erase(&block->code, i);
                s1 = array_get(&block->code, i - 1);
            } else {
                s1 = array_get(&block->code, i);
                i += 1;
            }
        }
    }

    return 0;
}

INTERNAL int dead_store_elimination(struct block *block)
{
    int i, c;
    struct statement *st;

    for (i = 0, c = 0; i < array_len(&block->code); ++i) {
        st = &array_get(&block->code, i);
        if (st->st == IR_ASSIGN
            && st->t.kind == DIRECT
            && !is_live_after(st->t.value.symbol, st)
            && st->t.value.symbol->linkage == LINK_NONE)
        {
            c += 1;
            if (has_side_effects(st->expr)) {
                st->st = IR_EXPR;
            } else {
                array_erase(&block->code, i);
                i -= 1;
            }
        }
    }

    return c;
}
