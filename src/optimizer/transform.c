#include "transform.h"

#include <lacc/type.h>
#include <assert.h>

static int var_equal(struct var a, struct var b)
{
    return type_equal(a.type, b.type)
        && a.symbol == b.symbol
        && a.kind == b.kind
        && a.field_width == b.field_width
        && a.field_offset == b.field_offset
        /* no compare of immediate numeric value, or lvalue. */
        && a.offset == b.offset;
}

/*
 * Look at a pair of IR operations, and determine if they can be merged
 * to a single assignment. This patterns is recurring when assigning to
 * a temporary variable, which is used only once.
 *
 *  s1: t1 = l <expr> r
 *  s2: t2 = t1
 *
 */
static int can_merge(
    const struct block *block,
    const struct statement s1,
    const struct statement s2,
    int n)
{
    return s1.st == IR_ASSIGN
        && s2.st == IR_ASSIGN
        && is_identity(s2.expr)
        && var_equal(s1.t, s2.expr.l)
        && type_equal(s1.t.type, s2.t.type)
        && s1.t.kind == DIRECT
        && !is_live(s1.t.symbol, block, n);
}

int merge_chained_assignment(struct block *block)
{
    int i = 1, n = 0;
    struct statement s1, s2;

    if (array_len(&block->code) > 1) {
        s1 = array_get(&block->code, 0);
        while (i < array_len(&block->code)) {
            n += 1;
            s2 = array_get(&block->code, i);
            if (can_merge(block, s1, s2, n)) {
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
