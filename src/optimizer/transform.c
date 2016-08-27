#include "transform.h"

#include <lacc/typetree.h>
#include <assert.h>

static int var_equal(struct var a, struct var b)
{
    return type_equal(a.type, b.type)
        && a.symbol == b.symbol
        && a.kind == b.kind
        && a.width == b.width
        /* no compare of immediate numeric value. */
        && a.offset == b.offset;
}

/*
 * Look at a pair of IR operations, and determine if they can be merged
 * to a single assignment. This patterns is recurring when assigning to
 * a temporary variable, which is used only once.
 *
 *  s1: t = a <expr> b
 *  s2: c = t
 *
 */
static int can_merge(
    const struct block *block,
    const struct op *s1,
    const struct op *s2,
    int n)
{
    if (OPERAND_COUNT(s1->type) != 3 || s2->type != IR_ASSIGN) {
        return 0;
    }

    return var_equal(s1->a, s2->b)
        && type_equal(s1->a.type, s2->a.type)
        && s1->a.kind == DIRECT
        && !is_live(s1->a.symbol, block, n);
}

int merge_chained_assignment(struct block *block)
{
    int i = 1, n = 0;
    struct op *s1, *s2;

    if (array_len(&block->code) > 1) {
        s1 = &array_get(&block->code, 0);
        while (i < array_len(&block->code)) {
            n += 1;
            s2 = &array_get(&block->code, i);
            if (can_merge(block, s1, s2, n)) {
                s1->a = s2->a;
                array_erase(&block->code, i);
                s1 = &array_get(&block->code, i - 1);
            } else {
                s1 = &array_get(&block->code, i);
                i += 1;
            }
        }
    }

    return 0;
}
