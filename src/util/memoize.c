#include "memoize.h"

#include <assert.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

struct memo {
    const char **ids;
    size_t n;
};

struct memo *memoize_init(void)
{
    struct memo *memo = calloc(1, sizeof(*memo));
    return memo;
}

void memoize_free(struct memo *memo)
{
    assert(memo);
    if (memo->n) {
        free(memo->ids);
    }
    free(memo);
}

int is_memoized(const struct memo *memo, const char *input)
{
    int i;
    assert(input);
    assert(memo);

    for (i = 0; i < memo->n; ++i) {
        if (!strcmp(input, memo->ids[i])) {
            return 1;
        }
    }
    return 0;
}

int memoize_guard(struct memo *memo, const char *input)
{
    if (!is_memoized(memo, input)) {
        memo->n++;
        memo->ids = realloc(memo->ids, memo->n * sizeof(*memo->ids));
        memo->ids[memo->n - 1] = input;
        return 1;
    }
    return 0;
}
