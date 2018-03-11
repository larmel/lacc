#ifndef OPTIMIZE_H
#define OPTIMIZE_H

#include "liveness.h"

#include <lacc/ir.h>

/* Set to non-zero to enable optimization. */
INTERNAL void push_optimization(int level);

/*
 * Do data flow analysis and perform optimizations on the intermediate
 * representation. Leaves the definition in a semantically equivalent,
 * and hopefully more consise, state.
 */
INTERNAL void optimize(struct definition *def);

/* Disable previously set optimization, cleaning up resources. */
INTERNAL void pop_optimization(void);

#endif
