#ifndef OPTIMIZE_H
#define OPTIMIZE_H

#include "liveness.h"

#include <lacc/ir.h>

/*
 * Each basic block in the intermediate representation is associated
 * with a list of dataflow nodes, holding information used throughout
 * optimization passes.
 */
struct dataflow {
    union liveness live;
};

/* Number of dataflow edges there are in a basic block. */
#define edges(block) \
    (array_len(&(block)->code) \
        + ((block)->jump[1] || block->has_return_value) + 1)

/* Number of IR operations in a basic block */
#define operations(block) (edges(block) - 1)

/* Set to non-zero to enable optimization. */
void push_optimization(int level);

/*
 * Do data flow analysis and perform optimizations on the intermediate
 * representation. Leaves the definition in a semantically equivalent,
 * and hopefully more consise, state.
 */
void optimize(struct definition *def);

/* Disable previously set optimization, cleaning up resources. */
void pop_optimization(void);

#endif
