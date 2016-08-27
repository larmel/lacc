#ifndef LIVENESS_H
#define LIVENESS_H

#include <lacc/ir.h>

union liveness {
    unsigned long bits;
};

/*
 * Compute liveness of each variable on every edge, before and after
 * every ir operation.
 */
int live_variable_analysis(struct block *block);

#endif
