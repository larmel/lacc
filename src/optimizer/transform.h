#ifndef TRANSFORM_H
#define TRANSFORM_H

#include <lacc/ir.h>

/*
 * Optimization pass which joins together paired assignment expression
 * to a temporary and a variable into a single statement.
 *
 *   .t1 = a + 1
 *   b = .t1
 *
 * If .t1 is not live after the second assignment, the sequence can be
 * converted to the following:
 *
 *   b = a + 1
 *
 * This rewriting can drastically reduce the number of temporaries used,
 * as evaluation will generate new variables for each partial result.
 */
int merge_chained_assignment(struct block *block);

#endif
