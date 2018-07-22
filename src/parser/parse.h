#ifndef PARSE_H
#define PARSE_H

#include <lacc/ir.h>

/*
 * Parse input for the next function or object definition, or NULL on
 * end of input.
 */
INTERNAL struct definition *parse(void);

/* Create an empty control flow graph.
 *
 * This is done in declaration parsing, which needs an empty graph while
 * reading declarations. Declarations ending up defining a symbol are
 * completed with cfg_define(2), making the struct definition object
 * available in later calls to parse(0).
 *
 * Declarations that only represent a prototype should not generate any
 * code, and the graph can be thrown away with cfg_discard(1).
 */
INTERNAL struct definition *cfg_init(void);

/* Associate symbol with a function or global variable definition. */
INTERNAL void cfg_define(struct definition *def, const struct symbol *sym);

/* Release resources associated with control flow graph. */
INTERNAL void cfg_discard(struct definition *def);

/* Create a basic block associated with control flow graph. */
INTERNAL struct block *cfg_block_init(struct definition *def);

/* Free memory after all input files are processed. */
INTERNAL void parse_finalize(void);

#endif
