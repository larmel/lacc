#ifndef PARSE_H
#define PARSE_H

#include <lacc/ir.h>

/* Parse input for the next function or object definition, or NULL on
 * end of input. Borrows memory.
 */
struct definition *parse(void);

/* Initialize a definition and empty control flow graph for symbol,
 * which must be of type SYM_DEFINITION.
 */
struct definition *cfg_init(const struct symbol *sym);

/* Create basic block associated with control flow graph of given
 * definition.
 */
struct block *cfg_block_init(struct definition *def);

/* Create temporary variable for evaluation. Added to current function
 * definition context, can only be called while parsing a function.
 */
struct var create_var(struct definition *def, const struct typetree *type);

#endif
