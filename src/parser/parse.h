#ifndef PARSE_H
#define PARSE_H

#include <lacc/ir.h>

/* Parse input for the next function or object definition. Symbol is NULL on
 * end of input. Takes ownership of memory, which must be cleaned up by calling
 * free_definition.
 */
struct definition parse(void);

/* Free allocated memory for blocks and lists associated with definition.
 */
void free_definition(struct definition def);

/* Create basic block associated with control flow graph of current function.
 * Parser must be in a function context.
 */
struct block *cfg_block_init(void);

/* Create temporary variable for evaluation. Added to current function
 * definition context, can only be called while parsing a function.
 */
struct var create_var(const struct typetree *type);

#endif
