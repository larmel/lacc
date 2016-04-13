#ifndef PARSE_H
#define PARSE_H

#include <lacc/ir.h>

/* Parse input for the next function or object definition, or NULL on
 * end of input. Borrows memory.
 */
struct definition *parse(void);

/* Create basic block associated with control flow graph of current
 * function. Parser must be in a function context.
 */
struct block *cfg_block_init(void);

/* Create temporary variable for evaluation. Added to current function
 * definition context, can only be called while parsing a function.
 */
struct var create_var(const struct typetree *type);

struct definition *create_definition(const struct symbol *sym);

struct definition *current_func(void);

#endif
