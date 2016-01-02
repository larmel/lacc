#ifndef DECLARATION_H
#define DECLARATION_H

#include <lacc/ir.h>

/* Create basic block associated with control flow graph of current function.
 * Parser must be in a function context.
 */
struct block *cfg_block_init(void);

/* Create temporary variable for evaluation. Added to current function
 * definition context, can only be called while parsing a function.
 */
struct var create_var(const struct typetree *type);

struct definition *current_func(void);

struct block *declaration(struct block *parent);

struct typetree *declarator(struct typetree *base, const char **symbol);

struct typetree *declaration_specifiers(int *stc);

#endif
