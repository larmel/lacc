#ifndef CFG_H
#define CFG_H

#include <lacc/ir.h>

#include <stddef.h>

/* Current declaration, accessed for creating new blocks or adding init code
 * in head block.
 */
extern struct cfg current_cfg;

/* Create a variable of the given type, returning a direct reference to a new
 * symbol.
 */
struct var create_var(const struct typetree *type);

/* Initialize a new control flow graph structure, updating current_cfg.
 */
void cfg_init_current(void);

/* Initialize a CFG block with a unique jump label. Borrows memory.
 */
struct block *cfg_block_init(void);

/* Add a 3-address code operation to the block. Code is kept in a separate list
 * for each block.
 */
void cfg_ir_append(struct block *block, struct op op);

/* Add local variable to symbol list, required for assembly.
 */
void cfg_register_local(struct symbol *symbol);

/* Add function parameter to symbol list, required for assembly.
 */
void cfg_register_param(struct symbol *symbol);

#endif
