#ifndef EVAL_H
#define EVAL_H

#include "ir.h"

/* Expression variables.
 */
struct var var_direct(const struct symbol *sym);
struct var var_string(const char *label, size_t length);
struct var var_int(int value);
struct var var_zero(int size);

/* Interface used in parser to evaluate expressions and add operations to the
 * control flow graph.
 */
struct var eval_expr(struct block *block, enum optype op, ...);
struct var eval_addr(struct block *block, struct var var);
struct var eval_deref(struct block *block, struct var var);
struct var eval_assign(struct block *block, struct var target, struct var var);
struct var eval_copy(struct block *block, struct var var);
struct var eval_call(struct block *block, struct var var);
struct var eval_cast(struct block *b, struct var v, const struct typetree *t);
struct var eval_conditional(struct var a, struct block *b, struct block *c);
void param(struct block *, struct var);

#endif
