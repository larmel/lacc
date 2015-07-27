#ifndef EVAL_H
#define EVAL_H

#include "ir.h"

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

struct var eval__builtin_va_start(struct block *block, struct var arg);

struct var eval__builtin_va_arg(
    struct block *block,
    struct var arg,
    const struct typetree *type);

#endif
