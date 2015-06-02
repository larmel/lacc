#ifndef EVAL_H
#define EVAL_H

#include "ir.h"

/* Expression variables. */
struct var var_direct(const struct symbol *);
struct var var_deref(const struct symbol *, int);
struct var var_string(const char *, size_t);
struct var var_int(int);
struct var var_void(void);

/* Interface used in parser to evaluate expressions and add operations to the
 * control flow graph. */
struct var eval_expr(struct block *, enum optype, ...);
struct var eval_addr(struct block *, struct var);
struct var eval_deref(struct block *, struct var);
struct var eval_assign(struct block *, struct var, struct var);
struct var eval_copy(struct block *, struct var);
struct var eval_call(struct block *, struct var);
struct var eval_cast(struct block *, struct var,
                          const struct typetree *);
void param(struct block *, struct var);

#endif
