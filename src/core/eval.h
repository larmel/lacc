#ifndef EVAL_H
#define EVAL_H

#include "ir.h"

/* Evaluate a = b <op> c, or unary expression a = <op> b
 *
 * Returns a DIRECT reference to a new temporary, or an immediate value.
 */
struct var eval_expr(struct block *block, enum optype op, ...);

/* Evaluate &a.
 */
struct var eval_addr(struct block *block, struct var var);

/* Evaluate *a.
 */
struct var eval_deref(struct block *block, struct var var);

/* Evaluate simple assignment (6.5.16.1).
 *
 *      a = b
 *
 * The type of the expression is typeof(a) after l-to-r-value conversion. The
 * result is an r-value. Operand b is converted to typeof(A) before assignment.
 */
struct var eval_assign(struct block *block, struct var target, struct var var);

/* Evaluate a().
 */
struct var eval_call(struct block *block, struct var var);

/* Evaluate (T) a.
 */
struct var eval_cast(struct block *b, struct var v, const struct typetree *t);

/* Evaluate (a) ? b : c.
 */
struct var eval_conditional(struct var a, struct block *b, struct block *c);

/* Push given parameter in preparation of a function call. Invoke in left to
 * right order, as argument appear in parameter list.
 */
void param(struct block *, struct var);

/* Evaluate return (expr)
 * If expr has a different type than return type T, a conversion equivalent to
 * assignment is made:
 *
 *      T a = expr;
 *      return a;
 */
struct var eval_return(struct block *block, const struct typetree *type);

/* Evaluate va_start builtin function.
 */
struct var eval__builtin_va_start(struct block *block, struct var arg);

/* Evaluate va_arg builtin function.
 */
struct var eval__builtin_va_arg(
    struct block *block,
    struct var arg,
    const struct typetree *type);

#endif
