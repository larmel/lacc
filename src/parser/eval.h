#ifndef EVAL_H
#define EVAL_H

#include <lacc/ir.h>

/* Evaluate a = b <op> c, or unary expression a = <op> b
 *
 * Returns a DIRECT reference to a new temporary, or an immediate value.
 */
struct var eval_expr(
    struct definition *def,
    struct block *block,
    enum optype op, ...);

/* Evaluate &a.
 */
struct var eval_addr(
    struct definition *def,
    struct block *block,
    struct var var);

/* Evaluate *a.
 */
struct var eval_deref(
    struct definition *def,
    struct block *block,
    struct var var);

/* Evaluate simple assignment (6.5.16.1).
 *
 *      a = b
 *
 * The type of the expression is typeof(a) after l-to-r-value
 * conversion. The result is an r-value. Operand b is converted to
 * typeof(A) before assignment.
 */
struct var eval_assign(
    struct definition *def,
    struct block *block,
    struct var target,
    struct var var);

/* Evaluate a().
 */
struct var eval_call(
    struct definition *def,
    struct block *block,
    struct var var);

/* Evaluate (T) a.
 */
struct var eval_cast(
    struct definition *def,
    struct block *b,
    struct var v,
    const struct typetree *t);

/* Evaluate (a) ? b : c.
 */
struct var eval_conditional(
    struct definition *def,
    struct var a,
    struct block *b,
    struct block *c);

/* Push given parameter in preparation of a function call. Invoke in
 * left to right order, as argument appear in parameter list.
 */
void param(
    struct definition *def,
    struct block *block,
    struct var arg);

/* Evaluate return (expr).
 *
 * If expr has a different type than return type of the current function
 * definition, a conversion equivalent to assignment is made:
 *
 *      T a = expr;
 *      return a;
 */
struct var eval_return(
    struct definition *def,
    struct block *block);

/* Evaluate left->expr || right->expr, where right_top is a pointer to
 * the top of the block chain ending up with right. Returns the next
 * block of execution.
 */
struct block *eval_logical_or(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right);

/* Evaluate left->expr && right->expr.
 */
struct block *eval_logical_and(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right);

/* Evaluate va_start builtin function.
 */
struct var eval__builtin_va_start(struct block *block, struct var arg);

/* Evaluate va_arg builtin function.
 */
struct var eval__builtin_va_arg(
    struct definition *def,
    struct block *block,
    struct var arg,
    const struct typetree *type);

#endif
