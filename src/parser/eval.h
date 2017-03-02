#ifndef EVAL_H
#define EVAL_H

#include <lacc/ir.h>

/*
 * Commit expression in block->expr to a new temporary variable, or
 * return the value directly if there is nothing to evaluate.
 */
struct var eval(
    struct definition *def,
    struct block *block,
    struct expression expr);

/* Evaluate l <op> r, or unary expression <op> l. */
struct expression eval_expr(
    struct definition *def,
    struct block *block,
    enum optype optype,
    struct var l, ...);

/* Evaluate &a. */
struct var eval_addr(
    struct definition *def,
    struct block *block,
    struct var var);

/* Evaluate *a. */
struct var eval_deref(
    struct definition *def,
    struct block *block,
    struct var var);

/*
 * Evaluate simple assignment (6.5.16.1).
 *
 *      a = expr
 *
 * The type of the expression is typeof(a) after l-to-r-value
 * conversion. The result is an r-value. Expression is converted to
 * typeof(a) before assignment.
 */
struct var eval_assign(
    struct definition *def,
    struct block *block,
    struct var target,
    struct expression expr);

/* Create and return a copy of variable. */
struct var eval_copy(
    struct definition *def,
    struct block *block,
    struct var var);

/*
 * Evaluate (-val). For integer operands, this is equivalent to 0 - val,
 * but floating point numbers can differentiate between positive and
 * negative zero.
 */
struct expression eval_unary_minus(
    struct definition *def,
    struct block *block,
    struct var val);

/*
 * Evaluate (+val). Perform integer promotion, and results in an
 * r-value.
 */
struct expression eval_unary_plus(struct var val);

/* Evaluate (a) ? b : c. */
struct expression eval_conditional(
    struct definition *def,
    struct expression a,
    struct block *b,
    struct block *c);

/* Prepare parameter expression. */
struct expression eval_param(
    struct definition *def,
    struct block *block,
    struct expression expr);

/*
 * Push given parameter in preparation of a function call. Invoke in
 * left to right order, as argument appear in parameter list.
 */
void param(struct block *block, struct expression arg);

/*
 * Evaluate return (expr).
 *
 * If expr has a different type than return type of the current function
 * definition, a conversion equivalent to assignment is made:
 *
 *      T a = expr;
 *      return a;
 */
struct expression eval_return(
    struct definition *def,
    struct block *block);

/*
 * Allocate stack space for variable length array, with size in bytes
 * determined by run-time evaluated expression.
 */
void eval_vla_alloc(
    struct definition *def,
    struct block *block,
    const struct symbol *sym);

/* Evaluate size of variable length array. */
struct expression eval_vla_size(
    struct definition *def,
    struct block *block,
    Type type);

/*
 * Evaluate left->expr || right->expr, where right_top is a pointer to
 * the top of the block chain ending up with right. Returns the next
 * block of execution.
 */
struct block *eval_logical_or(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right);

/* Evaluate left->expr && right->expr. */
struct block *eval_logical_and(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right);

/* Evaluate va_start builtin function. */
void eval__builtin_va_start(struct block *block, struct expression arg);

/*
 * Return 1 iff expression evaluates to an immediate non-zero value.
 * Type must be scalar.
 */
int is_immediate_true(struct expression expr);

/*
 * Return 1 iff expression evaluates to an immediate zero value.
 * Type must be scalar.
 */
int is_immediate_false(struct expression expr);

/* Create an immediate unsigned integer of the given type. */
struct var imm_unsigned(Type type, unsigned long val);

#endif
