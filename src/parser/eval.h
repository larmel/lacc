#ifndef EVAL_H
#define EVAL_H

#include <lacc/ir.h>

/* Add statement to current block. */
INTERNAL void emit_ir(struct block *block, enum sttype st, ...);

/*
 * Commit expression in block->expr to a new temporary variable, or
 * return the value directly if there is nothing to evaluate.
 */
INTERNAL struct var eval(
    struct definition *def,
    struct block *block,
    struct expression expr);

/* Evaluate l <op> r, or unary expression <op> l. */
INTERNAL struct expression eval_expr(
    struct definition *def,
    struct block *block,
    enum optype optype,
    struct var l, ...);

/* Evaluate &a. */
INTERNAL struct var eval_addr(
    struct definition *def,
    struct block *block,
    struct var var);

/* Evaluate *a. */
INTERNAL struct var eval_deref(
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
INTERNAL struct var eval_assign(
    struct definition *def,
    struct block *block,
    struct var target,
    struct expression expr);

/* Create and return a copy of variable. */
INTERNAL struct var eval_copy(
    struct definition *def,
    struct block *block,
    struct var var);

/*
 * Evaluate (+val). Perform integer promotion, and results in an
 * r-value.
 */
INTERNAL struct expression eval_unary_plus(struct var val);

/* Evaluate operands of (a) ? b : c, and return result type. */
INTERNAL Type eval_conditional(
    struct definition *def,
    struct block *left,
    struct block *right);

/* Prepare parameter expression. */
INTERNAL struct expression eval_param(
    struct definition *def,
    struct block *block,
    struct expression expr);

/*
 * Push given parameter in preparation of a function call. Invoke in
 * left to right order, as argument appear in parameter list.
 */
INTERNAL void param(struct block *block, struct expression arg);

/*
 * Evaluate return (expr).
 *
 * If expr has a different type than return type of the current function
 * definition, a conversion equivalent to assignment is made:
 *
 *      T a = expr;
 *      return a;
 */
INTERNAL struct expression eval_return(
    struct definition *def,
    struct block *block);

/*
 * Evaluate (expr) as a separate statement.
 *
 * No-op of expression cannot have side effects.
 */
INTERNAL struct expression eval_expression_statement(
    struct definition *def,
    struct block *block,
    struct expression expr);

/*
 * Allocate stack space for variable length array, with size in bytes
 * determined by run-time evaluated expression.
 */
INTERNAL void eval_vla_alloc(
    struct definition *def,
    struct block *block,
    const struct symbol *sym);

/* Evaluate size of variable length array. */
INTERNAL struct expression eval_vla_size(
    struct definition *def,
    struct block *block,
    Type type);

/*
 * Evaluate left->expr || right->expr, where right_top is a pointer to
 * the top of the block chain ending up with right. Returns the next
 * block of execution.
 */
INTERNAL struct block *eval_logical_or(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right);

/* Evaluate left->expr && right->expr. */
INTERNAL struct block *eval_logical_and(
    struct definition *def,
    struct block *left,
    struct block *right_top,
    struct block *right);

/* Evaluate va_start builtin function. */
INTERNAL void eval__builtin_va_start(
    struct block *block,
    struct expression arg);

/*
 * Return 1 iff expression evaluates to an immediate non-zero value.
 * Type must be scalar.
 */
INTERNAL int is_immediate_true(struct expression expr);

/*
 * Return 1 iff expression evaluates to an immediate zero value.
 * Type must be scalar.
 */
INTERNAL int is_immediate_false(struct expression expr);

/* Create temporary variable. */
INTERNAL struct var create_var(struct definition *def, Type type);

/* Create an immediate unsigned integer of the given type. */
INTERNAL struct var imm_unsigned(Type type, unsigned long val);

/* Immediate representing void value. */
INTERNAL struct var var_void(void);

#endif
