#ifndef EVAL_H
#define EVAL_H

#include <lacc/ir.h>

/* Add asm statement to current block. */
INTERNAL void ir_asm(struct block *block, int index);

/* Convert function, array, or field to rvalue. */
INTERNAL struct var rvalue(
    struct definition *def,
    struct block *block,
    struct var var);

/*
 * Convert expression to scalar value, changing type of string constants
 * to pointer from array.
 *
 * Other non-scalar values causes a compilation error.
 */
INTERNAL struct block *scalar(
    struct definition *def,
    struct block *block,
    const char *entity);

/*
 * Commit expression in block->expr to a new temporary variable, or
 * return the value directly if there is nothing to evaluate.
 */
INTERNAL struct var eval(
    struct definition *def,
    struct block *block,
    struct expression expr);

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

INTERNAL struct expression eval_cast(
    struct definition *def,
    struct block *block,
    struct var l,
    Type type);

INTERNAL struct expression eval_va_arg(
    struct definition *def,
    struct block *block,
    struct var l,
    Type type);

INTERNAL struct expression eval_call(
    struct definition *def,
    struct block *block,
    struct var l);

INTERNAL struct expression eval_not(
    struct definition *def,
    struct block *block,
    struct var l);

INTERNAL struct expression eval_neg(
    struct definition *def,
    struct block *block,
    struct var l);

INTERNAL struct expression eval_mod(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_mul(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_div(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_add(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_sub(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_cmp_eq(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_cmp_ne(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_cmp_ge(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_cmp_gt(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_and(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_or(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_xor(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_lshift(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);

INTERNAL struct expression eval_rshift(
    struct definition *def,
    struct block *block,
    struct var l,
    struct var r);
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
INTERNAL struct expression eval_prepare_arg(
    struct definition *def,
    struct block *block,
    struct expression expr,
    Type target);

/* Prepare parameter expression. */
INTERNAL struct expression eval_prepare_vararg(
    struct definition *def,
    struct block *block,
    struct expression expr);

/*
 * Push given parameter in preparation of a function call. Invoke in
 * left to right order, as argument appear in parameter list.
 */
INTERNAL void eval_push_param(struct block *block, struct expression arg);

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
 * Return 0 or 1 if expression evaluates to an immediate non-zero value,
 * otherwise -1 if result is not known at compile time.
 *
 * Type must be scalar.
 */
INTERNAL int immediate_bool(struct expression expr);

/* Create temporary variable. */
INTERNAL struct var create_var(struct definition *def, Type type);

/* Create an immediate unsigned integer of the given type. */
INTERNAL struct var imm_unsigned(Type type, unsigned long val);

/* Immediate representing void value. */
INTERNAL struct var var_void(void);

#endif
