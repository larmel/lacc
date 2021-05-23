#ifndef IR_H
#define IR_H
#if !defined(INTERNAL) || !defined(EXTERNAL)
# error Missing amalgamation macros
#endif

#include "array.h"
#include "symbol.h"
#include "token.h"

#include <stddef.h>

/*
 * A reference to some storage location based on a symbol and optional
 * offset, or immediate constant value. Used in intermediate
 * representation of expressions, forming operands of three-address
 * code.
 */
struct var {
    int kind : 8;

    /*
     * Width and offset in bits for field access. Normal references have
     * default values of 0.
     */
    int field_width : 8;
    int field_offset : 8;
    unsigned int lvalue : 1;

    /*
     * Set if symbol of value union is valid. Otherwise the var is an
     * immediate.
     */
    unsigned int is_symbol : 1;

    Type type;

    /*
     * Offset from symbol, which can only be positive. It is possible to
     * construct expressions that would result in negative offset, like
     * "Hello" - 3, but this is undefined behavior.
     */
    size_t offset;

    union {
        const struct symbol *symbol;
        union value imm;
    } value;
};

enum kind {
    /*
     * l-value or r-value reference to symbol, which must have some
     * storage location. Offset evaluate to *(&symbol + offset).
     * Offset in bytes, not pointer arithmetic.
     *
     * Also represent character in string literal. Direct references
     * to string literals produce DIRECT vars of array type.
     */
    DIRECT,
    /*
     * r-value address of symbol. Evaluate to (&symbol + offset),
     * always of pointer type. Offset in bytes, not pointer
     * arithmetic.
     *
     * Also represent offset string literal, like .LC1+3.
     */
    ADDRESS,
    /*
     * l-value or r-value reference to *(symbol + offset). Symbol
     * must have pointer type. If not is_symbol, the dereferenced
     * pointer is an immediate value. Offset in bytes, not pointer
     * arithmetic.
     *
     * Not valid if symbol is string literal.
     */
    DEREF,
    /*
     * r-value immediate, with the type specified.
     */
    IMMEDIATE
};

#define is_field(v) ((v).field_width != 0)

/*
 * Represent an intermediate expression with up to two operands.
 *
 * Handle va_arg(l, T) as a special type of function call, leaving it
 * up to the backend to generate code to read argument of type T.
 *
 * A transparent reference directly to a var is represented as IR_CAST,
 * where the type is the same as the var.
 */
struct expression {
    enum optype {
        IR_OP_CAST,   /* (T) l  */
        IR_OP_CALL,   /* l()    */
        IR_OP_VA_ARG, /* va_arg(l, T) */
        IR_OP_NOT,    /* ~l     */
        IR_OP_NEG,    /* -l     */
        IR_OP_ADD,    /* l + r  */
        IR_OP_SUB,    /* l - r  */
        IR_OP_MUL,    /* l * r  */
        IR_OP_DIV,    /* l / r  */
        IR_OP_MOD,    /* l % r  */
        IR_OP_AND,    /* l & r  */
        IR_OP_OR,     /* l | r  */
        IR_OP_XOR,    /* l ^ r  */
        IR_OP_SHL,    /* l << r */
        IR_OP_SHR,    /* l >> r */
        IR_OP_EQ,     /* l == r */
        IR_OP_NE,     /* l != r */
        IR_OP_GE,     /* l >= r */
        IR_OP_GT      /* l > r  */
    } op;
    Type type;
    struct var l, r;
};

#define has_side_effects(e) ((e).op == IR_OP_CALL || (e).op == IR_OP_VA_ARG)
#define is_identity(e) ((e).op == IR_OP_CAST && type_equal((e).type,(e).l.type))
#define is_immediate(e) (is_identity(e) && (e).l.kind == IMMEDIATE)
#define is_comparison(e) ((e).op >= IR_OP_EQ)

/*
 * Three-address code, specifying a target (t), left and right operand
 * (l and r, in expr), and the operation type.
 *
 * Handle va_start as a separate statement, as it requires knowledge
 * about memory layout only available to backend.
 *
 * Variable length arrays are allocated when declared, and deallocated
 * all at once when exiting function scope. Expression holds the size
 * in bytes to be allocated to VLA t.
 */
struct statement {
    enum sttype {
        IR_EXPR,      /* (expr)              */
        IR_PARAM,     /* param (expr)        */
        IR_VA_START,  /* va_start(expr)      */
        IR_ASSIGN,    /* t = expr            */
        IR_VLA_ALLOC, /* vla_alloc t, (expr) */
        IR_ASM        /* */
    } st;
    int asm_index;
    unsigned long out;
    struct var t;
    struct expression expr;
};

/*
 * Basic block in function control flow graph, containing a symbolic
 * address and a list of IR operations. Each block has a unique jump
 * target address, a symbol of type SYM_LABEL.
 */
struct block {
    const struct symbol *label;

    /* Contiguous block of three-address code operations. */
    array_of(struct statement) code;

    /*
     * Value to evaluate in branch conditions, or return value. Also
     * used for return value from expression parsing rules, as a
     * convenience. The decision on whether this block is a branch or
     * not is done purely based on the jump target list.
     */
    struct expression expr;

    /*
     * Branch targets.
     * - (NULL, NULL): Terminal node, return expr from function.
     * - (x, NULL)   : Unconditional jump; break, continue, goto, loop.
     * - (x, y)      : False and true branch targets, respectively.
     */
    struct block *jump[2];

    /*
     * Toggle last statement was return, meaning expr is valid. There
     * are cases where we reach end of control in a non-void function,
     * but not wanting to return a value. For example when exit has been
     * called.
     */
    char has_return_value;

    /*
     * Flag to mark block as having parsed and evaluated next element
     * for initialization.
     */
    char has_init_value;

    /* Used to mark nodes as visited during graph traversal. */
    enum color {
        WHITE,
        BLACK
    } color;

    /* Liveness at the start and end of the block. */
    unsigned long in;
    unsigned long out;
};

/*
 * Operand in __asm__ statement.
 *
 * Can have aliases instead of numeric reference.
 *
 *     [foo] "=r" (a)
 */
struct asm_operand {
    struct var variable;
    String alias;
    String constraint;
};

/*
 * Inline __asm__ statement, containing a template with raw assembly
 * instructions, and lists of input/output operands, clobbers, and goto
 * targets.
 *
 * Representation in IR is identical to what is written in the source
 * code. All translation of inline assembly happens in the backend,
 * where the template string is parsed and validated.
 */
struct asm_statement {
    String template;
    array_of(struct asm_operand) operands;
    array_of(String) clobbers;
    array_of(struct block *) targets;
};

/*
 * Represents a function or object definition. Parsing emits one
 * definition at a time, which is passed on to backend. A simple
 * definition can be a static or external symbol assigned to a value:
 * 
 *      int foo = 123, bar = 89;
 *
 * The whole statement is parsed in one go, but results yielded as two
 * definitions; foo, bar.
 *
 * Function definitions include a collection of blocks to model control
 * flow, and can include nested static definitions.
 *
 *      int baz(void) {
 *          static int i = 0;
 *          if (i) return 42;
 *          else return i++;
 *      }
 *
 * In the above example, baz and i are emitted as separate definitions
 * from parser.
 */
struct definition {
    /*
     * Symbol definition, which is assigned some value. A definition
     * only concerns a single symbol.
     */
    const struct symbol *symbol;

    /*
     * Function definitions are associated with a control flow graph,
     * where this is the entry point. Static and extern definitions are
     * represented as a series of assignment IR operations.
     */
    struct block *body;

    /*
     * Store all symbols associated with a function definition. Need
     * non-const references, as backend will use this to assign stack
     * offset of existing symbols.
     */
    array_of(struct symbol *)
        params,
        locals,
        labels;

    /*
     * Store all associated nodes in a list to be able to free
     * everything at the end.
     */
    array_of(struct block *) nodes;

    /* Inline assembly stored more or less as-is from parsing. */
    array_of(struct asm_statement) asm_statements;
};

/* Convert variable to no-op IR_OP_CAST expression. */
INTERNAL struct expression as_expr(struct var var);

/*
 * A direct reference to given symbol, with one exception; SYM_CONSTANT
 * reduce to an IMMEDIATE value.
 */
INTERNAL struct var var_direct(const struct symbol *sym);

/* Immediate numeric value of type int. */
INTERNAL struct var var_int(int value);

/* Immediate numeric value from typed number. */
INTERNAL struct var var_numeric(Type type, union value val);

/*
 * Create symbol representing a jump target, and associate it with the
 * given definition.
 */
INTERNAL struct symbol *create_label(struct definition *def);

#endif
