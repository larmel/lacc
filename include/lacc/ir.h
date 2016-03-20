#ifndef IR_H
#define IR_H

#include "symbol.h"
#include "token.h"

#include <stddef.h>

/* Find the number of operands to a given operation type, using the fact
 * that enumeration constants are sorted by operand count. 
 */
#define NOPERANDS(t) ((t) > IR_CAST ? 2 : (t) > IR_PARAM)
#define IS_COMPARISON(t) ((t) == IR_OP_EQ || (t) == IR_OP_GE || (t) == IR_OP_GT)

/* Three address code operation types.
 */
enum optype {
    IR_PARAM,    /* param a    */

    IR_ASSIGN,   /* a = b      */
    IR_DEREF,    /* a = *b     */
    IR_ADDR,     /* a = &b     */
    IR_NOT,      /* a = ~b     */
    IR_CALL,     /* a = b()    */
    IR_CAST,     /* a = (T) b  */

    IR_OP_ADD,   /* a = b + c  */
    IR_OP_SUB,   /* a = b - c  */
    IR_OP_MUL,   /* a = b * c  */
    IR_OP_DIV,   /* a = b / c  */
    IR_OP_MOD,   /* a = b % c  */
    IR_OP_AND,   /* a = b & c  */
    IR_OP_OR,    /* a = b | c  */
    IR_OP_XOR,   /* a = b ^ c  */
    IR_OP_SHL,   /* a = b << c */
    IR_OP_SHR,   /* a = b >> c */

    IR_OP_EQ,    /* a = b == c */
    IR_OP_GE,    /* a = b >= c */
    IR_OP_GT,    /* a = b > c  */

    /* Call va_start(a), setting reg_save_area and overflow_arg_area.
     * This, together with va_arg assumes some details about memory
     * layout that can only be known by backend, thus the need for these
     * operations. */
    IR_VA_START,

    /* Call a = va_arg(b, T), with type T taken from a. Intercepted as
     * call to __builtin_va_arg in parser. */
    IR_VA_ARG
};

/* A reference to some storage location or direct value, used in
 * intermediate representation of expressions.
 */
struct var {
    const struct typetree *type;
    const struct symbol *symbol;

    enum {
        /* l-value or r-value reference to symbol, which must have some
         * storage location. Offset evaluate to *(&symbol + offset).
         * Offset in bytes, not pointer arithmetic. */
        DIRECT,
        /* l-value or r-value reference to *(symbol + offset). Symbol
         * must have pointer type. Offset in bytes, not pointer
         * arithmetic. */
        DEREF,
        /* r-value immediate, with the type specified. Symbol is NULL,
         * or be of type SYM_STRING_VALUE. String immediates can either
         * have type array of char, or pointer to char. They can also
         * have offsets, representing constants such as .LC1+3. */
        IMMEDIATE
    } kind;

    union value imm;

    int offset;
    int lvalue;
};

/* Basic block in function control flow graph, containing a symbolic
 * address and a contiguous list of IR operations.
 */
struct block {
    /* Unique jump target address, symbol of type SYM_LABEL. */
    const struct symbol *label;

    /* Realloc-able list of 3-address code operations. */
    struct op {
        enum optype type;
        struct var a;
        struct var b;
        struct var c;
    } *code;

    /* Number of ir operations. */
    int n;

    /* Toggle last statement was return, meaning expr is valid. There
     * are cases where we reach end of control in a non-void function,
     * but not wanting to return a value. For example when exit has been
     * called. */
    int has_return_value;

    /* Value to evaluate in branch conditions, or return value. Also
     * used for return value from expression parsing rules, as a
     * convenience. The decision on whether this block is a branch or
     * not is done purely based on the jump target list. */
    struct var expr;

    /* Branch targets.
     * - (NULL, NULL): Terminal node, return expr from function.
     * - (x, NULL)   : Unconditional jump; break, continue, goto, loop.
     * - (x, y)      : False and true branch targets, respectively. */
    struct block *jump[2];

    /* Used to mark nodes as visited during graph traversal. */
    enum color {
        WHITE,
        BLACK
    } color;
};

struct block_list {
    struct block **block;
    int length;
    int capacity;
};

/* Represents a function or object definition. Parsing emits one
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
    /* Symbol definition, which is assigned some value. A definition
     * only concerns a single symbol. */
    const struct symbol *symbol;

    /* Function definitions are associated with a control flow graph,
     * where this is the entry point. Static and extern definitions are
     * represented as a series of assignment IR operations. */
    struct block *body;

    /* Store all symbols associated with a function definition. Need
     * non-const references, as backend will use this to assign stack
     * offset of existing symbols. */
    struct symbol_list
        params,
        locals;

    /* Store all associated nodes in a list to be able to free
     * everything at the end. */
    struct block_list nodes;
};

/* Parse input for the next function or object definition. Symbol is
 * NULL on end of input. Takes ownership of memory, which must be
 * cleaned up by calling free_definition.
 */
struct definition parse(void);

/* A direct reference to given symbol, with two exceptions:
 * SYM_ENUM_VALUE and SYM_STRING_VALUE reduce to IMMEDIATE values.
 */
struct var var_direct(const struct symbol *sym);

/* Immediate numeric value of type int.
 */
struct var var_int(int value);

/* Immediate numeric value from integer.
 */
struct var var_numeric(struct integer n);

#endif
