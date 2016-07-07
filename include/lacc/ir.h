#ifndef IR_H
#define IR_H

#include "array.h"
#include "symbol.h"
#include "token.h"

#include <stddef.h>

/* Three address code operation types.
 */
enum optype {
    IR_PARAM = 0x10,    /* param a    */

    IR_ASSIGN = 0x20,   /* a = b      */
    IR_ADDR = 0x21,     /* a = &b     */
    IR_NOT = 0x22,      /* a = ~b     */
    IR_CALL = 0x23,     /* a = b()    */
    IR_CAST = 0x24,     /* a = (T) b  */

    IR_OP_ADD = 0x30,   /* a = b + c  */
    IR_OP_SUB = 0x31,   /* a = b - c  */
    IR_OP_MUL = 0x32,   /* a = b * c  */
    IR_OP_DIV = 0x33,   /* a = b / c  */
    IR_OP_MOD = 0x34,   /* a = b % c  */
    IR_OP_AND = 0x35,   /* a = b & c  */
    IR_OP_OR = 0x36,    /* a = b | c  */
    IR_OP_XOR = 0x37,   /* a = b ^ c  */
    IR_OP_SHL = 0x38,   /* a = b << c */
    IR_OP_SHR = 0x39,   /* a = b >> c */

    IR_OP_EQ = 0x3A,    /* a = b == c */
    IR_OP_GE = 0x3B,    /* a = b >= c */
    IR_OP_GT = 0x3C,    /* a = b > c  */

    /* Call va_start(a), setting reg_save_area and overflow_arg_area.
     * This, together with va_arg assumes some details about memory
     * layout that can only be known by backend, thus the need for these
     * operations. */
    IR_VA_START = 0x11,

    /* Call a = va_arg(b, T), with type T taken from a. Intercepted as
     * call to __builtin_va_arg in parser. */
    IR_VA_ARG = 0x26
};

#define OPERAND_COUNT(optype) ((optype) >> 4)
#define IS_COMPARISON(optype) (((optype) & 0x0F) > 9)

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

    /* Width in bits of bitfield access. Direct or deref references to
     * fields in a struct are restricted to a number of bits, which is
     * used for masking evaluation of assignment. Normal references have
     * default value of 0. */
    int width;

    union value imm;

    int offset;
    int lvalue;
};

/* Three-address code, specifying a target (a), left and right operand
 * (b and c, respectively), and the operation type.
 */
struct op {
    enum optype type;
    struct var a;
    struct var b;
    struct var c;
};

/* Basic block in function control flow graph, containing a symbolic
 * address and a list of IR operations. Each block has a unique jump
 * target address, a symbol of type SYM_LABEL.
 */
struct block {
    const struct symbol *label;

    /* Contiguous block of three-address code operations. */
    array_of(struct op) code;

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

    /* Toggle last statement was return, meaning expr is valid. There
     * are cases where we reach end of control in a non-void function,
     * but not wanting to return a value. For example when exit has been
     * called. */
    int has_return_value;

    /* Used to mark nodes as visited during graph traversal. */
    enum color {
        WHITE,
        BLACK
    } color;
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
    array_of(struct symbol *)
        params,
        locals;

    /* Store all associated nodes in a list to be able to free
     * everything at the end. */
    array_of(struct block *) nodes;
};

/* A direct reference to given symbol, with two exceptions:
 * SYM_CONSTANT and SYM_STRING_VALUE reduce to IMMEDIATE values.
 */
struct var var_direct(const struct symbol *sym);

/* Immediate numeric value of type int.
 */
struct var var_int(int value);

/* Immediate numeric value from typed number.
 */
struct var var_numeric(struct number n);

#endif
