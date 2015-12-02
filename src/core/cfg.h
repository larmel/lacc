#ifndef CFG_H
#define CFG_H

#include "symbol.h"

#include <stddef.h>

/* A reference to some storage location or direct value, used in intermediate
 * representation of expressions.
 */
struct var
{
    const struct typetree *type;
    const struct symbol *symbol;

    enum {
        /* l-value or r-value reference to symbol, which must have some storage
         * location. Offset evaluate to *(&symbol + offset). Offset in bytes,
         * not pointer arithmetic. */
        DIRECT,
        /* l-value or r-value reference to *(symbol + offset). Symbol must have
         * pointer type. Offset in bytes, not pointer arithmetic. */
        DEREF,
        /* r-value immediate, with the type specified. Symbol is NULL. */
        IMMEDIATE
    } kind;

    union value {
        long i;
        unsigned long u;
    } imm;

    /* Represent string constant value, or label, for IMMEDIATE values. If type
     * is char [], this is the literal string constant. If type is char *, this
     * is the label representing the string, as in '.LC1'. Pointers can have a
     * constant offset, representing address constants such as .LC1+3. */
    const char *string;

    int offset;
    int lvalue;
};

/* A direct reference to given symbol.
 */
struct var var_direct(const struct symbol *sym);

/* A string value of type [] char.
 */
struct var var_string(const char *str);

/* A constant value of integer type.
 */
struct var var_int(int value);

/* A zero constant value of integer type.
 */
struct var var_zero(int size);

/* A value with no type.
 */
struct var var_void(void);

/* Create a variable of the given type, returning a direct reference to a new
 * symbol.
 */
struct var create_var(const struct typetree *type);

/* Three address code operation types.
 */
enum optype
{
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

    /* Call va_start(a), setting reg_save_area and overflow_arg_area. This,
     * together with va_arg assumes some details about memory layout that can
     * only be known by backend, thus the need for these operations. */
    IR_VA_START,

    /* Call a = va_arg(b, T), with type T taken from a. Intercepted as call to
     * __builtin_va_arg in parser. */
    IR_VA_ARG
};

/* Find the number of operands to a given operation type, using the fact that
 * enumeration constants are sorted by operand count. 
 */
#define NOPERANDS(t) ((t) > IR_CAST ? 2 : (t) > IR_PARAM)

#define IS_COMPARISON(t) ((t) == IR_OP_EQ || (t) == IR_OP_GE || (t) == IR_OP_GT)

/* Basic block in control flow graph.
 */
struct block
{
    /* A unique jump target label. */
    const char *label;

    /* Realloc-able list of 3-address code operations. */
    struct op {
        enum optype type;
        struct var a;
        struct var b;
        struct var c;
    } *code;

    /* Number of ir operations. */
    int n;

    /* Toggle last statement was return, meaning expr is valid. There are cases
     * where we reach end of control in a non-void function, but not wanting to
     * return a value. For example when exit has been called. */
    int has_return_value;

    /* Value to evaluate in branch conditions, or return value. Also used for
     * return value from expression parsing rules, as a convenience. The
     * decision on whether this block is a branch or not is done purely based
     * on the jump target list. */
    struct var expr;

    /* Branch targets.
     * - (NULL, NULL): Terminal node, return expr from function.
     * - (x, NULL)   : Unconditional jump, f.ex break, goto, or bottom of loop.
     * - (x, y)      : False and true branch targets, respectively. */
    struct block *jump[2];

    /* Used to mark nodes as visited during graph traversal. */
    enum color {
        WHITE,
        BLACK
    } color;
};

/* Represents an external declaration list or a function definition.
 */
struct cfg
{
    /* Function symbol or NULL if list of declarations. */
    const struct symbol *fun;

    /* References to blocks holding global declarations and head of function
     * CFG, respectively. */
    struct block *head, *body;

    /* Number of bytes to allocate to local variables on stack. */
    int locals_size;

    /* Store all symbols associated with a function declaration. Need non-const
     * references, as backend will use this to assign stack offset of existing
     * symbols. */
    struct symbol_list
        params,
        locals;

    /* Store all associated nodes in a list to simplify deallocation. */
    struct block **nodes;
    size_t size;
    size_t capacity;
};

/* Current declaration, accessed for creating new blocks or adding init code
 * in head block.
 */
extern struct cfg current_cfg;

/* Initialize a new control flow graph structure, updating current_cfg.
 */
void cfg_init_current(void);

/* Initialize a CFG block with a unique jump label. Borrows memory.
 */
struct block *cfg_block_init(void);

/* Add a 3-address code operation to the block. Code is kept in a separate list
 * for each block.
 */
void cfg_ir_append(struct block *block, struct op op);

/* Add local variable to symbol list, required for assembly.
 */
void cfg_register_local(struct symbol *symbol);

/* Add function parameter to symbol list, required for assembly.
 */
void cfg_register_param(struct symbol *symbol);

#endif
