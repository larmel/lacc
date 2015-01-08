#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>

enum tree_type
{ 
    CHAR_T, INT64_T, DOUBLE_T, VOID_T, POINTER, FUNCTION, ARRAY 
};

enum qualifier
{
    CONST_Q = 0x1, VOLATILE_Q = 0x2, NONE_Q = 0x0
};

/* static int *foo, bar;
 * ( .name = "foo", .type )    ( .name = "bar", .type )
 *                     |                         |
 *          ( .type = POINTER, .next )          /
 *                               |             /
 *                             ( .type = INT64_T )
 */
typedef struct typetree
{
    enum tree_type type;
    int flags;

    /* Storage size in bytes */
    unsigned size;

    /* Array dimension width, how many elements of size we have */
    unsigned length;

    /* Specific for function types */
    const struct typetree **args;
    const char **params;
    unsigned n_args;

    /* Function return value, pointer target, or array base */
    const struct typetree *next;
} typetree_t;

/* Immediate value. */
typedef union value
{
    char v_char;
    long v_long;
    double v_double;
    const char *v_string;
} value_t;

/* A symbol represents declarations that may have a storage location 
 * at runtime, such as functions, static and local variables.
 * Store offset to base pointer for automatic variables and function 
 * arguments. */
typedef struct symbol
{
    const char *name;
    const typetree_t *type;
    int param_n; /* The n'th function argument. 1-indexed to keep 0 default. */
    int stack_offset; /* Argument or local variable offset to base pointer. */
    int depth;
    value_t *value; /* initialized value */
} symbol_t;

/* A reference to some storage location or direct value, used in intermediate
 * representation of expressions. There are three modes:
 *
 * DIRECT: l-value or r-value reference to symbol, which must have some
 *         storage location. For array or function types, a direct reference
 *         means the memory address of the array or function.
 * OFFSET: l-value or r-value reference to *(symbol + offset). Symbol should 
 *         have pointer or array type.
 * IMMEDIATE: 
 *         r-value immediate value, with the type specified. Symbol is NULL.
 */
typedef struct variable
{
    enum { DIRECT, OFFSET, IMMEDIATE } kind;
    const typetree_t *type;
    const symbol_t *symbol;
    int offset;
    value_t value;
} var_t;


/* Resolve symbol in current scope, or NULL if not found. Add new symbol based
 * on identifier name, or error if it is a duplicate. Create a new temporary 
 * symbol and register it to current scope.  */
const symbol_t *sym_lookup(const char *);
const symbol_t *sym_add(const char *, const typetree_t *);
const symbol_t *sym_temp(const typetree_t *);

/* Expression variables. */
var_t var_direct(const symbol_t *);
var_t var_offset(const symbol_t *, int);
var_t var_string(const char *);
var_t var_long(long);
var_t var_void();


/* functions on types */
typetree_t *type_init(enum tree_type);
const typetree_t *type_combine(const typetree_t *, const typetree_t *);
const typetree_t *type_deref(const typetree_t *);
const typetree_t *init_type_basic(enum tree_type);

unsigned type_size(const typetree_t *);

char *typetostr(const typetree_t *);

void push_scope();
void pop_scope();

void dump_symtab();

#endif
