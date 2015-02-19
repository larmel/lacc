#ifndef SYMBOL_H
#define SYMBOL_H

#include <stddef.h>
#include <stdio.h>

enum tree_type
{
    INTEGER,    /* char, short, int, long */ 
    REAL,       /* float, double */
    POINTER,
    FUNCTION,
    ARRAY,
    OBJECT,     /* struct, union */
    NONE        /* void */
};

typedef struct flags
{
    unsigned fconst : 1;
    unsigned fvolatile : 1;
    unsigned funsigned: 1;
} flags_t;

/* Recursive structure representing a type.
 */
typedef struct typetree
{
    enum tree_type type;
    flags_t flags;

    /* Total storage size in bytes, returned for sizeof( ). */
    unsigned size;

    /* Function parameters or struct/union members. */
    const struct typetree **args;
    const char **params;
    unsigned n_args;

    /* Function return value, pointer target, or array base. */
    const struct typetree *next;
} typetree_t;

/* Storage of a symbol, as specified with storage-class-specifier.
 */
enum storage_class {
    STC_NONE,
    STC_AUTO,
    STC_EXTERN,
    STC_STATIC,
    STC_TYPEDEF
};

/* A symbol represents declarations that may have a storage location at runtime,
 * such as functions, static and local variables. Store offset to base pointer
 * for automatic variables and function arguments.
 */
typedef struct symbol
{
    const char *name;
    const typetree_t *type;

    /* The n'th function argument. 1-indexed to keep 0 default. */
    int param_n;

    /* Argument or local variable offset to base pointer. */
    int stack_offset;

    int depth;
    enum storage_class storage;
} symbol_t;

/* Immediate value.
 */
typedef union value
{
    long integer;
    double real;
    const char *string;
} value_t;

/* A reference to some storage location or direct value, used in intermediate
 * representation of expressions. There are three modes:
 *
 * DIRECT: l-value or r-value reference to symbol, which must have some
 *         storage location. For array or function types, a direct reference
 *         means the memory address of the array or function.
 * OFFSET: l-value or r-value reference to *(symbol + offset). Symbol should 
 *         have pointer, array or object type.
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
    int lvalue;
} var_t;


/* Resolve symbol in current scope, or NULL if not found. Add new symbol based
 * on identifier name, or error if it is a duplicate. Create a new temporary 
 * symbol and register it to current scope.
 */
const symbol_t *sym_lookup(const char *);
const symbol_t *sym_add(const char *, const typetree_t *, enum storage_class);
const symbol_t *sym_temp(const typetree_t *);
const symbol_t *sym_temp_static(const typetree_t *);

/* Expression variables.
 */
var_t var_direct(const symbol_t *);
var_t var_offset(const symbol_t *, int);
var_t var_string(const char *, size_t);
var_t var_long(long);
var_t var_void();

/* Functions on types.
 */
typetree_t *type_init(enum tree_type);
const typetree_t *type_combine(const typetree_t *, const typetree_t *);
const typetree_t *type_deref(const typetree_t *);
const typetree_t *init_type_basic(enum tree_type);
void type_complete(typetree_t *, const typetree_t *);

char *typetostr(const typetree_t *);

void push_scope();
void pop_scope();

void dump_symtab();


const char *string_constant_label(const char *);
void output_string(FILE *, const char *label);
void output_strings(FILE *);


#endif
