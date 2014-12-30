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

enum storageclass
{
    STORAGE_EXTERN, STORAGE_STATIC
};

typedef union value {
    char vchar;
    char *string;
    long vlong;
    double vdouble;
} value_t;

/* A symbol always has a type. In addition to that, it can either be a plain
 * value represented as a value_t, or a variable with a name and some storage 
 * assigned at runtime. */
typedef struct symbol
{
    /* If name is NULL, then this is an immediate value. */
    const char *name;

    /* Both values and variables have a type. */
    const struct typetree *type;

    enum storageclass storage;

    /* Union of immediate values, compile time constants or evaluated. NULL if
     * no value at compile time. */
    value_t *value;

    /* Offset to base pointer. */
    int stack_offset;

    /* Scope depth. */
    int depth;
} symbol_t;


/* Resolve symbol in current scope, or NULL if not found. Add new symbol based
 * on identifier name, or error if it is a duplicate. Create a new temporary 
 * symbol and register it to current scope.  */
const symbol_t *sym_lookup(const char *);
const symbol_t *sym_add(const char *, const typetree_t *);
const symbol_t *sym_temp(const typetree_t *);


/* Create an anonymous symbol based on an immediate value, without assigning a
 * name or making it visible in scope. */
const symbol_t *sym_string_init(const char *);
const symbol_t *sym_number_init(long);


/* functions on types */
typetree_t *type_init(enum tree_type);
const typetree_t *type_combine(const typetree_t *, const typetree_t *);
const typetree_t *type_deref(const typetree_t *);
const typetree_t *init_type_basic(enum tree_type);
size_t type_varsize(const typetree_t *);

char *typetostr(const typetree_t *);

void push_scope();
void pop_scope();

void dump_symtab();

#endif
