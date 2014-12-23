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

/* static int *foo, *bar;
 * ( .name = "foo", .type )    ( .name = "bar", .type )
 *                     |                         |
 *          ( .type = POINTER, .next )          /
 *                               |             /
 *                             ( .type = INT64_T )
 */
typedef struct symbol
{
    const char *name;
    const struct typetree *type;

    /* Offset to base pointer. */
    int stack_offset;

    int depth; /* todo: this should not be explicitly stored here */
    enum storageclass storage;
    int is_immediate;
    union {
        char charval;
        int intval;
        long longval;
        double doubleval;
        float floatval;
    } immediate;
} symbol_t;


/* resolve symbol in current scope, or NULL if not found */
const symbol_t *sym_lookup(const char *);

/* add symbol to current scope */
const symbol_t *sym_add(const char *, const typetree_t *);
const symbol_t *sym_mktemp(const typetree_t *);
const symbol_t *sym_mkimmediate(enum tree_type, const char *value);
const symbol_t *sym_mkimmediate_long(long);

/* functions on types */
typetree_t *type_init(enum tree_type);
const typetree_t *type_combine(const typetree_t *, const typetree_t *);
const typetree_t *type_deref(const typetree_t *);
const typetree_t *init_type_basic(enum tree_type);
size_t type_varsize(const typetree_t *);

void push_scope();
void pop_scope();

void dump_symtab();

#endif
