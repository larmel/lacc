#ifndef SYMBOL_H
#define SYMBOL_H

#include "lcc.h"

enum tree_type { BASIC, POINTER, FUNCTION, ARRAY };
enum data_type { NONE_T, CHAR_T, INT64_T, DOUBLE_T, VOID_T };
enum qualifier { CONST_Q = 0x1, VOLATILE_Q = 0x2, NONE_Q = 0x0 };

typedef struct typetree {
    enum tree_type type;
    unsigned size; /* storage size in bytes */
    unsigned length; /* array dimension width, how many elements of size we have */
    union {
        struct {
            enum data_type type;
            enum qualifier qualifier;
        } basic;
        struct {
            struct typetree *to;
            enum qualifier qualifier;
        } ptr;
        struct {
            struct typetree **args;
            const char **params;
            unsigned n_args;
            struct typetree *ret;
        } func;
        struct {
            unsigned size;
            /* Bytes to offset on indexing. Ex. for 'int a[4][3][2]', indexing into
             * a[1] should skip 1 * (3 * 2 * sizeof(int)) bytes, reducing to a new type
             * which is array [3][2] of int. Stored in each level for convenience. */
            unsigned skip;
            struct typetree *of;
        } arr;
    } d;
} typetree_t;

enum storageclass { STORAGE_EXTERN, STORAGE_STATIC };

/* static int *foo, *bar;
 * ( .name = "foo", .type )    ( .name = "bar", .type )
 *                     |                         |
 *          ( .type = POINTER, .ptrto )
 *                               |
 *                       ( .type = T_INT, flag = 0x02 )
 */
typedef struct symbol {
    const char *name;
    int depth;
    struct typetree *type;
    enum storageclass storage;
    void *value;
} symbol_t;

/* resolve symbol in current scope, or NULL if not found */
symbol_t *sym_lookup(const char *);

/* add symbol to current scope */
symbol_t *sym_add(const char *, typetree_t *);
symbol_t *sym_mktemp(typetree_t *);
symbol_t *sym_mkimmediate(struct token);
symbol_t *sym_mktemp_immediate(enum data_type, void *);

/* functions on types */
typetree_t *type_combine(typetree_t *, typetree_t *);
typetree_t *init_type_basic(enum data_type);

void push_scope();

void pop_scope();


#endif
