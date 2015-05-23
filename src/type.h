#ifndef TYPE_H
#define TYPE_H

#include <stdlib.h>

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

struct member;

/* Internal representation of a type.
 */
typedef struct typetree
{
    enum tree_type type;
    flags_t flags;

    /* Total storage size in bytes, returned for sizeof( ). */
    unsigned size;

    /* Function parameters or struct/union members. */
    /* todo: add offset array, and make 
     * type_add_subtree(struct typetree *t, struct typetree *c);
     */
    struct member *member;

    /* Number of function parameters or object members. */
    int n;

    /*const struct typetree **args;
    const char **params;
    unsigned n_args; */

    /* Function accepts variable argument list (...) */
    int vararg;

    /* Function return value, pointer target, or array base. */
    const struct typetree *next;
} typetree_t;

struct member
{
    const struct typetree *type;
    const char *name;

    /* Byte offset into struct. */
    int offset;
};

void type_add_member(struct typetree *, const struct typetree *, const char *);

void type_align_struct_members(struct typetree *);

typetree_t *type_init(enum tree_type);

const typetree_t *type_init_string(size_t);

int type_equal(const typetree_t *, const typetree_t *);

const typetree_t *type_combine(const typetree_t *, const typetree_t *);

const typetree_t *type_deref(const typetree_t *);

const typetree_t *init_type_basic(enum tree_type);

const typetree_t *type_complete(const typetree_t *, const typetree_t *);

char *typetostr(const typetree_t *);


#endif
