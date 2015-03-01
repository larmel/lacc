#ifndef TYPE_H
#define TYPE_H

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


typetree_t *type_init(enum tree_type);

const typetree_t *type_combine(const typetree_t *, const typetree_t *);

const typetree_t *type_deref(const typetree_t *);

const typetree_t *init_type_basic(enum tree_type);

const typetree_t *type_complete(const typetree_t *, const typetree_t *);

char *typetostr(const typetree_t *);


#endif
