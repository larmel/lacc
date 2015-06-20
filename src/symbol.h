#ifndef SYMBOL_H
#define SYMBOL_H

#include "type.h"
#include "util.h"

#include <stdio.h>

/* A symbol represents declarations that may have a storage location at runtime,
 * such as functions, static and local variables. Store offset to base pointer
 * for automatic variables and function arguments.
 */
struct symbol
{
    const char *name;
    const struct typetree *type;

    enum symtype {
        SYM_DEFINITION = 0,
        SYM_TENTATIVE,
        SYM_DECLARATION,
        SYM_TYPEDEF,
        SYM_ENUM
    } symtype;

    /* Visibility of external declarations, or LINK_NONE for other symbols. */
    enum linkage {
        LINK_NONE = 0,
        LINK_INTERN,
        LINK_EXTERN
    } linkage;

    /* Tag to disambiguate differently scoped static variables with the same
     * name. */
    int n;

    /* Enumeration constants live in the normal symbol table, and always have
     * integer type. Denoted by symtype SYM_ENUM. */
    int enum_value;

    /* Parameter or local variable offset to base pointer. This is kept as 0
     * during parsing, but assigned when passed to back-end. */
    int stack_offset;

    /* Scope depth. */
    int depth;
};

/* A namespace holds symbols and manage resolution in scopes as they are pushed
 * or popped.
 */
struct namespace
{
    const char *name;
    struct symbol **symbol;
    int size, cap;

    /* Hold a list of symbols per depth, optimizing lookup. Store indices into
     * list of symbols. */
    struct scope {
        int *idx, size, cap;
    } *scope;

    /* Current depth, and number of scopes. Depth 0 is translation unit, 1 is 
     * function arguments, and n is local or member variables. */
    int current_depth;
};

extern struct namespace
    ns_ident,   /* Identifiers. */
    ns_label,   /* Labels. */
    ns_tag;     /* Tags. */

void push_scope(struct namespace *);
void pop_scope(struct namespace *);

struct symbol *sym_lookup(struct namespace *, const char *);
struct symbol *sym_add(struct namespace *, struct symbol);
struct symbol *sym_temp(struct namespace *, const struct typetree *);

void register_builtin_types(struct namespace *);
void output_definitions(FILE *);
void output_symbols(FILE *, struct namespace *);

#endif
