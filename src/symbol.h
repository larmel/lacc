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

    /* Visibility of external declarations, or NONE for other symbols. */
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

struct scope;

/* Hold symbols and manage scopes in a namespace. There are three different
 * types of namespaces in C (A11.1):
 *  1) Objects, functions, typedef names and enum constants.
 *  2) Labels.
 *  3) Structure, union and enum tags.
 *  4) Structure and union members, for each instance.
 *
 * Depth 0 is translation unit, depth 1 is function arguments, depth n is local
 * or member variables. Keep track of size of locals and number of parameters,
 * as symbols are added to different scope depths.
 */
struct namespace
{
    const char *name;
    struct symbol **symbol;
    int capacity, size;

    struct scope *scope;
    int depth;
};

extern struct namespace
    ns_ident,   /* Identifiers */
    ns_label,   /* Labels */
    ns_tag;     /* Tags */

void push_scope(struct namespace *);
void pop_scope(struct namespace *);
void dump_symtab(struct namespace *);

/* Resolve symbol in current scope, or NULL if not found. Add new symbol based
 * on identifier name, or error if it is a duplicate. Create a new temporary 
 * symbol and register it to current scope.
 */
struct symbol *sym_lookup(struct namespace *, const char *);
struct symbol *sym_add(struct namespace *, struct symbol);

struct symbol *sym_temp(struct namespace *, const struct typetree *);
const struct symbol *
sym_temp_static(struct namespace *, const struct typetree *);

void register_builtin_types(struct namespace *);
void output_definitions(FILE *);

#endif
