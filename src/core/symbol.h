#ifndef SYMBOL_H
#define SYMBOL_H

#include "type.h"

#include <stdio.h>

/* A symbol represents declarations that may have a storage location at runtime,
 * such as functions, static and local variables. Store offset to base pointer
 * for automatic variables and function arguments.
 */
struct symbol
{
    const char *name;

    /* Top-level type is inlined in the symbol. Partial declarations are updated
     * by writing directly to this object. Members are still const, and should
     * never be mutated. */
    struct typetree type;

    enum symtype {
        SYM_DEFINITION = 0,
        SYM_TENTATIVE,
        SYM_DECLARATION,
        SYM_TYPEDEF,
        SYM_ENUM_VALUE
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
     * integer type. Denoted by symtype SYM_ENUM_VALUE. */
    int enum_value;

    /* Parameter or local variable offset to base pointer. This is kept as 0
     * during parsing, but assigned when passed to back-end. */
    int stack_offset;

    /* Scope depth. */
    int depth;
};

/* Get the full name, including numeric value to disambiguate.
 */
const char *sym_name(const struct symbol *sym);

struct scope;

/* A namespace holds symbols and manage resolution in scopes as they are pushed
 * or popped.
 */
struct namespace
{
    /* Friendly name of the namespace, can be identifier, label, tags or
     * anonymous. */
    const char *name;

    /* All symbols, regardless of scope, are stored in the same list. Pointers
     * must not be affected by reallocation, so store list of pointers. */
    struct symbol **symbol;
    size_t length;
    size_t capacity;

    /* Hold a list of symbols per depth, optimizing lookup. Store indices into
     * list of symbols. */
    struct scope *scope;

    /* Current depth, and number of scopes. Depth 0 is translation unit, 1 is 
     * function arguments, and n is local or member variables. */
    int current_depth;
};

struct scope
{
    /* Each scope maintains a hash table of indices into the global symbol
     * list. Index 0 is used as sentinel value meaning empty. */
    struct sym_ref {
        size_t index;
        unsigned long hash;
        struct sym_ref *next;
    } *hash_tab;

    /* The size of the hash table is determined dynamically, based on the
     * scope depth. More nested scopes are generally assumed to contain fewer
     * symbols. */
    size_t hash_length;
};

extern struct namespace
    ns_ident,   /* Identifiers. */
    ns_label,   /* Labels. */
    ns_tag;     /* Tags. */

void push_scope(struct namespace *ns);
void pop_scope(struct namespace *ns);

/* Retrieve a symbol based on identifier name, or NULL of not registered or
 * visible from current scope.
 */
struct symbol *sym_lookup(struct namespace *ns, const char *name);

/* Add symbol to current scope, or resolve to or complete existing symbols when
 * they occur repeatedly.
 */
struct symbol *sym_add(
    struct namespace *ns,
    const char *name,
    const struct typetree *type,
    enum symtype symtype,
    enum linkage linkage);

/* Create a symbol with the provided type and add it to current scope. Used to
 * hold temporary values in expression evaluation.
 */
struct symbol *sym_temp(struct namespace *ns, const struct typetree *type);

/* Register compiler internal builtin symbols, that are assumed to exists by
 * standard library headers.
 */
void register_builtin_types(struct namespace *ns);

/* Output tentative definitions, symbols that have not been assigned a value in
 * this translation unit. Output as .comm directives in GNU assembly syntax.
 */
void assemble_tentative_definitions(FILE *stream);

/* Verbose output all symbols from symbol table.
 */
void output_symbols(FILE *stream, struct namespace *ns);

#endif
