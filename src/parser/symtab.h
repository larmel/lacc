#ifndef SYMTAB_H
#define SYMTAB_H

#include <lacc/symbol.h>
#include <lacc/hash.h>

/* A namespace holds symbols and manage resolution in scopes as they are
 * pushed or popped.
 */
struct namespace {
    /* Friendly name of the namespace, can be identifier, label, tags or
     * anonymous. */
    const char *name;

    /* All symbols, regardless of scope, are stored in the same list.
     * Must not be affected by reallocation, so store pointers. */
    struct symbol **symbol;
    size_t length;
    size_t capacity;

    /* Hold a list of symbols per depth, optimizing lookup. Store
     * pointers to symbols. */
    struct hash_table *scope;

    /* Current depth, and number of scopes. Depth 0 is translation unit,
     * 1 is function arguments, and n is local or member variables. */
    int current_depth;
};

extern struct namespace
    ns_ident,   /* Identifiers. */
    ns_label,   /* Labels. */
    ns_tag;     /* Tags. */

void push_scope(struct namespace *ns);
void pop_scope(struct namespace *ns);

/* Retrieve a symbol based on identifier name, or NULL of not registered
 * or visible from current scope.
 */
struct symbol *sym_lookup(struct namespace *ns, const char *name);

/* Add symbol to current scope, or resolve to or complete existing
 * symbols when they occur repeatedly.
 */
struct symbol *sym_add(
    struct namespace *ns,
    const char *name,
    const struct typetree *type,
    enum symtype symtype,
    enum linkage linkage);

/* Create a symbol with the provided type and add it to current scope in
 * identifier namespace. Used to hold temporary values in expression
 * evaluation.
 */
struct symbol *sym_create_tmp(const struct typetree *type);

/* Register compiler internal builtin symbols, that are assumed to
 * exists by standard library headers.
 */
void register_builtin_types(struct namespace *ns);

/* Retrieve all tentative definitions from given scope. Caller takes
 * ownership of memory allocated to hold the list.
 */
struct symbol_list get_tentative_definitions(const struct namespace *ns);

/* Verbose output all symbols from symbol table.
 */
void output_symbols(FILE *stream, struct namespace *ns);

#endif
