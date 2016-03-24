#ifndef SYMTAB_H
#define SYMTAB_H

#include <lacc/hash.h>
#include <lacc/list.h>
#include <lacc/symbol.h>

/* A namespace holds symbols and manage resolution in scopes as they are
 * pushed or popped.
 */
struct namespace {
    /* Friendly name of the namespace, can be identifier, label, tags or
     * anonymous. */
    const char *name;

    /* All symbols, regardless of scope, are stored in the same list.
     * Must not be affected by reallocation, so store pointers. */
    struct list symbol_list;

    /* Hold a list of 'struct hash_table *', per scope depth, optimizing
     * lookup. The hash table stores pointers to symbols in the symbol
     * list. */
    struct list scope_list;

    /* Iterator for successive calls to yield. */
    int cursor;
};

extern struct namespace
    ns_ident,   /* Identifiers. */
    ns_label,   /* Labels. */
    ns_tag;     /* Tags. */

void push_scope(struct namespace *ns);
void pop_scope(struct namespace *ns);

/* Current depth as translation pushes and pops scopes. Depth 0 is
 * translation unit, 1 is function arguments, and n is local or member
 * variables.
 */
unsigned current_scope_depth(struct namespace *ns);

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

/* Retrieve next tentative definition or declaration from given scope.
 */
const struct symbol *yield_declaration(struct namespace *ns);

/* Verbose output all symbols from symbol table.
 */
void output_symbols(FILE *stream, struct namespace *ns);

#endif
