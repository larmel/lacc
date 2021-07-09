#ifndef SYMTAB_H
#define SYMTAB_H

#include <lacc/array.h>
#include <lacc/hash.h>
#include <lacc/symbol.h>

typedef array_of(struct symbol *) SymbolArray;

/*
 * A namespace holds symbols and manage resolution in scopes as they are
 * pushed or popped.
 */
struct namespace {
    /*
     * All symbols, regardless of scope, are stored in the same list.
     * Must not be affected by reallocation, so store pointers.
     */
    SymbolArray symbols;

    /* Global symbols are stored in a hash table for fast lookup. */
    struct hash_table globals;

    /*
     * Scoped symbols are keps in a simple array, using just linear
     * search for lookup and relying on there being relatively few names
     * to check in the typical case. In practice this is much faster
     * than having a hash table per scope.
     */
    struct {
        /*
         * List containing number of symbols in each scope, last element
         * corresponding to current scope.
         */
        array_of(int) counts;

        /*
         * All symbols contained in some scope, expanding on entering a
         * new block, and shrinking when leaving. Resolving symbols will
         * traverse this list backwards.
         */
        array_of(String) names;
        SymbolArray symbols;
    } scope;

    /* Iterator for successive calls to yield. */
    int cursor;
};

/*
 * There are tree different namespaces; in addition to normal identifier
 * names, there also labels (for goto) and struct/union tags. These
 * categories do not collide with each other.
 */
EXTERNAL struct namespace
    ns_ident,
    ns_label,
    ns_tag;

/* Push scope to namespace. */
INTERNAL void push_scope(struct namespace *ns);

/* Internal structures are reset on popping the last scope. */
INTERNAL void pop_scope(struct namespace *ns);

/*
 * Current depth as translation pushes and pops scopes. Depth 0 is
 * translation unit, 1 is function arguments, and n is local or member
 * variables.
 */
INTERNAL int current_scope_depth(struct namespace *ns);

/*
 * Retrieve a symbol based on identifier name, or NULL of not registered
 * or visible from current scope.
 */
INTERNAL struct symbol *sym_lookup(struct namespace *ns, String name);

/*
 * Add symbol to current scope, or resolve to or complete existing
 * symbols when they occur repeatedly.
 */
INTERNAL struct symbol *sym_add(
    struct namespace *ns,
    String name,
    Type type,
    enum symtype symtype,
    enum linkage linkage);

/* Add symbol to current scope of given namespace. */
INTERNAL void sym_make_visible(struct namespace *ns, struct symbol *sym);

/*
 * Create a symbol with the provided type and add it to current scope in
 * identifier namespace. Used to hold temporary values in expression
 * evaluation.
 */
INTERNAL struct symbol *sym_create_temporary(Type type);

/* Create an unnamed variable, produced by a compound literal. */
INTERNAL struct symbol *sym_create_unnamed(Type type);

/* Create a label. */
INTERNAL struct symbol *sym_create_label(void);

/* Create a symbol representing a string constant. */
INTERNAL struct symbol *sym_create_string(String str);

/*
 * Create symbol for builtins, which are associated with special
 * functions for translation once encountered in the source.
 *
 * Builtin symbols have symtype SYM_BUILTIN.
 */
INTERNAL struct symbol *sym_create_builtin(
    String name,
    struct block *(*handler)(struct definition *, struct block *));

/*
 * Release memory used for a temporary symbol, allowing it to be reused
 * in a different function.
 */
INTERNAL void sym_discard(struct symbol *sym);

/*
 * Retrieve next tentative definition or declaration from given scope.
 */
INTERNAL const struct symbol *yield_declaration(struct namespace *ns);

/* Verbose output all symbols from symbol table. */
INTERNAL void output_symbols(FILE *stream, struct namespace *ns);

/* Called after each translation unit to clear buffers. */
INTERNAL void symtab_clear(void);

/* Free memory after all input files are processed. */
INTERNAL void symtab_finalize(void);

#endif
