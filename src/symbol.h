#ifndef SYMBOL_H
#define SYMBOL_H

#include "type.h"


/* Storage of a symbol, as specified with storage-class-specifier.
 */
enum storage_class {
    STC_NONE,
    STC_AUTO,
    STC_EXTERN,
    STC_STATIC,
    STC_TYPEDEF
};

/* A symbol represents declarations that may have a storage location at runtime,
 * such as functions, static and local variables. Store offset to base pointer
 * for automatic variables and function arguments.
 */
typedef struct symbol
{
    const char *name;
    const typetree_t *type;

    /* The n'th function argument. 1-indexed to keep 0 default. */
    int param_n;

    /* Argument or local variable offset to base pointer. */
    int stack_offset;

    int depth;
    enum storage_class storage;
} symbol_t;

/* Resolve symbol in current scope, or NULL if not found. Add new symbol based
 * on identifier name, or error if it is a duplicate. Create a new temporary 
 * symbol and register it to current scope.
 */
symbol_t *sym_lookup(const char *);
symbol_t *sym_add(const char *, const typetree_t *, enum storage_class);
const symbol_t *sym_temp(const typetree_t *);
const symbol_t *sym_temp_static(const typetree_t *);


void push_scope();
void pop_scope();

void dump_symtab();


#endif
