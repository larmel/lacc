#ifndef SYMBOL_H
#define SYMBOL_H

#include "type.h"


typedef enum symtype {
    SYM_DEFINITION = 0,
    SYM_TENTATIVE,
    SYM_TYPEDEF,
    SYM_ENUM
} symtype_t;

/* Linkage is internal iff storage class is static, otherwise linkage is 
 * external. */
typedef enum linkage {
    LINK_NONE = 0,
    LINK_INTERN,
    LINK_EXTERN
} linkage_t;

/* A symbol represents declarations that may have a storage location at runtime,
 * such as functions, static and local variables. Store offset to base pointer
 * for automatic variables and function arguments. */
typedef struct symbol {
    symtype_t symtype;
    linkage_t linkage;

    const char *name;
    const typetree_t *type;

    /* Enumeration constants live in the normal symbol table, and always have
     * integer type. Denoted by symtype SYM_ENUM. */
    int enum_value;

    /* The n'th function argument. 1-indexed to keep 0 default. */
    int param_n;

    /* Argument or local variable offset to base pointer. */
    int stack_offset;

    /* Scope depth. */
    int depth;
} symbol_t;


/* Hold symbols and manage scopes in a namespace. There are three different 
 * types of namespaces in C (A11.1):
 *  1) Objects, functions, typedef names and enum constants.
 *  2) Labels.
 *  3) Structure, union and enum tags.
 *  4) Structure and union members, for each instance.
 *
 * Depth 0 is translation unit, depth 1 is function arguments, depth n is
 * local or memober variables.
 * Keep track of size of locals and number of parameters, as symbols are added
 * to different scope depths.
 */
struct scope;

typedef struct namespace {
    const char *name;
    symbol_t **symbol;
    int capacity, size;

    struct scope *scope;
    int depth;

    int var_stack_offset;
    int param_number;

} namespace_t;

namespace_t ns_ident, ns_label, ns_tag;

void push_scope(namespace_t *);

void pop_scope(namespace_t *);

void dump_symtab(namespace_t *);


/* Resolve symbol in current scope, or NULL if not found. Add new symbol based
 * on identifier name, or error if it is a duplicate. Create a new temporary 
 * symbol and register it to current scope.
 */
symbol_t *sym_lookup(namespace_t *, const char *);
symbol_t *sym_add(namespace_t *, const char *, const typetree_t *);

const symbol_t *sym_temp(namespace_t *, const typetree_t *);
const symbol_t *sym_temp_static(namespace_t *, const typetree_t *);

void register_builtin_types(namespace_t *);


#endif
