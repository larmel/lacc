#ifndef SYMBOL_H
#define SYMBOL_H

#include "typetree.h"

#include <stdio.h>

/* A symbol represents declarations that may have a storage location at runtime,
 * such as functions, static and local variables. Store offset to base pointer
 * for automatic variables and function arguments.
 */
struct symbol {
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
        SYM_ENUM_VALUE,
        SYM_STRING_VALUE,
        SYM_LABEL
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

    /* String literals are also handled as symbols, having type [] const char.
     * Denoted by symtype SYM_STRING_VALUE. Free string constants are always
     * named '.LC', disambiguated with n. */ 
    const char *string_value;

    /* Parameter or local variable offset to base pointer. This is kept as 0
     * during parsing, but assigned when passed to back-end. */
    int stack_offset;

    /* Scope depth. */
    int depth;

    /* Number of times symbol has been referenced in the program. */
    int referenced;
};

struct symbol_list {
    struct symbol **symbol;
    int length;
    int capacity;
};

/* Holds the declaration for memcpy, which is needed for codegen.
 */
extern const struct symbol *decl_memcpy;

/* Get the full name, including numeric value to disambiguate.
 */
const char *sym_name(const struct symbol *sym);

/* Create a jump label symbol, of type void.
 */
struct symbol *sym_create_label(void);

#endif
