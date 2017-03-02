#ifndef SYMBOL_H
#define SYMBOL_H

#include "string.h"
#include "token.h"
#include "type.h"

#include <stdio.h>

struct block;

/*
 * A symbol represents declarations that may have a storage location at
 * runtime, such as functions, static and local variables. Store offset
 * to base pointer for automatic variables and function arguments.
 */
struct symbol {
    String name;
    Type type;

    enum symtype {
        SYM_DEFINITION = 0,
        SYM_TENTATIVE,
        SYM_DECLARATION,
        SYM_TYPEDEF,
        SYM_STRING_VALUE,
        SYM_CONSTANT,
        SYM_LABEL
    } symtype;

    /*
     * Visibility of external declarations, or LINK_NONE for other
     * symbols.
     */
    enum linkage {
        LINK_NONE = 0,
        LINK_INTERN,
        LINK_EXTERN
    } linkage;

    /*
     * Hold a constant integral or floating point value. Used for
     * enumeration members and numbers which must be loaded from memory
     * in assembly code. Denoted by symtype SYM_CONSTANT.
     */
    union value constant_value;

    /*
     * String literals are also handled as symbols, having type [] const
     * char. Denoted by symtype SYM_STRING_VALUE. Free string constants
     * are always named '.LC', disambiguated with n.
     */
    String string_value;

    /*
     * Symbols in label namespace hold a pointer to the block they
     * represent.
     */
    struct block *label_value;

    /*
     * Tag to disambiguate differently scoped static variables with the
     * same name.
     */
    int n;

    /*
     * Parameter or local variable offset to base pointer. This is kept
     * as 0 during parsing, but assigned when passed to back-end.
     */
    int stack_offset;

    /*
     * Location in memory of variable length array. Set on IR_VLA_ALLOC
     * to current position of stack pointer after subtracting total size
     * of the array.
     */
    const struct symbol *vla_address;

    /* Scope depth. */
    int depth;

    /* Number of times symbol has been referenced in the program. */
    int referenced;

    /* Counter used for enumeration during optimization. */
    int index;

    /* Register allocation slot. */
    int slot;
};

/* Holds the declaration for memcpy, which is needed for codegen. */
extern const struct symbol *decl_memcpy;

/* Get the full name, including numeric value to disambiguate. */
const char *sym_name(const struct symbol *sym);

/*
 * Determine if given symbol is a temporary value generated during
 * evaluation.
 */
int is_temporary(const struct symbol *sym);

/*
 * Create a floating point constant, which can be stored and loaded from
 * memory.
 */
struct symbol *sym_create_constant(Type type, union value val);

#endif
