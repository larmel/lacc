#ifndef SYMBOL_H
#define SYMBOL_H
#if !defined(INTERNAL) || !defined(EXTERNAL)
# error Missing amalgamation macros
#endif

#include "string.h"
#include "token.h"
#include "type.h"

#include <stdio.h>

struct block;
struct definition;

enum symtype {
    SYM_DEFINITION = 0,
    SYM_TENTATIVE,
    SYM_DECLARATION,
    SYM_TYPEDEF,
    SYM_LITERAL,
    SYM_CONSTANT,
    SYM_LABEL,
    SYM_TAG,
    SYM_BUILTIN
};

/* Visibility of external declarations, LINK_NONE for other symbols. */
enum linkage {
    LINK_NONE = 0,
    LINK_INTERN,
    LINK_EXTERN
};

/*
 * A symbol represents declarations that may have a storage location at
 * runtime, such as functions, static and local variables.
 */
struct symbol {
    String name;
    Type type;

    unsigned int symtype : 8;
    unsigned int linkage : 8;
    unsigned int referenced : 1; /* Mark symbol as used. */
    unsigned int memory : 1;     /* Disable register allocation. */
    unsigned int inlined : 1;    /* Inline function. */
    unsigned int : 1;
    unsigned int slot : 4;       /* Register allocation slot. */
    unsigned int index : 8;      /* Enumeration used in optimization. */

    /*
     * Tag to disambiguate temporaries, strings, constants, labels, and
     * scoped static variables.
     */
    short n;

    /*
     * Scope depth; 0 for global, 1 for function parameters, > 1 for
     * local or scoped static variables.
     */
    short depth;

    /*
     * Parameter or local variable offset to base pointer. This is kept
     * as 0 during parsing, but assigned when passed to back-end.
     *
     * Also used for index into .symtab during ELF generation.
     */
    int stack_offset;

    union {
        /*
         * Hold a constant integral or floating point value. Used for
         * enumeration members and numbers which must be loaded from
         * memory in assembly code. Denoted by symtype SYM_CONSTANT.
         */
        union value constant;

        /*
         * String literals are also handled as symbols, having type []
         * const char. Denoted by symtype SYM_LITERAL. Free string
         * constants are always named '.LC', disambiguated with n.
         */
        String string;

        /*
         * Symbols in label namespace hold a pointer to the block they
         * represent.
         */
        struct block *label;

        /*
         * Location in memory of variable length array. Set to current
         * position of stack pointer on IR_VLA_ALLOC, after subtracting
         * total size of the array.
         */
        const struct symbol *vla_address;

        /*
         * Translation rule for intrinsic builtin functionality, such as
         * va_arg or alloca.
         */
        struct block *(*handler)(struct definition *, struct block *);
    } value;
};

/* Holds the declaration for memcpy, which is needed for codegen. */
EXTERNAL const struct symbol *decl_memcpy;

/* Get the full name, including numeric value to disambiguate. */
INTERNAL const char *sym_name(const struct symbol *sym);

/*
 * Determine if given symbol is a temporary value generated during
 * evaluation.
 */
INTERNAL int is_temporary(const struct symbol *sym);

/*
 * Create a floating point constant, which can be stored and loaded from
 * memory.
 */
INTERNAL struct symbol *sym_create_constant(Type type, union value val);

#endif
