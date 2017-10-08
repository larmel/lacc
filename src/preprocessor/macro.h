#ifndef MACRO_H
#define MACRO_H

#include <lacc/array.h>
#include <lacc/token.h>

typedef array_of(struct token) TokenArray;

/* Get empty token array, possibly already allocated with capacity. */
TokenArray get_token_array(void);

/* Release token array previously aquired by get_token_array. */
void release_token_array(TokenArray list);

struct macro {
    String name;

    enum {
        OBJECT_LIKE,
        FUNCTION_LIKE
    } type;

    /* Number of parameters required for substitution. */
    int params;

    unsigned int is__line__ : 1;
    unsigned int is__file__ : 1;
    unsigned int is_vararg : 1;

    /*
     * A substitution is either a token or a parameter, and parameters
     * are represented by PARAM tokens with an integer index between
     * 0 and params.
     */
    TokenArray replacement;
};

/* Define standard macros. */
void register_builtin_definitions(void);

/* Stringify a list of tokens, returning a new token of type STRING. */
struct token stringify(const TokenArray *list);

/*
 * Add macro definition. Takes ownership of any dynamically allocated
 * replacement list.
 */
void define(struct macro macro);

/*
 * Remove macro definition corresponding to identifier. If the name has
 * not previously been defined, this is a no-op.
 */
void undef(String name);

/* Look up definition of identifier, or NULL if not defined. */
const struct macro *macro_definition(String name);

/*
 * Expand a list of tokens, replacing any macro definitions. Mutates
 * input list as necessary. Return non-zero if any macro was expanded.
 */
int expand(TokenArray *list);

/*
 * Compare tokens for equality, returing 0 iff of same type and value.
 */
int tok_cmp(struct token a, struct token b);

#endif
