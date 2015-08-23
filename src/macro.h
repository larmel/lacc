#ifndef MACRO_H
#define MACRO_H

#include "preprocess.h"

#include <stdlib.h>

struct macro {
    struct token name;
    enum { OBJECT_LIKE, FUNCTION_LIKE } type;

    size_t params;
    size_t size;

    /* A substitution is either a token or a parameter. */
    struct replacement {
        struct token token;
        int param;
    } *replacement;
};

struct toklist {
    struct token *elem;
    size_t length;
};

struct toklist *toklist_init(void);
void toklist_push_back(struct toklist *, struct token);
void toklist_destroy(struct toklist *);

/* Stringify a list of tokens.
 */
struct token toklist_to_string(struct toklist *list);

/* Append token string representation at the end of provided buffer. If NULL is
 * provided, a new buffer is allocated that must be free'd by caller.
 */
char *pastetok(char *buf, struct token);

/* Add macro definition. Takes ownership of any dynamically allocated
 * replacement list.
 */
void define(struct macro macro);

/* Remove macro definition corresponding to identifier. If the name has not
 * previously been defined, this is a no-op.
 */
void undef(struct token name);

/* Look up definition of identifier, or NULL if not defined.
 */
const struct macro *definition(struct token);

/* Expand a list of tokens, replacing any macro definitions. 
 */
struct toklist *expand(struct toklist *tl);

#endif
