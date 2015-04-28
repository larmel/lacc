#ifndef MACRO_H
#define MACRO_H

#include "preprocess.h"

#include <stdlib.h>


typedef struct {
    struct token name;
    enum { OBJECT_LIKE, FUNCTION_LIKE } type;

    size_t params;
    size_t size;

    /* A substitution is either a token or a parameter. */
    struct macro_subst_t {
        struct token token;
        int param;
    } *replacement;

} macro_t;

typedef struct toklist {
    struct token *elem;
    size_t length;
    size_t cap;
} toklist_t;

toklist_t *toklist_init();
void toklist_push_back(toklist_t *, struct token);
void toklist_destroy(toklist_t *);
struct token toklist_to_string(toklist_t *);

char *pastetok(char *, struct token);

void define_macro(macro_t *);

void define(struct token, struct token);

void undef(struct token name);

macro_t *definition(struct token);

toklist_t *expand_macro(macro_t *, toklist_t **);

void register_builtin_definitions();


#endif
