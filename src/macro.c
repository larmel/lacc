#include "macro.h"
#include "error.h"
#include "input.h"
#include "util/map.h"

#include <assert.h>


static map_t definitions;

void define_macro(macro_t *macro)
{
    map_insert(&definitions, macro->name.strval, (void *) macro);
}

void define(token_t name, token_t subst)
{
    macro_t *macro;

    assert(name.strval);

    macro = map_lookup(&definitions, name.strval);
    if (!macro) {
        macro_t *p = malloc(sizeof(macro_t));
        p->name = name;
        p->replacement = malloc(1 * sizeof(struct macro_subst_t));
        p->replacement[0].token = subst;
        p->replacement[0].param = 0;
        p->size = 1;
        map_insert(&definitions, name.strval, (void *) p);
    } else {
        error("Redefinition of macro %s.", name.strval);
    }
}

void undef(token_t name)
{
    assert(name.strval);

    /* No-op if name is not a macro. */
    map_remove(&definitions, name.strval);
}

macro_t *definition(token_t name)
{
    macro_t *macro = NULL;

    if (name.strval) {
        macro = map_lookup(&definitions, name.strval);
        if (macro && macro->name.token == IDENTIFIER) {
            if (!strcmp(macro->name.strval, "__LINE__")) {
                macro->replacement[0].token.intval = current_file.line;
            }
        }
    }
    return macro;
}

toklist_t *toklist_init()
{
    return calloc(1, sizeof(toklist_t));
}

void toklist_destroy(toklist_t *tl)
{
    assert(tl);
    if (tl->elem) {
        free(tl->elem);
        tl->elem = NULL;
    }
    free(tl);
}

void toklist_push_back(toklist_t *tl, token_t t)
{
    assert(tl);

    tl->length++;
    tl->elem = realloc(tl->elem, sizeof(token_t) * tl->length);
    tl->elem[tl->length - 1] = t;
}

toklist_t *expand_macro(macro_t *def, toklist_t **args)
{
    int i, j, n;
    toklist_t *res;

    assert(def->type == FUNCTION_LIKE || !args);
    res = toklist_init();

    for (i = 0; i < def->size; ++i) {
        n = def->replacement[i].param;
        if (n) {
            for (j = 0; j < args[n - 1]->length; ++j) {
                toklist_push_back(res, args[n - 1]->elem[j]);
            }
        } else {
            toklist_push_back(res, def->replacement[i].token);
        }
    }

    return res;
}

void register_builtin_definitions()
{
    token_t 
        name = { IDENTIFIER, NULL, 0 },
        valu = { INTEGER_CONSTANT, NULL, 0 };

    name.strval = "__STDC_VERSION__";
    valu.intval = 199409L;
    define( name, valu );

    name.strval = "__STDC__";
    valu.intval = 1;
    define( name, valu );

    name.strval = "__STDC_HOSTED__";
    valu.intval = 1;
    define( name, valu );

    name.strval = "__LINE__";
    valu.intval = 0;
    define( name, valu );

    name.strval = "__FILE__";
    valu.token = STRING;
    valu.strval = current_file.path;
    define( name, valu );
}
