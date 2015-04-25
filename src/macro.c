#include "macro.h"
#include "error.h"
#include "input.h"
#include "util/map.h"

#include <assert.h>
#include <string.h>
#include <ctype.h>


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

void toklist_push_back_list(toklist_t *tl, toklist_t *tr)
{
    assert(tl && tr);

    tl->elem = realloc(tl->elem, sizeof(token_t) * (tl->length + tr->length));
    memcpy(tl->elem + tl->length, tr->elem, tr->length * sizeof(token_t));
    tl->length += tr->length;
}

/* Stringify a list of tokens. */
token_t toklist_to_string(toklist_t *tl)
{
    token_t t = {STRING, NULL, 0};
    char *buf = calloc(1, sizeof *buf);
    int i, len;

    assert(tl);
    for (i = len = 0; i < tl->length; ++i) {
        assert(tl->elem[i].strval);

        len = len + strlen(tl->elem[i].strval) + 1;
        buf = realloc(buf, len);
        strcat(buf, tl->elem[i].strval);
    }

    t.strval = buf;
    return t;
}

/* Append token string representation at the end of provided buffer. If NULL is
 * provided, a new buffer is allocated that must be free'd by caller. */
char *pastetok(char *buf, token_t t) {
    size_t len;

    if (!buf) {
        buf = calloc(16, sizeof(char));
        len = 0;
    } else {
        len = strlen(buf);
        if (t.strval) {
            buf = realloc(buf, len + strlen(t.strval) + 1);
        } else {
            buf = realloc(buf, len + 32);
        }
    }

    if (t.strval) {
        strcat(buf, t.strval);
    } else if (t.intval) {
        sprintf(buf + len, "%ld", t.intval);
    } else {
        assert(isprint(t.token));
        sprintf(buf + len, "%c", t.token);
    }

    return buf;
}

static toklist_t *expand_toklist(toklist_t *tl)
{
    toklist_t *res = toklist_init();
    int i;

    assert(tl);
    for (i = 0; i < tl->length; ++i) {
        macro_t *def = definition(tl->elem[i]);

        /* todo: skip if macro already expanded in this context (depth). */
        if (def) {
            if (def->type == FUNCTION_LIKE) {
                internal_error("%s.", "Unsupported macro type.");
                exit(1);
            }
            toklist_push_back_list(res, expand_macro(def, NULL));
        } else {
            toklist_push_back(res, tl->elem[i]);
        }
    }

    return res;
}

toklist_t *expand_macro(macro_t *def, toklist_t **args)
{
    int i, n;
    toklist_t *res, *prescanned;

    assert(def->type == FUNCTION_LIKE || !args);
    res = toklist_init();

    /* Rewrite this: First prescan all the params and put into one big replacement
     * list. Then do regular expand_list on that. No more duplication. */
    for (i = 0; i < def->size; ++i) {
        n = def->replacement[i].param;
        if (n) {
            prescanned = expand_toklist(args[n - 1]);
            toklist_push_back_list(res, prescanned);
        } else if (
            i < def->size - 1 &&
            def->replacement[i].token.token == '#' &&
            def->replacement[i + 1].param
        ) {
            i++;
            n = def->replacement[i].param;
            toklist_push_back(res, toklist_to_string(args[n - 1]));
        } else {
            macro_t *m = definition(def->replacement[i].token);

            /* Handle only direct self-referencing macros. */
            if (m && strcmp(m->name.strval, def->name.strval)) {
                if (m->type == FUNCTION_LIKE) {
                    internal_error("%s.", "Unsupported macro type.");
                    exit(1);
                }
                toklist_push_back_list(res, expand_macro(m, NULL));
            } else {
                toklist_push_back(res, def->replacement[i].token);    
            }
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
