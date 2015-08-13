#include "macro.h"
#include "error.h"
#include "input.h"

#include <assert.h>
#include <string.h>
#include <ctype.h>

static struct macro *definitions;
static size_t n_defs;

static int macrocmp(const struct macro *a, const struct macro *b)
{
    int i;
    if (strcmp(a->name.strval, b->name.strval) ||
        a->type != b->type ||
        a->params != b->params)
    {
        return 1;
    }

    for (i = 0; i < a->size; ++i) {
        if (a->replacement[i].param || b->replacement[i].param) {
            if (a->replacement[i].param != b->replacement[i].param) {
                return 1;
            }
        } else if (
            a->replacement[i].token.token != b->replacement[i].token.token ||
            strcmp(a->replacement[i].token.strval,
                b->replacement[i].token.strval))
        {
            return 1;
        }
    }
    return 0;
}

void define(struct macro macro)
{
    int i;
    for (i = 0; i < n_defs; ++i)
        if (!strcmp(definitions[i].name.strval, macro.name.strval))
            break;

    if (i < n_defs) {
        if (macrocmp(&definitions[i], &macro)) {
            error("Redefinition of macro '%s' with different substitution.",
                macro.name.strval);
            exit(1);
        }
        /* Already have this definition, but need to clean up memory that we
         * took ownership of. */
        if (macro.size) {
            free(macro.replacement);
        }
    } else {
        n_defs++;
        definitions = realloc(definitions, sizeof(*definitions) * n_defs);
        definitions[n_defs - 1] = macro;
    }
}

void undef(struct token name)
{
    int i;
    assert(name.strval);
    for (i = 0; i < n_defs; ++i)
        if (!strcmp(definitions[i].name.strval, name.strval))
            break;

    if (i < n_defs) {
        n_defs--;
        assert(n_defs - i >= 0);
        if (definitions[i].size) {
            free(definitions[i].replacement);
        }
        memmove(&definitions[i], &definitions[i + 1],
            (n_defs - i) * sizeof(*definitions));
    }
}

const struct macro *definition(struct token name)
{
    int i;
    assert(name.strval);
    for (i = 0; i < n_defs; ++i)
        if (!strcmp(definitions[i].name.strval, name.strval))
            break;

    if (i < n_defs) {
        if (!strcmp(definitions[i].name.strval, "__LINE__")) {
            definitions[i].replacement[0].token.intval = current_file.line;
        }
        return &definitions[i];
    }
    return NULL;
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

void toklist_push_back(toklist_t *tl, struct token t)
{
    assert(tl);

    tl->length++;
    tl->elem = realloc(tl->elem, sizeof(struct token) * tl->length);
    tl->elem[tl->length - 1] = t;
}

void toklist_push_back_list(toklist_t *tl, toklist_t *tr)
{
    assert(tl && tr);

    tl->elem = 
        realloc(tl->elem, sizeof(struct token) * (tl->length + tr->length));
    memcpy(tl->elem + tl->length, tr->elem, tr->length * sizeof(struct token));
    tl->length += tr->length;
}

struct token toklist_to_string(toklist_t *tl)
{
    struct token t = {STRING, NULL, 0};
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

char *pastetok(char *buf, struct token t) {
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

/* Keep track of which macros have been expanded, avoiding recursion by looking
 * up in this list for each new expansion.
 */
static struct {
    const char *name;
} *expand_stack;
static int stack_size;

static void push_expand_stack(const char *macro_name)
{
    stack_size++;
    expand_stack = realloc(expand_stack, stack_size * sizeof(*expand_stack));
    expand_stack[stack_size - 1].name = macro_name;
}

static void pop_expand_stack(void)
{
    assert(stack_size);
    stack_size--;
    expand_stack = realloc(expand_stack, stack_size * sizeof(*expand_stack));
}

static int is_macro_expanded(const char *macro_name)
{
    int i;
    for (i = 0; i < stack_size; ++i)
        if (!strcmp(expand_stack[i].name, macro_name))
            return 1;
    return 0;
}

void print_list(const toklist_t *list, unsigned i)
{
    printf("[");
    if (i < list->length) {
        printf("'%s'", list->elem[i].strval);
    }
    i++;
    while (i < list->length) {
        printf(", '%s'", list->elem[i].strval);
        i++;
    }
    printf("]\n");
}

/* Paste together two tokens, forming a new token which has to be re-scanned
 * by tokenizer.
 */
static struct token paste_tokens(struct token left, struct token right)
{
    extern struct token tokenize(char *in, char **endptr);
    struct token result;
    size_t length;
    char *data, *endptr;

    assert(left.strval && right.strval);

    length = strlen(left.strval) + strlen(right.strval);
    data = calloc(length + 1, sizeof(char));
    strcpy(data, left.strval);
    strcat(data, right.strval);
    result = tokenize(data, &endptr);
    if (endptr != data + length) {
        error("Invalid token resulting from pasting '%s' and '%s'.",
            left.strval, right.strval);
        exit(1);
    }
    free(data);

    return result;
}

/* Resolve token pasting with '##' operator.
 */
static toklist_t *expand_paste_operators(toklist_t *list)
{
    int i = 1,  /* Index into list. */
        j = 0;  /* Index into result. */
    toklist_t *res = (list->length) ? toklist_init() : list;

    if (!list->length) {
        return res;
    }
    if (list->elem[0].token == TOKEN_PASTE) {
        error("Invalid token paste operator at beginning of line.");
        exit(1);
    }
    if (list->elem[list->length - 1].token == TOKEN_PASTE) {
        error("Invalid token paste operator at end of line.");
        exit(1);
    }

    /* Overwrite last element in result list for each paste occurrence. */
    toklist_push_back(res, list->elem[0]);
    for (; i < list->length - 1; ++i) {
        if (list->elem[i].token == TOKEN_PASTE) {
            struct token
                left = res->elem[j],
                right = list->elem[i + 1];
            res->elem[j] = paste_tokens(left, right);
            i += 1;
        } else {
            toklist_push_back(res, list->elem[i]);
            j += 1;
        }
    }

    /* Include last element unless it has already been pasted. */
    if (i < list->length) {
        toklist_push_back(res, list->elem[i]);
    }
    return res;
}

/* Expand a macro with given arguments to a list of tokens.
 */
static toklist_t *expand_macro(const struct macro *def, toklist_t **args)
{
    int i, n;
    toklist_t *res, *prescanned;
    assert(def->type == FUNCTION_LIKE || !args);

    push_expand_stack(def->name.strval);
    res = toklist_init();
    for (i = 0; i < def->size; ++i) {
        n = def->replacement[i].param;
        if (n) {
            prescanned = expand(args[n - 1]);
            toklist_push_back_list(res, prescanned);
        } else if (
            i < def->size - 1 &&
            def->replacement[i].token.token == '#' &&
            def->replacement[i + 1].param)
        {
            i++;
            n = def->replacement[i].param;
            toklist_push_back(res, toklist_to_string(args[n - 1]));
        } else {
            toklist_push_back(res, def->replacement[i].token);
        }
    }
    res = expand_paste_operators(res);
    res = expand(res);
    pop_expand_stack();

    return res;
}

toklist_t *expand(toklist_t *tl)
{
    int i;
    toklist_t *res = toklist_init();

    assert(tl);

    for (i = 0; i < tl->length; ++i) {
        const struct macro *def;

        if (tl->elem[i].token == IDENTIFIER &&
            (def = definition(tl->elem[i])) &&
            !is_macro_expanded(def->name.strval))
        {
            toklist_t **args = NULL;
            if (def->type == FUNCTION_LIKE) {
                int j,
                    nesting = 0;    /* Keep track parenthesis nesting level. */

                #define skip_ws(l, i)                                          \
                    while (i < l->length && l->elem[i].token == SPACE) {       \
                        i++;                                                   \
                    }

                #define expect_token_at(l, i, t)                               \
                    if (i >= l->length || l->elem[i].token != t) {             \
                        error("Unexpected input '%s', expected '%c'.",         \
                            l->elem[i].strval, (char) (t));                    \
                        exit(1);                                               \
                    }

                i++;
                skip_ws(tl, i);
                expect_token_at(tl, i, '(');
                i++;
                if (def->params) {
                    args = calloc(def->params, sizeof(*args));
                }
                for (j = 0; j < def->params; ++j) {
                    struct token next;
                    args[j] = toklist_init();
                    while (1) {
                        if (i >= tl->length) {
                            error("Unexpected end of input.");
                            exit(1);
                        }
                        skip_ws(tl, i);
                        next = tl->elem[i];
                        if (!nesting &&
                            (next.token == ',' || next.token == ')'))
                        {
                            /* Got a valid argument separator, next param. */
                            break;
                        }
                        i++;
                        if (next.token == ',' && !nesting) {
                            error(
                                "Expansion of '%s' does not match definition.",
                                def->name.strval);
                            exit(1);
                        }
                        if (next.token == '(') {
                            nesting++;
                        } else if (next.token == ')') {
                            nesting--;
                            if (nesting < 0) {
                                error("Negative nesting depth in expansion.");
                                exit(1);
                            }
                        }
                        toklist_push_back(args[j], next);
                    }
                    if (j < def->params - 1) {
                        skip_ws(tl, i);
                        expect_token_at(tl, i, ',');
                        i++;
                        /* no i++ since it will be done next while iteration. */
                    }
                }
                skip_ws(tl, i);
                expect_token_at(tl, i, ')');
                /* no i++, because reasons.. */

                #undef skip_ws
                #undef expect_token_at
            }

            /* Push result of macro expansion. */
            toklist_push_back_list(res, expand_macro(def, args));
        } else {
            toklist_push_back(res, tl->elem[i]);
        }
    }

    return res;
}

static struct replacement *parse(char *str, size_t *out_size)
{
    extern struct token tokenize(char *in, char **endptr);

    char *endptr;
    size_t n = 0;
    struct replacement *repl = NULL;

    while (*str) {
        n++;
        repl = realloc(repl, sizeof(*repl) * n);
        memset(repl + n - 1, 0x0, sizeof(*repl));
        if (*str == '@') {
            repl[n - 1].param = 1;
            str++;
        } else {
            repl[n - 1].token = tokenize(str, &endptr);
            assert(str != endptr);
            str = endptr;
        }
    }
    *out_size = n;
    return repl;
}

static void register__builtin_va_end(void)
{
    struct macro macro = {
        {IDENTIFIER, "__builtin_va_end"},
        FUNCTION_LIKE,
        1, /* parameters */
    };

    macro.replacement = parse(
        "@[0].gp_offset=0;"
        "@[0].fp_offset=0;"
        "@[0].overflow_arg_area=(void*)0;"
        "@[0].reg_save_area=(void*)0;", &macro.size);

    assert(macro.size == 44);
    define(macro);
}

void register_builtin_definitions(void)
{
    struct macro macro = {
        {IDENTIFIER, NULL, 0},
        OBJECT_LIKE,
        0, /* parameters */
    };

    macro.name.strval = "__STDC_VERSION__";
    macro.replacement = parse("199409L", &macro.size);
    define(macro);

    macro.name.strval = "__STDC__";
    macro.replacement = parse("1", &macro.size);
    define(macro);

    macro.name.strval = "__STDC_HOSTED__";
    macro.replacement = parse("1", &macro.size);
    define(macro);

    macro.name.strval = "__LINE__";
    macro.replacement = parse("0", &macro.size);
    define(macro);

    macro.name.strval = "__FILE__";
    macro.replacement = calloc(1, sizeof(*macro.replacement));
    macro.replacement[0].token.token = STRING;
    macro.replacement[0].token.strval = current_file.path;
    macro.replacement[0].token.intval = 0;
    define(macro);

    register__builtin_va_end();
}
