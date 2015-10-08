#include "core/error.h"
#include "macro.h"
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
    if (name.token == IDENTIFIER) {
        for (i = 0; i < n_defs; ++i)
            if (!strcmp(definitions[i].name.strval, name.strval))
                break;

        if (i < n_defs) {
            if (!strcmp(definitions[i].name.strval, "__LINE__")) {
                definitions[i].replacement[0].token.intval = current_file.line;
            }
            return &definitions[i];
        }
    }
    return NULL;
}

/* Keep track of which macros have been expanded, avoiding recursion by looking
 * up in this list for each new expansion.
 */
static const struct macro **expand_stack;
static size_t stack_size;

static int is_macro_expanded(const struct macro *macro)
{
    size_t i = 0;
    for (; i < stack_size; ++i)
        if (!strcmp(expand_stack[i]->name.strval, macro->name.strval))
            return 1;
    return 0;
}

static void push_expand_stack(const struct macro *macro)
{
    assert(!is_macro_expanded(macro));
    stack_size++;
    expand_stack = realloc(expand_stack, stack_size * sizeof(*expand_stack));
    expand_stack[stack_size - 1] = macro;
}

static void pop_expand_stack(void)
{
    assert(stack_size);
    stack_size--;
    if (!stack_size) {
        free(expand_stack);
        expand_stack = NULL;
    }
}

/* Calculate length of list, excluding trailing END marker.
 */
static size_t len(const struct token *list)
{
    size_t i = 0;
    assert(list);
    while (list[i].token != END)
        i++;
    return i;
}

void print_list(const struct token *list)
{
    int first = 1;
    size_t l = len(list);
    printf("[");
    while (list->token != END) {
        if (!first)
            printf(", ");
        printf("'%s'", list->strval);
        first = 0;
        list++;
    }
    printf("] (%lu)\n", l);
}

/* Extend input list with concatinating another list to it. Takes ownership of
 * both arguments.
 */
static struct token *concat(struct token *list, struct token *other)
{
    size_t i = len(list);
    size_t j = len(other);

    list = realloc(list, (i + j + 1) * sizeof(*list));
    memmove(list + i, other, (j + 1) * sizeof(*list));
    assert(list[i + j].token == END);
    free(other);
    return list;
}

/* Extend input list by a single token. Take ownership of input.
 */
static struct token *append(struct token *list, struct token other)
{
    size_t i = len(list);
    assert(list[i].token == END);
    list = realloc(list, (i + 2) * sizeof(*list));
    list[i + 1] = list[i];
    list[i] = other;
    return list;
}

/* Paste together two tokens.
 */
static struct token paste(struct token left, struct token right)
{
    struct token result;
    size_t length;
    char *data, *endptr;

    length = strlen(left.strval) + strlen(right.strval);
    data   = calloc(length + 1, sizeof(*data));
    data   = strcpy(data, left.strval);
    data   = strcat(data, right.strval);
    result = tokenize(data, &endptr);
    if (endptr != data + length) {
        error("Invalid token resulting from pasting '%s' and '%s'.",
            left.strval, right.strval);
        exit(1);
    }

    free(data);
    return result;
}

static struct token *skip_ws(struct token *list)
{
    while (list->token == SPACE) list++;
    return list;
}

#define SKIP_WS(lst) \
    while (lst->token == SPACE) lst++;

/* In-place expansion of token paste operators, '##'.
 * ['foo', ' ', '##', '_f', ' ', '##', ' ', 'u', '##', 'nc']
 * becomes
 * ['foo_func']
 *
 * NB: Probably not preserving whitespace..
 */
static struct token *expand_paste_operators(struct token *list)
{
    struct token
        *start = list,
        *end;

    if (list->token == END) {
        return list;
    }

    end = skip_ws(list + 1);

    if (start->token == TOKEN_PASTE) {
        error("Invalid token paste operator at beginning of line.");
        exit(1);
    }

    while (end->token != END) {
        if (end->token == TOKEN_PASTE) {
            end = skip_ws(end + 1);
            if (end->token == END) {
                error("Invalid paste operator at end of line.");
                exit(1);
            }
            *start = paste(*start, *end);
            end = skip_ws(end + 1);
        } else {
            do {
                start++;
                *start = *end++;
            } while (end->token == SPACE);
        }
    }

    *(start + 1) = *end;
    return list;
}

static struct token end_token = {END, "$"};

static struct token *expand_macro(
    const struct macro *macro,
    struct token *args[])
{
    size_t i;
    struct token *res = calloc(1, sizeof(*res));

    res[0] = end_token;
    push_expand_stack(macro);
    for (i = 0; i < macro->size; ++i) {
        int n = macro->replacement[i].param;
        if (n) {
            res = concat(res, expand(args[n - 1]));
        } else if (
            i < macro->size - 1 &&
            macro->replacement[i].token.token == '#' &&
            macro->replacement[i + 1].param)
        {
            i++;
            n = macro->replacement[i].param;
            res = append(res, stringify(args[n - 1]));
        } else {
            res = append(res, macro->replacement[i].token);
        }
    }
    res = expand_paste_operators(res);
    res = expand(res);
    pop_expand_stack();

    for (i = 0; i < macro->params; ++i) {
        free(args[i]);
    }
    free(args);

    return res;
}

static const struct token *skip_to(const struct token *list, int token)
{
    while (list->token == SPACE) list++;
    if (list->token != token) {
        error("Unexpected '%c', expected '%c'.", list->strval);
    }
    return list;
}

static const struct token *skip_past(const struct token *list, int token)
{
    list = skip_to(list, token) + 1;
    while (list->token == SPACE) list++;
    return list;
}

/* Read argument in macro expansion, starting from one offset from the initial
 * open parenthesis. Stop readin when reaching a comma, and nesting depth is
 * zero. Track nesting depth to allow things like MAX( foo(a), b ).
 */
static struct token *read_arg(
    const struct token *list,
    const struct token **endptr)
{
    size_t n = 0;
    struct token *arg = calloc(1, sizeof(*arg));
    int nesting = 0;

    SKIP_WS(list);
    do {
        if (list->token == END) {
            error("Unexpected end of input in expansion.");
            exit(1);
        }
        if (list->token == '(') {
            nesting++;
        } else if (list->token == ')') {
            nesting--;
            if (nesting < 0) {
                error("Negative nesting depth in expansion.");
                exit(1);
            }
        }
        arg = realloc(arg, (++n + 1) * sizeof(*arg));
        arg[n - 1] = *list++;
        SKIP_WS(list);
    } while (nesting || (list->token != ',' && list->token != ')'));

    arg[n] = end_token;
    *endptr = list;
    return arg;
}

static struct token **read_args(
    const struct token *list,
    const struct token **endptr,
    const struct macro *macro)
{
    struct token **args = calloc(macro->params, sizeof(*args));
    int n = 0;

    if (macro->type == FUNCTION_LIKE) {
        list = skip_past(list, '(');
        for (; n < macro->params; ++n) {
            args[n] = read_arg(list, &list);
            if (n < macro->params - 1) {
                list = skip_past(list, ',');
            }
        }
        list = skip_past(list, ')');
    }

    *endptr = list;
    return args;
}

struct token *expand(const struct token list[])
{
    struct token *res = calloc(1, sizeof(*res));

    res[0] = end_token;
    while (list->token != END) {
        const struct macro *def = definition(*list);
        struct token **args;

        if (def && !is_macro_expanded(def)) {
            args = read_args(list + 1, &list, def);
            res = concat(res, expand_macro(def, args));
        } else {
            res = append(res, *list++);
        }
    }
    return res;
}

struct token stringify(const struct token list[])
{
    char *str = calloc(1, sizeof(*str));
    size_t len = 0;
    struct token t = {STRING};

    while (list->token != END) {
        len += strlen(list->strval);
        str  = realloc(str, (len + 1) * sizeof(*str));
        str  = strncat(str, list->strval, len);
        list++;
    }
    t.strval = str;
    return t;
}

static struct replacement *parse(char *str, size_t *out_size)
{
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

    macro.name.strval = "__x86_64__";
    macro.replacement = parse("1", &macro.size);
    define(macro);

    macro.name.strval = "__FILE__";
    macro.replacement = calloc(1, sizeof(*macro.replacement));
    macro.replacement[0].token.token = STRING;
    macro.replacement[0].token.strval = current_file.path;
    macro.replacement[0].token.intval = 0;
    define(macro);

    register__builtin_va_end();
}
