#include "input.h"
#include "macro.h"
#include "strtab.h"
#include "tokenize.h"
#include <lacc/cli.h>
#include <lacc/hash.h>

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

#define HASH_TABLE_BUCKETS 1024

static struct hash_table macro_hash_table;
static int initialized;
static int new_macro_added;

static int macrocmp(const struct macro *a, const struct macro *b)
{
    int i;
    struct replacement ra, rb;

    if ((a->type != b->type) || (a->params != b->params))
        return 1;

    if (tok_cmp(a->name, b->name))
        return 1;

    if (a->size != b->size)
        return 1;

    for (i = 0; i < a->size; ++i) {
        ra = a->replacement[i];
        rb = b->replacement[i];

        if ((ra.param || rb.param) && (ra.param != rb.param))
            return 1;

        if (tok_cmp(ra.token, rb.token))
            return 1;
    }

    return 0;
}

static struct string macro_hash_key(void *ref)
{
    return ((struct macro *) ref)->name.strval;
}

static void macro_hash_del(void *ref)
{
    struct macro *macro = (struct macro *) ref;
    if (macro->replacement)
        free(macro->replacement);
    free(macro);
}

static void *macro_hash_add(void *ref)
{
    struct macro *macro, *arg;

    arg = (struct macro *) ref;
    macro = calloc(1, sizeof(*macro));
    *macro = *arg;

    /* Signal that the hash table has ownership now, and it will not be
     * freed in define(). */
    new_macro_added = 1;
    return macro;
}

static void cleanup(void)
{
    hash_destroy(&macro_hash_table);
}

static void initialize(void)
{
    assert(!initialized);
    hash_init(
        &macro_hash_table,
        HASH_TABLE_BUCKETS,
        macro_hash_key,
        macro_hash_add,
        macro_hash_del);
    atexit(cleanup);
    initialized = 1;
}

const struct macro *definition(struct token name)
{
    struct macro *ref = NULL;

    if (!initialized)
        initialize();

    if (name.token == IDENTIFIER) {
        ref = hash_lookup(&macro_hash_table, name.strval);
        if (ref) {
            /* Replace __LINE__ with current line number, by mutating
             * the replacement list on the fly. */
            if (!str_cmp(ref->name.strval, str_init("__LINE__")))
                ref->replacement[0].token.intval = current_file.line;
        }
    }

    return ref;
}

void define(struct macro macro)
{
    struct macro *ref;

    if (!initialized)
        initialize();

    new_macro_added = 0;
    ref = hash_insert(&macro_hash_table, &macro);
    if (macrocmp(ref, &macro)) {
        error("Redefinition of macro '%s' with different substitution.",
            macro.name.strval.str);
        exit(1);
    }

    /* Need to clean up memory for replacement list since ownership was
     * not given to hash table. */
    if (!new_macro_added && macro.replacement)
        free(macro.replacement);
}

void undef(struct token name)
{
    if (!initialized)
        initialize();

    if (name.token == IDENTIFIER)
        hash_remove(&macro_hash_table, name.strval);
}

/* Keep track of which macros have been expanded, avoiding recursion by
 * looking up in this list for each new expansion.
 */
static const struct macro **expand_stack;
static int stack_size;

static int is_macro_expanded(const struct macro *macro)
{
    int i = 0;
    for (; i < stack_size; ++i)
        if (!tok_cmp(expand_stack[i]->name, macro->name))
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
        printf("'");
        if (list->leading_whitespace > 0) {
            printf("%*s", list->leading_whitespace, " ");
        }
        if (list->token == NEWLINE)
            printf("\\n");
        else
            printf("%s", list->strval.str);
        printf("'");
        first = 0;
        list++;
    }
    printf("] (%lu)\n", l);
}

/* Extend input list with concatinating another list to it. Takes
 * ownership of both arguments.
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

static struct token *copy(const struct token *list)
{
    size_t i = len(list) + 1;
    struct token *c = calloc(i, sizeof(*c));

    return memcpy(c, list, i * sizeof(*c));
}

/* Paste together two tokens.
 */
static struct token paste(struct token left, struct token right)
{
    struct token result;
    size_t length;
    char *data, *endptr;

    length = left.strval.len + right.strval.len;
    data   = calloc(length + 1, sizeof(*data));
    data   = strcpy(data, left.strval.str);
    data   = strcat(data, right.strval.str);
    result = tokenize(data, &endptr);
    if (endptr != data + length) {
        error("Invalid token resulting from pasting '%s' and '%s'.",
            left.strval.str, right.strval.str);
        exit(1);
    }

    result.leading_whitespace = left.leading_whitespace;
    free(data);
    return result;
}

#define SKIP_WS(lst) \
    while (list->token == NEWLINE) lst++;

/* In-place expansion of token paste operators.
 * ['foo', '##', '_f', '##', 'u', '##', 'nc'] becomes ['foo_func']
 */
static struct token *expand_paste_operators(struct token *list)
{
    struct token
        *ptr = list,
        *end = list + 1;

    if (list->token == END)
        return list;

    if (list->token == TOKEN_PASTE) {
        error("Unexpected token paste operator at beginning of line.");
        exit(1);
    }

    while (end->token != END) {
        if (end->token == TOKEN_PASTE) {
            end++;
            if (end->token == END) {
                error("Unexpected token paste operator at end of line.");
                exit(1);
            }
            *ptr = paste(*ptr, *end);
            end++;
        } else {
            *(++ptr) = *end++;
            assert(end->token != NEWLINE);
        }
    }

    *(ptr + 1) = *end;
    return list;
}

static struct token *expand_macro(
    const struct macro *macro,
    struct token *args[])
{
    size_t i;
    struct token *res = calloc(1, sizeof(*res));

    res[0] = basic_token[END];
    push_expand_stack(macro);
    for (i = 0; i < macro->size; ++i) {
        int n = macro->replacement[i].param;
        if (n) {
            /* Create a copy of args before expanding to avoid it being
             * free'd. */
            res = concat(res, expand(copy(args[n - 1])));
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
    while (list->token == NEWLINE)
        list++;

    if (list->token != token) {
        assert(basic_token[token].strval.str);
        error("Expected '%s', but got '%s'.",
            basic_token[token].strval.str, list->strval.str);
    }

    return list;
}

static const struct token *skip_past(const struct token *list, int token)
{
    list = skip_to(list, token) + 1;
    while (list->token == NEWLINE)
        list++;

    return list;
}

static enum token_type peek_next(const struct token *list)
{
    while (list->token == NEWLINE)
        list++;

    return list->token;
}

/* Read argument in macro expansion, starting from one offset from the
 * initial open parenthesis. Stop readin when reaching a comma, and
 * nesting depth is zero. Track nesting depth to allow things like
 * MAX( foo(a), b ).
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
    } while (nesting || (list->token != ',' && list->token != ')'));

    arg[n] = basic_token[END];
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

static int needs_expansion(const struct token *list)
{
    const struct macro *def;

    while (list->token != END) {
        def = definition(*list);
        if (def && !is_macro_expanded(def))
            return 1;
        list++;
    }

    return 0;
}

struct token *expand(struct token *original)
{
    const struct token *list;
    struct token *res;

    /* Do nothing if there is nothing to expand. */
    if (!needs_expansion(original))
        return original;

    /*printf("Expanding ");
    print_list(original);*/

    list = original;
    res = calloc(1, sizeof(*res));
    res[0] = basic_token[END];
    while (list->token != END) {
        const struct macro *def = definition(*list);
        int leading_whitespace = list->leading_whitespace;
        struct token *expn;
        struct token **args;

        /* Only expand function-like macros if they appear as function
         * invocations, beginning with an open paranthesis. */
        if (def && !is_macro_expanded(def) &&
            (def->type != FUNCTION_LIKE || peek_next(list + 1) == '('))
        {
            args = read_args(list + 1, &list, def);
            expn = expand_macro(def, args);

            /* Dirty fix for adding whitespace after expansion. Fill in
             * correct number of spaces from the expanded token. */
            expn->leading_whitespace = leading_whitespace;
            res = concat(res, expn);
        } else {
            res = append(res, *list++);
        }
    }

    /*printf("Result: ");
    print_list(res);*/

    free(original);
    return res;
}

int tok_cmp(struct token a, struct token b)
{
    return (a.token != b.token) || str_cmp(a.strval, b.strval);
}

/* From GCC documentation: All leading and trailing whitespace in text
 * being stringified is ignored. Any sequence of whitespace in the
 * middle of the text is converted to a single space in the stringified
 * result.
 */
struct token stringify(const struct token list[])
{
    int n = 0;
    size_t len = 0;
    struct token t = {STRING};
    char *str = calloc(1, sizeof(*str));

    while (list->token != END) {
        assert(list->token != NEWLINE);

        /* Reduce to a single space, and only insert between other
         * tokens in the list. */
        len += list->strval.len + (list->leading_whitespace && n);
        str = realloc(str, (len + 1) * sizeof(*str));
        if (n && list->leading_whitespace) {
            str[len - list->strval.len - 1] = ' ';
            str[len - list->strval.len] = '\0';
        }

        str = strncat(str, list->strval.str, len);
        list++;
        n++;
    }

    t.strval = str_register(str, len);
    free(str);
    return t;
}

static struct replacement *parse(char *str, int *out_size)
{
    char *endptr;
    int n = 0;
    struct replacement *repl = NULL;

    while (*str) {
        n++;
        repl = realloc(repl, sizeof(*repl) * n);
        memset(repl + n - 1, 0, sizeof(*repl));
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
        {IDENTIFIER},
        FUNCTION_LIKE,
        1, /* parameters */
    };

    macro.name.strval = str_init("__builtin_va_end");
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
        {IDENTIFIER},
        OBJECT_LIKE,
        0, /* parameters */
    };

    macro.name.strval = str_init("__STDC_VERSION__");
    macro.replacement = parse("199409L", &macro.size);
    define(macro);

    macro.name.strval = str_init("__STDC__");
    macro.replacement = parse("1", &macro.size);
    define(macro);

    macro.name.strval = str_init("__STDC_HOSTED__");
    macro.replacement = parse("1", &macro.size);
    define(macro);

    macro.name.strval = str_init("__LINE__");
    macro.replacement = parse("0", &macro.size);
    define(macro);

    macro.name.strval = str_init("__x86_64__");
    macro.replacement = parse("1", &macro.size);
    define(macro);

    /* For some reason this is not properly handled by musl. */
    macro.name.strval = str_init("__inline");
    macro.replacement = parse(" ", &macro.size);
    define(macro);

    macro.name.strval = str_init("__FILE__");
    macro.replacement = calloc(1, sizeof(*macro.replacement));
    macro.replacement[0].token.token = STRING;
    macro.replacement[0].token.strval = str_init(current_file.path);
    macro.replacement[0].token.intval = 0;
    define(macro);

    register__builtin_va_end();
}
