#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "input.h"
#include "macro.h"
#include "strtab.h"
#include "tokenize.h"
#include <lacc/context.h>
#include <lacc/hash.h>

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include <time.h>

static struct hash_table macro_hash_table;
static int new_macro_added;

typedef array_of(String) ExpandStack;

/* Keep track of arrays being recycled. */
static array_of(TokenArray) arrays;
static array_of(ExpandStack) stacks;

static int is_expanded(const ExpandStack *scope, String name)
{
    int i;
    for (i = 0; i < array_len(scope); ++i) {
        if (str_eq(array_get(scope, i), name)) {
            return 1;
        }
    }
    return 0;
}

INTERNAL TokenArray get_token_array(void)
{
    TokenArray list = {0};
    if (array_len(&arrays)) {
        list = array_pop_back(&arrays);
        array_zero(&list);
        array_empty(&list);
    }

    return list;
}

INTERNAL void release_token_array(TokenArray list)
{
    array_push_back(&arrays, list);
}

static ExpandStack get_expand_stack(void)
{
    ExpandStack stack = {0};
    if (array_len(&stacks)) {
        stack = array_pop_back(&stacks);
        array_zero(&stack);
        array_empty(&stack);
    }

    return stack;
}

static void release_expand_stack(ExpandStack stack)
{
    array_push_back(&stacks, stack);
}

static int macrocmp(const struct macro *a, const struct macro *b)
{
    int i;

    if ((a->type != b->type) || (a->params != b->params))
        return 1;

    if (!str_eq(a->name, b->name))
        return 1;

    if (array_len(&a->replacement) != array_len(&b->replacement))
        return 1;

    for (i = 0; i < array_len(&a->replacement); ++i) {
        if (tok_cmp(
                array_get(&a->replacement, i),
                array_get(&b->replacement, i)))
            return 1;
    }

    return 0;
}

static void macro_hash_del(void *ref)
{
    struct macro *macro = (struct macro *) ref;
    release_token_array(macro->replacement);
    free(macro);
}

static void *macro_hash_add(void *ref, String *key)
{
    struct macro *macro, *arg;

    arg = (struct macro *) ref;
    macro = calloc(1, sizeof(*macro));
    *macro = *arg;
    *key = macro->name;
    /*
     * Signal that the hash table has ownership now, and it will not be
     * freed in define().
     */
    new_macro_added = 1;
    return macro;
}

INTERNAL void macro_reset(void)
{
    hash_clear(&macro_hash_table, macro_hash_del);
}

INTERNAL void macro_finalize(void)
{
    int i;
    TokenArray list;
    ExpandStack stack;

    hash_destroy(&macro_hash_table);
    for (i = 0; i < array_len(&arrays); ++i) {
        list = array_get(&arrays, i);
        array_clear(&list);
    }

    for (i = 0; i < array_len(&stacks); ++i) {
        stack = array_get(&stacks, i);
        array_clear(&stack);
    }

    array_clear(&arrays);
    array_clear(&stacks);
}

static struct token get__line__token(void)
{
    int len;
    char buf[16];
    struct token t = basic_token[PREP_NUMBER];

    len = sprintf(buf, "%d", current_file_line);
    t.d.string = str_intern(buf, len);
    return t;
}

static struct token get__file__token(void)
{
    struct token t = {STRING};
    t.d.string = current_file_path;
    return t;
}

/*
 * Replace __FILE__ with file name, and __LINE__ with line number, by
 * mutating the replacement list on the fly.
 */
INTERNAL const struct macro *macro_definition(String name)
{
    struct macro *ref;

    ref = hash_lookup(&macro_hash_table, name);
    if (ref) {
        if (ref->is__file__) {
            array_get(&ref->replacement, 0) = get__file__token();
        } else if (ref->is__line__) {
            array_get(&ref->replacement, 0) = get__line__token();
        }
    }

    return ref;
}

INTERNAL void define(struct macro macro)
{
    struct macro *ref;
    static String
        builtin__file__ = SHORT_STRING_INIT("__FILE__"),
        builtin__line__ = SHORT_STRING_INIT("__LINE__");

    new_macro_added = 0;
    ref = hash_insert(&macro_hash_table, macro.name, &macro, macro_hash_add);
    if (macrocmp(ref, &macro)) {
        error("Redefinition of macro '%s' with different substitution.",
            str_raw(macro.name));
        exit(1);
    } else {
        ref->is__file__ = str_eq(builtin__file__, ref->name);
        ref->is__line__ = str_eq(builtin__line__, ref->name);
        if (!new_macro_added) {
            release_token_array(macro.replacement);
        }
    }
}

INTERNAL void undef(String name)
{
    hash_remove(&macro_hash_table, name, macro_hash_del);
}

#if !NDEBUG
void print_token_array(const TokenArray *list)
{
    int i;
    struct token t;

    putchar('[');
    for (i = 0; i < array_len(list); ++i) {
        if (i) {
            printf(", ");
        }
        t = array_get(list, i);
        if (t.token == PARAM) {
            printf("<param %ld>", t.d.val.i);
        } else {
            putchar('\'');
            if (t.leading_whitespace > 0) {
                printf("%*s", t.leading_whitespace, " ");
            }
            if (t.token == NEWLINE) {
                printf("\\n");
            } else {
                assert(t.token != NUMBER);
                printf("%s", str_raw(t.d.string));
            }
            putchar('\'');
        }
    }

    printf("] (%u)\n", array_len(list));
}
#endif

static struct token paste(struct token left, struct token right)
{
    char *buf;
    const char *endptr;
    String s1, s2;
    size_t l1, l2;

    assert(left.token != NUMBER);
    assert(right.token != NUMBER);

    s1 = left.d.string;
    s2 = right.d.string;
    l1 = str_len(s1);
    l2 = str_len(s2);

    buf = calloc(l1 + l2 + 1, sizeof(*buf));
    strncpy(buf, str_raw(s1), l1);
    strncpy(buf + l1, str_raw(s2), l2);

    right = tokenize(buf, &endptr);
    if (endptr != buf + l1 + l2) {
        error("Invalid token resulting from pasting '%s' and '%s'.",
            str_raw(s1), str_raw(s2));
        exit(1);
    }

    right.leading_whitespace = left.leading_whitespace;
    free(buf);
    return right;
}

static enum token_type peek_token(const TokenArray *list, int i)
{
    if (i < array_len(list)) {
        return array_get(list, i).token;
    }

    return END;
}

/*
 * Replace content of list in segment [start, start + gaplength] with
 * contents of slice. The gap is from reading arguments from list, and
 * the slice is result of expanding it. Slice might be smaller or larger
 * than the gap.
 */ 
static void array_replace_slice(
    TokenArray *list,
    int start,
    int gaplength,
    TokenArray *slice)
{
    int length;
    assert(start + gaplength <= array_len(list));

    length = array_len(list) - gaplength + array_len(slice);
    array_realloc(list, length);

    /*
     * Move trailing data out of the way, or move closer to prefix, to
     * align exactly where slice is inserted.
     */
    if (array_len(slice) != gaplength) {
        memmove(
            list->data + start + array_len(slice),
            list->data + start + gaplength,
            (array_len(list) - (start + gaplength)) * sizeof(*list->data));
    }

    /* Copy slice directly into now vacant space in list. */
    if (array_len(slice)) {
        memcpy(
            list->data + start,
            slice->data,
            array_len(slice) * sizeof(*list->data));
    }

    list->length = length;
}

/*
 * Replacing # <param> and <a> ## <b> is done in an initial scan of
 * the replacement list. This pass requires the parameters to not be
 * expanded.
 *
 * Special cases to consider for empty parameter substitution:
 *
 *    [] ## [] produces no tokens.
 *    [] ## ['foo'], and conversely, ['foo'] ## [], produces ['foo'].
 *    # [] produces an empty string.
 *
 * Also implement a GNU extension with special treatment of token paste
 * and comma: ', ## __VA_ARGS__' is expanded as if the paste was not
 * there if __VA_ARGS__ is non-empty. If it is empty, the whole sequence
 * is removed.
 *
 * Return an array which still can contain PARAM tokens that needs
 * further expansion.
 */
static TokenArray expand_stringify_and_paste(
    const struct macro *def,
    TokenArray *args)
{
    int len, d, i;
    struct token t, s;
    TokenArray list = get_token_array();

    len = array_len(&def->replacement);
    if (len && array_get(&def->replacement, 0).token == TOKEN_PASTE) {
        error("Unexpected '##' operator at beginning of line.");
        exit(1);
    } else if (len > 2) {
        if (array_get(&def->replacement, len - 1).token == TOKEN_PASTE) {
            error("Unexpected '##' operator at end of line.");
            exit(1);
        }
    }

    for (i = 0; i < len; ++i) {
        t = array_get(&def->replacement, i);
        switch (t.token) {
        case TOKEN_PASTE:
            i += 1;
            t = array_back(&list);
            s = array_get(&def->replacement, i);
            if (t.token == PARAM) {
                (void) array_pop_back(&list);
                if (!array_len(&args[t.d.val.i])) {
                    if (s.token == PARAM) {
                        array_concat(&list, &args[s.d.val.i]);
                    } else {
                        array_push_back(&list, s);
                    }
                    break;
                } else {
                    array_concat(&list, &args[t.d.val.i]);
                    t = array_back(&list);
                }
            }
            if (s.token == PARAM) {
                if (def->is_vararg
                    && t.token == ','
                    && s.d.val.i == def->params - 1)
                {
                    if (array_len(&args[s.d.val.i])) {
                        i--;
                    } else {
                        (void) array_pop_back(&list);
                    }
                } else if (array_len(&args[s.d.val.i])) {
                    t = array_pop_back(&list);
                    d = array_len(&list);
                    array_concat(&list, &args[s.d.val.i]);
                    s = array_get(&args[s.d.val.i], 0);
                    t = paste(t, s);
                    array_get(&list, d) = t;
                }
            } else {
                t = paste(t, s);
                array_back(&list) = t;
            }
            break;
        case '#':
            i += 1;
            if (peek_token(&def->replacement, i) == PARAM) {
                d = array_get(&def->replacement, i).d.val.i;
                t = stringify(&args[d]);
                array_push_back(&list, t);
            } else {
                error("Stray '#' in replacement list.");
                exit(1);
            }
            break;
        default:
            array_push_back(&list, t);
            break;
        }
    }

    return list;
}

static int expand_line(ExpandStack *scope, TokenArray *list);

static TokenArray expand_macro(
    ExpandStack *scope,
    const struct macro *def,
    TokenArray *args)
{
    int i, j;
    struct token t;
    TokenArray list;

    list = expand_stringify_and_paste(def, args);
    if (def->params > 0) {
        assert(def->type == FUNCTION_LIKE);
        for (i = 0; i < def->params; ++i) {
            expand(&args[i]);
            if (array_len(&args[i])) {
                if (!array_get(&args[i], 0).leading_whitespace) {
                    array_get(&args[i], 0).leading_whitespace = 1;
                }
            }
        }

        for (i = 0; i < array_len(&list); ++i) {
            t = array_get(&list, i);
            if (t.token == PARAM) {
                j = t.d.val.i;
                if (!array_len(&args[j])) {
                    array_erase(&list, i);
                    i--;
                } else {
                    array_replace_slice(&list, i, 1, &args[j]);
                }
            }
        }

        for (i = 0; i < def->params; ++i)
            release_token_array(args[i]);
        free(args);
    }

    expand_line(scope, &list);
    return list;
}

/*
 * Read tokens forming next macro argument. Missing arguments are
 * represented by an empty list.
 *
 * Stop reading on first ',' encountered with no parenthesis nesting
 * depth. Exception is argument for (...), which consumes input until
 * first ')'.
 */
static TokenArray read_arg(
    ExpandStack *scope,
    int is_va_arg,
    const struct token *list,
    const struct token **endptr)
{
    int nesting = 0;
    struct token t;
    TokenArray arg = get_token_array();

    while (nesting
        || ((list->token != ',' || is_va_arg) && list->token != ')'))
    {
        if (list->token == NEWLINE) {
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
        t = *list++;
        if (t.is_expandable && is_expanded(scope, t.d.string)) {
            t.disable_expand = 1;
        }
        array_push_back(&arg, t);
    }

    *endptr = list;
    return arg;
}

static TokenArray *read_args(
    ExpandStack *scope,
    const struct macro *def,
    const struct token *list,
    const struct token **endptr)
{
    int i;
    TokenArray *args = NULL;

    if (def->type == FUNCTION_LIKE) {
        if (list->token != '(') {
            error("Expected '(' to begin macro argument list.");
            exit(1);
        }

        list += 1;
        if (def->params) {
            args = calloc(def->params, sizeof(*args));
            for (i = 0; i < def->params - def->is_vararg; ++i) {
                args[i] = read_arg(scope, 0, list, &list);
                if (list->token != ',') {
                    if (i == def->params - 1)
                        break;
                    if (def->is_vararg && i == def->params - 2) {
                        i = -1;
                        break;
                    } else {
                        error("Expected ',' between macro parameters.");
                        exit(1);
                    }
                } else list += 1;
            }

            /* Last parameter can be optional for vararg macros. */
            if (def->is_vararg && i != -1) {
                assert(i == def->params - 1);
                args[i] = read_arg(scope, 1, list, &list);
            }
        }

        if (list->token != ')') {
            error("Expected ')' to close macro argument list.");
            exit(1);
        }

        list += 1;
    }

    *endptr = list;
    return args;
}

static int expand_line(ExpandStack *scope, TokenArray *list)
{
    int size, i, n;
    struct token t;
    const struct macro *def;
    const struct token *endptr;
    TokenArray *args, expn;

    for (n = 0, i = 0; i < array_len(list); ++i) {
        t = array_get(list, i);
        if (!t.is_expandable || t.disable_expand) {
            continue;
        }

        def = macro_definition(t.d.string);
        if (!def)
            continue;

        if (is_expanded(scope, t.d.string)) {
            array_get(list, i).disable_expand = 1;
            continue;
        }

        /* Only expand if next token is '(' */
        if (def->type == FUNCTION_LIKE
            && (i == array_len(list) - 1
                || array_get(list, i + 1).token != '('))
        {
            continue;
        }

        args = read_args(scope, def, list->data + i + 1, &endptr);
        array_push_back(scope, def->name);
        expn = expand_macro(scope, def, args);
        size = (endptr - list->data) - i;
        (void) array_pop_back(scope);

        /* Fix leading whitespace after expansion. */
        if (array_len(&expn)) {
            expn.data[0].leading_whitespace = t.leading_whitespace;
        }

        /* Squeeze in expansion in list. */
        array_replace_slice(list, i, size, &expn);
        i += array_len(&expn) - 1;
        release_token_array(expn);
        n += 1;
    }

    return n;
}

INTERNAL int expand(TokenArray *list)
{
    int n;
    ExpandStack stack = get_expand_stack();

    n = expand_line(&stack, list);
    release_expand_stack(stack);
    return n;
}

INTERNAL int tok_cmp(struct token a, struct token b)
{
    if (a.token != b.token)
        return 1;

    if (a.token == PARAM || a.token == NUMBER) {
        return !type_equal(a.type, b.type) || a.d.val.u != b.d.val.u;
    }

    return !str_eq(a.d.string, b.d.string);
}

static char *str_write_escaped(char *ptr, const char *str, size_t len)
{
    int i;

    for (i = 0; i < len; ++i) {
        if (str[i] == '"' || str[i] == '\\') {
            *ptr++ = '\\';
        }

        *ptr++ = str[i];
    }

    return ptr;
}

static char *stringify_concat(
    char *buf,
    size_t *cap,
    size_t *pos,
    struct token tok)
{
    size_t len, max;
    const char *raw;
    char *ptr;
    String str;

    assert(tok.token != NUMBER);
    assert(tok.token != PARAM);
    str = tok.d.string;
    len = str_len(str);
    max = (tok.leading_whitespace != 0) + len * 2 + 4;
    if (*cap < *pos + max) {
        *cap = *pos + max;
        buf = realloc(buf, *cap);
    }

    ptr = buf + *pos;
    if (tok.leading_whitespace != 0) {
        *ptr++ = ' ';
    }

    raw = str_raw(str);
    switch (tok.token) {
    case PREP_STRING:
    case STRING:
        *ptr++ = '\\';
        *ptr++ = '"';
        ptr = str_write_escaped(ptr, raw, len);
        *ptr++ = '\\';
        *ptr++ = '"';
        break;
    case PREP_CHAR:
        *ptr++ = '\'';
        ptr = str_write_escaped(ptr, raw, len);
        *ptr++ = '\'';
        break;
    default:
        memcpy(ptr, raw, len);
        ptr += len;
        break;
    }

    *pos += ptr - (buf + *pos);
    assert(*cap >= *pos);
    return buf;
}

/*
 * Convert list of tokens to a single PREP_STRING or STRING token.
 *
 * - All leading and trailing whitespace in text being stringified is
 *   ignored.
 * - Any sequence of whitespace in the middle of the text is converted
 *   to a single space in the stringified result.
 */
INTERNAL struct token stringify(const TokenArray *list)
{
    struct token str = {0}, t;
    size_t cap, len;
    char *buf;
    int i;

    if (!array_len(list)) {
        str.token = STRING;
        str.d.string = str_empty();
    } else {
        t = array_get(list, 0);
        switch (t.token) {
        default:
            assert(t.token != END);
            assert(t.token != NUMBER);
            if (array_len(list) == 1) {
                str.token = STRING;
                str.d.string = t.d.string;
                str.leading_whitespace = t.leading_whitespace;
                break;
            }
        case '\\':
        case STRING:
        case PREP_STRING:
        case PREP_CHAR:
            str.token = PREP_STRING;
            cap = array_len(list) * 8;
            buf = malloc(cap);
            len = 0;
            for (i = 0; i < array_len(list); ++i) {
                t = array_get(list, i);
                if (t.token == NEWLINE) {
                    assert(i == array_len(list) - 1);
                } else {
                    assert(t.token != END);
                    if (i == 0) {
                        t.leading_whitespace = 0;
                    }
                    buf = stringify_concat(buf, &cap, &len, t);
                }
            }
            if (len > 0 && buf[len - 1] == '\\') {
                error("Invalid string literal ending with '\\'.");
                exit(1);
            }
            str.leading_whitespace = array_get(list, 0).leading_whitespace;
            str.d.string = str_intern(buf, len);
            free(buf);
            break;
        }
    }

    return str;
}

static TokenArray parse_macro_replacement(const char *str)
{
    const char *endptr;
    TokenArray arr = get_token_array();

    while (*str) {
        array_push_back(&arr, tokenize(str, &endptr));
        assert(str != endptr);
        str = endptr;
    }

    return arr;
}

static void register_macro(const char *key, const char *value)
{
    struct macro macro = {0};

    macro.type = OBJECT_LIKE;
    macro.name = str_c(key);
    macro.replacement = parse_macro_replacement(value);
    define(macro);
}

static char *get__time__(char *ts)
{
    static char str[11];

    assert(strlen(ts) == 25);
    str[0] = '"';
    memcpy(&str[1], ts + 11, 8);
    str[9] = '"';
    assert(str[10] == '\0');
    return str;
}

static char *get__date__(char *ts)
{
    static char str[14];

    assert(strlen(ts) == 25);
    str[0] = '"';
    memcpy(&str[1], ts + 4, 7);
    memcpy(&str[8], ts + 20, 4);
    str[12] = '"';
    assert(str[13] == '\0');
    return str;
}

/*
 * Current date and time are taken from ctime output, which has format
 * like "Sun Feb 19 01:26:43 2017\n". In this case, __DATE__ will be
 * "Feb 19 2017", and __TIME__ is "01:26:43".
 */
INTERNAL void register_builtin_definitions(enum cstd version)
{
    time_t timestamp = time(NULL);
    char *ts = ctime(&timestamp);

    register_macro("__STDC__", "1");
    register_macro("__STDC_HOSTED__", "1");
    register_macro("__FILE__", "0");
    register_macro("__LINE__", "0");
    register_macro("__DATE__", get__date__(ts));
    register_macro("__TIME__", get__time__(ts));
    register_macro("__x86_64__", "1");
    register_macro("__LP64__", "1");
    register_macro("__SIZE_TYPE__", "unsigned long");
    register_macro("__WCHAR_TYPE__", "signed int");
    register_macro("__PTRDIFF_TYPE__", "signed long");
    register_macro("__CHAR_BIT__", "8");
    register_macro("__SIZEOF_LONG__", "8");
    register_macro("__SIZEOF_POINTER__", "8");
    register_macro("__lacc__", "");

#ifdef LINUX
    register_macro("__linux__", "1");
#endif
#ifdef UNIX
    register_macro("__unix__", "1");
#endif
#ifdef OpenBSD
    register_macro("__OpenBSD__", "1");
    register_macro("_ANSI_LIBRARY", "1");
    if (version == STD_C89) {
        register_macro("__ISO_C_VISIBLE", "1990");
    }
#endif

    switch (version) {
    case STD_C89:
        break;
    case STD_C99:
        register_macro("__STDC_VERSION__", "199901L");
        break;
    case STD_C11:
        register_macro("__STDC_VERSION__", "201112L");
        break;
    }
}
