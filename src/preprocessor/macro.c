#define _XOPEN_SOURCE 500
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

#define HASH_TABLE_BUCKETS 1024

static struct hash_table macro_hash_table;
static int new_macro_added;

/* Keep track of which macros have been expanded, avoiding recursion by
 * looking up in this list for each new expansion.
 */
static array_of(String) expand_stack;

static int is_macro_expanded(const struct macro *macro)
{
    int i;
    String name;

    for (i = 0; i < array_len(&expand_stack); ++i) {
        name = array_get(&expand_stack, i);
        if (!str_cmp(name, macro->name)) {
            return 1;
        }
    }

    return 0;
}

static int macrocmp(const struct macro *a, const struct macro *b)
{
    int i;

    if ((a->type != b->type) || (a->params != b->params))
        return 1;

    if (str_cmp(a->name, b->name))
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

static String macro_hash_key(void *ref)
{
    return ((struct macro *) ref)->name;
}

static void macro_hash_del(void *ref)
{
    struct macro *macro = (struct macro *) ref;
    array_clear(&macro->replacement);
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
    array_clear(&expand_stack);
    hash_destroy(&macro_hash_table);
}

static void ensure_initialized(void)
{
    static int done;

    if (!done) {
        hash_init(
            &macro_hash_table,
            HASH_TABLE_BUCKETS,
            macro_hash_key,
            macro_hash_add,
            macro_hash_del);
        atexit(cleanup);
        done = 1;
    }
}

static struct token get__line__token(void)
{
    int len;
    char buf[32];
    struct token t = basic_token[PREP_NUMBER];

    len = snprintf(buf, sizeof(buf), "%d", current_file_line);
    t.d.string = str_register(buf, len);
    return t;
}

const struct macro *definition(String name)
{
    String builtin__line__ = SHORT_STRING_INIT("__LINE__");
    struct macro *ref;

    ensure_initialized();
    ref = hash_lookup(&macro_hash_table, name);
    if (ref) {
        /* Replace __LINE__ with current line number, by mutating
         * the replacement list on the fly. */
        if (!str_cmp(ref->name, builtin__line__)) {
            array_get(&ref->replacement, 0) = get__line__token();
        }
    }

    return ref;
}

void define(struct macro macro)
{
    struct macro *ref;

    ensure_initialized();
    new_macro_added = 0;
    ref = hash_insert(&macro_hash_table, &macro);
    if (macrocmp(ref, &macro)) {
        error("Redefinition of macro '%s' with different substitution.",
            str_raw(macro.name));
        exit(1);
    }

    /* Need to clean up memory for replacement list since ownership was
     * not given to hash table. */
    if (!new_macro_added) {
        array_clear(&macro.replacement);
    }
}

void undef(String name)
{
    ensure_initialized();
    hash_remove(&macro_hash_table, name);
}

void print_token_array(const TokenArray *list)
{
    int i;
    String s;
    struct token t;

    putchar('[');
    for (i = 0; i < array_len(list); ++i) {
        if (i) {
            printf(", ");
        }
        t = array_get(list, i);
        if (t.token == PARAM) {
            printf("<param %ld>", t.d.number.val.i);
        } else if (t.token == EMPTY_ARG) {
            printf("<no-arg>");
        } else {
            putchar('\'');
            if (t.leading_whitespace > 0) {
                printf("%*s", t.leading_whitespace, " ");
            }
            if (t.token == NEWLINE) {
                printf("\\n");
            } else {
                s = tokstr(t);
                printf("%s", str_raw(s));
            }
            putchar('\'');
        }
    }

    printf("] (%u)\n", array_len(list));
}

static struct token paste(struct token left, struct token right)
{
    struct token res;
    char *buf, *endptr;
    String ls, rs;

    assert(left.token != EMPTY_ARG || right.token != EMPTY_ARG);
    if (left.token == EMPTY_ARG) {
        return right;
    } else if (right.token == EMPTY_ARG) {
        return left;
    }

    ls = tokstr(left);
    rs = tokstr(right);
    buf = calloc(ls.len + rs.len + 1, sizeof(*buf));
    buf = strcpy(buf, str_raw(ls));
    buf = strcat(buf, str_raw(rs));
    res = tokenize(buf, &endptr);
    if (endptr != buf + ls.len + rs.len) {
        error("Invalid token resulting from pasting '%s' and '%s'.",
            str_raw(ls), str_raw(rs));
        exit(1);
    }

    res.leading_whitespace = left.leading_whitespace;
    free(buf);
    return res;
}

/* In-place expansion of token paste operators.
 *
 * Example:
 *    ['f', '##', 'u', '##', 'nction'] -> ['function'].
 */
static void expand_paste_operators(TokenArray *list)
{
    unsigned i, j, len;
    struct token t, l, r;

    len = array_len(list);
    if (len && array_get(list, 0).token == TOKEN_PASTE) {
        error("Unexpected token paste operator at beginning of line.");
        exit(1);
    } else if (len > 2) {
        if (array_get(list, len - 1).token == TOKEN_PASTE) {
            error("Unexpected token paste operator at end of line.");
            exit(1);
        }

        for (i = 0, j = 1; j < len; ++j) {
            assert(i < len);
            t = array_get(list, j);
            if (t.token == TOKEN_PASTE) {
                l = array_get(list, i);
                r = array_get(list, j + 1);
                if (l.token == EMPTY_ARG && r.token == EMPTY_ARG) {
                    /* Pasting together two arguments that are not given
                     * will result in no token. */
                    i--;
                } else {
                    array_get(list, i) = paste(l, r);
                }
                j++;
            } else if (t.token != EMPTY_ARG) {
                if (i < j - 1) {
                    i++;
                    array_get(list, i) = array_get(list, j);
                } else {
                    i++;
                }
            }
        }

        list->length = i + 1;
    }
}

static TokenArray expand_macro(const struct macro *def, TokenArray *args)
{
    int i, param;
    struct token t;
    struct token *stringified = NULL;
    TokenArray list = {0};

    array_push_back(&expand_stack, def->name);
    if (def->params) {
        stringified = calloc(def->params, sizeof(*stringified));
        for (i = 0; i < def->params; ++i) {
            stringified[i] = stringify(&args[i]);
            expand(&args[i]);
            if (!args[i].data[0].leading_whitespace) {
                args[i].data[0].leading_whitespace = 1;
            }
        }
    }

    for (i = 0; i < array_len(&def->replacement); ++i) {
        t = array_get(&def->replacement, i);
        if (t.token == PARAM) {
            param = t.d.number.val.i;
            assert(param < def->params);
            array_concat(&list, &args[param]);
        } else if (t.token == '#'
            && i < array_len(&def->replacement) - 1
            && array_get(&def->replacement, i + 1).token == PARAM)
        {
            i++;
            param = array_get(&def->replacement, i).d.number.val.i;
            array_push_back(&list, stringified[param]);
        } else {
            array_push_back(&list, t);
        }
    }

    expand_paste_operators(&list);
    expand(&list);
    (void) array_pop_back(&expand_stack);
    for (i = 0; i < def->params; ++i) {
        array_clear(&args[i]);
    }

    free(args);
    free(stringified);
    return list;
}

static const struct token *skip(const struct token *list, enum token_type token)
{
    String a, b;
    if (list->token != token) {
        a = tokstr(basic_token[token]);
        b = tokstr(*list);
        error("Expected '%s', but got '%s'.", str_raw(a), str_raw(b));
        exit(1);
    }

    list++;
    return list;
}

/* Read tokens forming next macro argument. Missing arguments are
 * represented by a single EMPTY_ARG element.
 */
static TokenArray read_arg(
    const struct token *list,
    const struct token **endptr)
{
    int nesting = 0;
    TokenArray arg = {0};

    while (nesting || (list->token != ',' && list->token != ')')) {
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

        array_push_back(&arg, *list++);
    }

    if (!array_len(&arg)) {
        array_push_back(&arg, basic_token[EMPTY_ARG]);
    }

    *endptr = list;
    return arg;
}

static TokenArray *read_args(
    const struct macro *def,
    const struct token *list,
    const struct token **endptr)
{
    int i;
    TokenArray *args = NULL;

    if (def->type == FUNCTION_LIKE) {
        args = malloc(def->params * sizeof(*args));
        list = skip(list, '(');
        for (i = 0; i < def->params; ++i) {
            args[i] = read_arg(list, &list);
            if (i < def->params - 1) {
                list = skip(list, ',');
            }
        }
        list = skip(list, ')');
    }

    *endptr = list;
    return args;
}

/* Replace content of list between indices [start, end] with contents of
 * slice.
 */ 
static void array_replace_slice(
    TokenArray *list,
    unsigned start,
    unsigned size,
    TokenArray *slice)
{
    unsigned length, end;
    int offset;
    assert(size <= array_len(list));

    end = start + size;
    offset = array_len(slice) - size;
    length = array_len(list) - size + array_len(slice);

    if (length > list->capacity) {
        list->capacity = length;
        list->data = realloc(list->data, list->capacity * sizeof(*list->data));
    }

    /* Move trailing data out of the way, or move closer to prefix, to
     * align exactly where slice is inserted. */
    if (offset != 0) {
        memmove(
            list->data + end + offset,
            list->data + end,
            (array_len(list) - end) * sizeof(*list->data));
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

void expand(TokenArray *list)
{
    struct token t;
    unsigned i = 0, size;
    const struct macro *def;
    const struct token *endptr;
    TokenArray *args, expn;

    while (i < array_len(list)) {
        t = array_get(list, i);
        if (t.token == IDENTIFIER) {
            def = definition(tokstr(t));

            /* Only expand function-like macros if they appear as func-
             * tion invocations, beginning with an open paranthesis. */
            if (def && !is_macro_expanded(def) &&
                (def->type != FUNCTION_LIKE ||
                    array_get(list, i + 1).token == '('))
            {
                args = read_args(def, list->data + i + 1, &endptr);
                expn = expand_macro(def, args);
                size = (endptr - list->data) - i;

                /* Fix leading whitespace after expansion. */
                if (array_len(&expn)) {
                    expn.data[0].leading_whitespace = t.leading_whitespace;
                }

                /* Squeeze in expn in list, starting from index i and
                 * extending size elements. */
                array_replace_slice(list, i, size, &expn);
                i += array_len(&expn);
                array_clear(&expn);
                continue;
            }
        }
        i++;
    }
}

int tok_cmp(struct token a, struct token b)
{
    if (a.token != b.token)
        return 1;

    if (a.token == PARAM) {
        return a.d.number.val.i != b.d.number.val.i;
    } else if (a.token == NUMBER) {
        if (!type_equal(a.d.number.type, b.d.number.type))
            return 1;
        return
            (a.d.number.type->type == T_UNSIGNED) ?
                a.d.number.val.u != b.d.number.val.u :
                a.d.number.val.i != b.d.number.val.i;
    } else {
        return str_cmp(a.d.string, b.d.string);
    }
}

/* From GCC documentation: All leading and trailing whitespace in text
 * being stringified is ignored. Any sequence of whitespace in the
 * middle of the text is converted to a single space in the stringified
 * result.
 */
struct token stringify(const TokenArray *list)
{
    int i;
    struct token str, tok;
    String strval;
    char *buf;
    size_t cap, len, ptr;

    if (array_len(list) == 0 || array_get(list, 0).token == EMPTY_ARG) {
        str.d.string = str_init("");
    } else if (array_len(list) == 1) {
        tok = array_get(list, 0);
        str.d.string = tokstr(tok);
        if (tok.token == NUMBER) {
            str.d.string =
                str_register(str_raw(str.d.string), str.d.string.len);
        }
    } else {
        /* Estimate 7 characters per token, trying to avoid unnecessary
         * reallocations. */
        cap = array_len(list) * 7 + 1;
        buf = malloc(cap);
        len = ptr = 0;
        buf[0] = '\0';

        for (i = 0; i < array_len(list); ++i) {
            tok = array_get(list, i);
            assert(tok.token != END);

            /* Do not include trailing space of line. This case hits
             * when producing message for #error directives. */
            if (tok.token == NEWLINE) {
                assert(i == array_len(list) - 1);
                break;
            }

            /* Reduce to a single space, and only insert between other
             * tokens in the list. */
            strval = tokstr(tok);
            len += strval.len + (tok.leading_whitespace && i);
            if (len >= cap) {
                cap = len + array_len(list) + 1;
                buf = realloc(buf, cap);
            }
            if (tok.leading_whitespace && i) {
                buf[ptr++] = ' ';
            }
            memcpy(buf + ptr, str_raw(strval), strval.len);
            ptr += strval.len;
        }

        str.d.string = str_register(buf, len);
        free(buf);
    }

    str.token = STRING;
    str.leading_whitespace = 0;
    return str;
}

static TokenArray parse(char *str)
{
    char *endptr;
    struct token param = {PARAM};
    TokenArray arr = {0};

    while (*str) {
        if (*str == '@') {
            array_push_back(&arr, param);
            str++;
        } else {
            array_push_back(&arr, tokenize(str, &endptr));
            assert(str != endptr);
            str = endptr;
        }
    }

    return arr;
}

static void register__builtin__FILE__(void)
{
    struct token file = {STRING};
    struct macro macro = {
        SHORT_STRING_INIT("__FILE__"),
        OBJECT_LIKE,
        0, /* parameters */
    };

    file.d.string = current_file_path;
    array_push_back(&macro.replacement, file);
    define(macro);
}

static void register_macro(const char *key, char *value)
{
    struct macro macro = {{{0}}, OBJECT_LIKE};

    macro.name = str_init(key);
    macro.replacement = parse(value);
    define(macro);
}

void register_builtin_definitions(void)
{
    register_macro("__STDC__", "1");
    register_macro("__STDC_HOSTED__", "1");
    register_macro("__LINE__", "0");
    register_macro("__x86_64__", "1");
    register_macro("__inline", "");

    switch (context.standard) {
    case STD_C89:
        register_macro("__STDC_VERSION__", "199409L");
        register_macro("__STRICT_ANSI__", "");
        break;
    case STD_C99:
        register_macro("__STDC_VERSION__", "199901L");
        break;
    }

    register__builtin__FILE__();
}
