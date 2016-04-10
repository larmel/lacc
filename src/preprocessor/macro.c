#include "input.h"
#include "macro.h"
#include "strtab.h"
#include "tokenize.h"
#include <lacc/cli.h>
#include <lacc/hash.h>
#include <lacc/list.h>

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stddef.h>
#include <stdlib.h>

#define HASH_TABLE_BUCKETS 1024

static struct hash_table macro_hash_table;
static int new_macro_added;

static int macrocmp(const struct macro *a, const struct macro *b)
{
    int i;

    if ((a->type != b->type) || (a->params != b->params))
        return 1;

    if (tok_cmp(a->name, b->name))
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

static struct string macro_hash_key(void *ref)
{
    return ((struct macro *) ref)->name.d.string;
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

const struct macro *definition(struct token name)
{
    struct macro *ref = NULL;
    struct token *tok;

    ensure_initialized();
    if (name.token == IDENTIFIER) {
        ref = hash_lookup(&macro_hash_table, name.d.string);
        if (ref) {
            /* Replace __LINE__ with current line number, by mutating
             * the replacement list on the fly. */
            if (!str_cmp(ref->name.d.string, str_init("__LINE__"))) {
                tok = &array_get(&ref->replacement, 0);
                tok->d.number.val.i = current_file_line();
            }
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
            macro.name.d.string.str);
        exit(1);
    }

    /* Need to clean up memory for replacement list since ownership was
     * not given to hash table. */
    if (!new_macro_added) {
        array_clear(&macro.replacement);
    }
}

void undef(struct token name)
{
    ensure_initialized();
    if (name.token == IDENTIFIER) {
        hash_remove(&macro_hash_table, name.d.string);
    }
}

/* Keep track of which macros have been expanded, avoiding recursion by
 * looking up in this list for each new expansion.
 */
static struct list expand_stack;

static int is_macro_expanded(const struct macro *macro)
{
    int i;
    const struct macro *other;

    for (i = 0; i < list_len(&expand_stack); ++i) {
        other = (const struct macro *) list_get(&expand_stack, i);
        if (!tok_cmp(other->name, macro->name))
            return 1;
    }

    return 0;
}

static void push_expand_stack(const struct macro *macro)
{
    assert(!is_macro_expanded(macro));
    list_push(&expand_stack, (void *) macro);
}

static void pop_expand_stack(void)
{
    assert(list_len(&expand_stack));
    list_pop(&expand_stack);
    if (!list_len(&expand_stack)) {
        list_clear(&expand_stack, NULL);
    }
}

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
            printf("<param %ld>", t.d.number.val.i);
        } else {
            putchar('\'');
            if (t.leading_whitespace > 0) {
                printf("%*s", t.leading_whitespace, " ");
            }
            if (t.token == NEWLINE) {
                printf("\\n");
            } else {
                printf("%s", tokstr(t).str);
            }
            putchar('\'');
        }
    }

    printf("] (%u)\n", array_len(list));
}

static struct token paste(struct token left, struct token right)
{
    struct token result;
    size_t length;
    char *data, *endptr;

    length = left.d.string.len + right.d.string.len;
    data   = calloc(length + 1, sizeof(*data));
    data   = strcpy(data, left.d.string.str);
    data   = strcat(data, right.d.string.str);
    result = tokenize(data, &endptr);
    if (endptr != data + length) {
        error("Invalid token resulting from pasting '%s' and '%s'.",
            left.d.string.str, right.d.string.str);
        exit(1);
    }

    result.leading_whitespace = left.leading_whitespace;
    free(data);
    return result;
}

static void expand_paste_operators(TokenArray *list)
{
    unsigned i, j, len;
    struct token t;

    len = array_len(list);
    if (len) {
        if (array_get(list, 0).token == TOKEN_PASTE) {
            error("Unexpected token paste operator at beginning of line.");
            exit(1);
        } else if (len > 2) {
            if (array_get(list, len - 1).token == TOKEN_PASTE) {
                error("Unexpected token paste operator at end of line.");
                exit(1);
            }

            /* In-place expansion of token paste operators.
             * ['f', '##', 'u', '##', 'nction'] becomes ['function']. */
            for (i = 0, j = 1; j < len; ++j) {
                assert(i < len);
                t = array_get(list, j);
                if (t.token == TOKEN_PASTE) {
                    array_get(list, i) = paste(
                        array_get(list, i), array_get(list, j + 1));
                    j++;
                } else if (i < j - 1) {
                    array_get(list, i) = array_get(list, j);
                    i++;
                } else {
                    i++;
                }
            }

            list->length = i + 1;
        }
    }
}

static TokenArray expand_macro(const struct macro *def, TokenArray *args)
{
    int i, param;
    struct token t;
    struct token *stringified = NULL;
    TokenArray list = {0};

    push_expand_stack(def);
    if (def->params) {
        stringified = calloc(def->params, sizeof(*stringified));
        for (i = 0; i < def->params; ++i) {
            stringified[i] = stringify(&args[i]);
            expand(&args[i]);
        }
    }

    for (i = 0; i < array_len(&def->replacement); ++i) {
        t = array_get(&def->replacement, i);
        if (t.token == PARAM) {
            param = t.d.number.val.i;
            assert(param < def->params);
            array_concat(&list, &args[param]);
        } else if (t.token == '#' &&
            i < array_len(&def->replacement) - 1 &&
            array_get(&def->replacement, i + 1).token == PARAM)
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
    pop_expand_stack();
    for (i = 0; i < def->params; ++i) {
        array_clear(&args[i]);
    }

    free(args);
    free(stringified);
    return list;
}

static const struct token *skip(const struct token *list, enum token_type token)
{
    if (list->token != token) {
        assert(basic_token[token].d.string.str);
        error("Expected '%s', but got '%s'.",
            basic_token[token].d.string.str, list->d.string.str);
    }

    list++;
    return list;
}

static TokenArray read_arg(
    const struct token *list,
    const struct token **endptr)
{
    int nesting = 0;
    TokenArray arg = {0};

    do {
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
    } while (nesting || (list->token != ',' && list->token != ')'));

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
    unsigned i, size;
    const struct macro *def;
    const struct token *endptr;
    TokenArray *args, expn;

    for (i = 0; i < array_len(list); ++i) {
        t = array_get(list, i);
        if (t.token == IDENTIFIER) {
            def = definition(t);

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
                array_clear(&expn);
                i += size;
            }
        }
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
    struct token t;
    struct string strval;
    char *buf;
    size_t cap, len, ptr;

    /* Estimate 7 characters per token, trying to avoid unnecessary
     * reallocations. */
    cap = array_len(list) * 7 + 1;
    buf = malloc(cap);
    len = ptr = 0;
    buf[0] = '\0';

    for (i = 0; i < array_len(list); ++i) {
        t = array_get(list, i);
        assert(t.token != END);

        /* Do not include trailing space of line. This case hits when
         * producing message for #error directives. */
        if (t.token == NEWLINE) {
            assert(i == array_len(list) - 1);
            break;
        }

        /* Reduce to a single space, and only insert between other
         * tokens in the list. */
        strval = tokstr(t);
        len += strval.len + (t.leading_whitespace && i);
        if (len >= cap) {
            cap = len + array_len(list) + 1;
            buf = realloc(buf, cap);
        }
        if (t.leading_whitespace && i) {
            buf[ptr++] = ' ';
        }
        memcpy(buf + ptr, strval.str, strval.len);
        ptr += strval.len;
    }

    t.token = STRING;
    t.d.string = str_register(buf, len);
    free(buf);
    return t;
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

static void register__builtin_va_end(void)
{
    struct macro macro = {
        {IDENTIFIER},
        FUNCTION_LIKE,
        1, /* parameters */
    };

    macro.name.d.string = str_init("__builtin_va_end");
    macro.replacement = parse(
        "@[0].gp_offset=0;"
        "@[0].fp_offset=0;"
        "@[0].overflow_arg_area=(void*)0;"
        "@[0].reg_save_area=(void*)0;");

    assert(array_len(&macro.replacement) == 44);
    define(macro);
}

static void register__builtin__FILE__(void)
{
    struct token file = {STRING};
    struct macro macro = {
        {IDENTIFIER},
        OBJECT_LIKE,
        0, /* parameters */
    };

    file.d.string = str_init(current_file_path());
    array_push_back(&macro.replacement, file);

    macro.name.d.string = str_init("__FILE__");
    define(macro);
}

void register_builtin_definitions(void)
{
    struct macro macro = {
        {IDENTIFIER},
        OBJECT_LIKE,
        0, /* parameters */
    };

    macro.name.d.string = str_init("__STDC_VERSION__");
    macro.replacement = parse("199409L");
    define(macro);

    macro.name.d.string = str_init("__STDC__");
    macro.replacement = parse("1");
    define(macro);

    macro.name.d.string = str_init("__STDC_HOSTED__");
    macro.replacement = parse("1");
    define(macro);

    macro.name.d.string = str_init("__LINE__");
    macro.replacement = parse("0");
    define(macro);

    macro.name.d.string = str_init("__x86_64__");
    macro.replacement = parse("1");
    define(macro);

    /* For some reason this is not properly handled by musl. */
    macro.name.d.string = str_init("__inline");
    macro.replacement = parse("");
    define(macro);

    register__builtin__FILE__();
    register__builtin_va_end();
}
