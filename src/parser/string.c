#if _XOPEN_SOURCE < 500
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 700 /* strndup, snprintf */
#endif
#include "eval.h"
#include <lacc/string.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define HASH_TABLE_LENGTH 1024

struct string
{
    size_t length;
    char *string;
    char *label;

    struct hash {
        unsigned long val;
        struct string *next;
    } hash;
};

static struct string
    str_hash_tab[HASH_TABLE_LENGTH];

static void hash_node_cleanup(struct string *ref)
{
    if (ref->hash.next)
        hash_node_cleanup(ref->hash.next);

    if (ref->label)
        free(ref->label);
    free(ref->string);
    free(ref);
}

static void cleanup(void)
{
    int i;
    struct string *ref;

    for (i = 0; i < HASH_TABLE_LENGTH; ++i) {
        ref = &str_hash_tab[i];
        if (ref->hash.next)
            hash_node_cleanup(ref->hash.next);
        if (ref->label)
            free(ref->label);
        if (ref->string)
            free(ref->string);
    }
}

static struct string *hash_insert(const char *str, size_t len)
{
    static int reg_cleanup;
    struct string *ref;
    unsigned long
        hash = djb2_hash_p(str, str + len),
        pos = hash % HASH_TABLE_LENGTH;

    if (!reg_cleanup) {
        atexit(cleanup);
        reg_cleanup = 1;
    }

    ref = &str_hash_tab[pos];
    if (!ref->string) {
        ref->length = len;
        ref->string = strndup(str, len);
        ref->hash.val = hash;
        return ref;
    }

    while ((ref->hash.val != hash || strncmp(ref->string, str, len)) &&
            ref->hash.next)
        ref = ref->hash.next;

    if (ref->hash.val == hash && !strncmp(ref->string, str, len)) {
        return ref;
    }

    assert(!ref->hash.next);
    ref->hash.next = calloc(1, sizeof(*ref));
    ref = ref->hash.next;

    ref->length = len;
    ref->string = strndup(str, len);
    ref->hash.val = hash;
    return ref;
}

/* Adapted from http://www.cse.yorku.ca/~oz/hash.html.
 */
unsigned long djb2_hash(const char *str)
{
    unsigned long hash = 5381;
    int c;

    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }

    return hash;
}

/* Adapted from http://www.cse.yorku.ca/~oz/hash.html.
 */
unsigned long djb2_hash_p(const char *str, const char *endptr)
{
    unsigned long hash = 5381;
    int c;

    while (str < endptr) {
        c = *str++;
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }

    return hash;
}

const char *str_register(const char *s)
{
    struct string *str = hash_insert(s, strlen(s));
    return str->string;
}

const char *str_register_n(const char *s, size_t n)
{
    struct string *str = hash_insert(s, n);
    return str->string;
}

static void assign_string(const char *name, const char *str)
{
    struct var target, value = var_string(str);
    struct symbol *sym =
        sym_add(&ns_ident, name, value.type, SYM_DEFINITION, LINK_INTERN);

    target = var_direct(sym);
    target.lvalue = 1;
    eval_assign(current_cfg.rodata, target, value);
}

const char *strlabel(const char *s)
{
    struct string *ref;

    ref = hash_insert(s, strlen(s));
    if (!ref->label) {
        static int n;

        /* Integer (32 bit) can be at most 10 digits. Leave 3 for constant
         * prefix, and one for trailing null byte. */
        ref->label = calloc(14, sizeof(*ref->label));
        snprintf(ref->label, 14, ".LC%d", n++);

        assign_string(ref->label, s);
    }

    return ref->label;
}
