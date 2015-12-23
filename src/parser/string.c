#if _XOPEN_SOURCE < 700
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 700 /* strndup */
#endif
#include "symtab.h"
#include <lacc/hash.h>
#include <lacc/string.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define HASH_TABLE_LENGTH 1024

struct string {
    size_t length;
    char *string;

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
