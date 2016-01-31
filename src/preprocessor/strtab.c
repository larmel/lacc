#include "strtab.h"
#include <lacc/hash.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define HASH_TABLE_LENGTH 1024

static struct entry {
    struct string string;
    struct {
        unsigned long val;
        struct entry *next;
    } hash;
} str_hash_tab[HASH_TABLE_LENGTH];

static void hash_node_cleanup(struct entry *ref)
{
    if (ref->hash.next)
        hash_node_cleanup(ref->hash.next);

    free((void *) ref->string.str);
    free(ref);
}

static void cleanup(void)
{
    int i;
    struct entry *ref;

    for (i = 0; i < HASH_TABLE_LENGTH; ++i) {
        ref = &str_hash_tab[i];
        if (ref->hash.next)
            hash_node_cleanup(ref->hash.next);
        if (ref->string.len)
            free((void *) ref->string.str);
    }
}

static struct string str_dup(struct string s)
{
    char *buf = calloc(s.len + 1, sizeof(*buf));
    s.str = memcpy(buf, s.str, s.len);
    return s;
}

static struct entry *hash_insert(const char *str, size_t len)
{
    static int reg_cleanup;
    struct string s;
    struct entry *ref;
    unsigned long
        hash = djb2_hash_p(str, str + len),
        pos = hash % HASH_TABLE_LENGTH;

    if (!reg_cleanup) {
        atexit(cleanup);
        reg_cleanup = 1;
    }

    s.str = str;
    s.len = len;

    ref = &str_hash_tab[pos];
    if (!ref->string.str) {
        ref->string = str_dup(s);
        ref->hash.val = hash;
        return ref;
    }

    while ((ref->hash.val != hash || str_cmp(ref->string, s)) && ref->hash.next)
        ref = ref->hash.next;

    if (ref->hash.val == hash && !str_cmp(ref->string, s)) {
        return ref;
    }

    assert(!ref->hash.next);
    ref->hash.next = calloc(1, sizeof(*ref));
    ref = ref->hash.next;

    ref->string = str_dup(s);
    ref->hash.val = hash;
    return ref;
}

struct string str_register(const char *str, size_t len)
{
    struct entry *e;
    assert(len >= 0);

    e = hash_insert(str, len);
    return e->string;
}

struct string str_init(const char *str)
{
    struct string s = {0};
    s.str = str;
    s.len = strlen(str);
    return s;
}

int str_cmp(struct string s1, struct string s2)
{
    return (s1.len != s2.len) || memcmp(s1.str, s2.str, s1.len);
}
