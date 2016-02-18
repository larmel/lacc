#include "strtab.h"
#include <lacc/hash.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define STRTAB_SIZE 1024

static struct hash_table strtab;

static void strtab_free(void)
{
    hash_destroy(&strtab);
}

static void *str_hash_add(void *ref)
{
    char *buf;
    struct string
        *str = (struct string *) ref,
        *dup;

    buf = calloc(str->len + 1, sizeof(*buf));
    dup = calloc(1, sizeof(*dup));

    dup->str = memcpy(buf, str->str, str->len);
    dup->len = str->len;

    return dup;
}

static void str_hash_del(void *ref)
{
    struct string *str = (struct string *) ref;

    free((char *) str->str);
    free(str);
}

static struct string str_hash_key(void *ref)
{
    struct string *str = (struct string *) ref;
    return *str;
}

struct string str_register(const char *str, size_t len)
{
    static int initialized;
    struct string data, *ref;
    assert(len >= 0);

    if (!initialized) {
        hash_init(
            &strtab,
            STRTAB_SIZE,
            str_hash_key,
            str_hash_add,
            str_hash_del);

        atexit(strtab_free);
        initialized = 1;
    }

    data.str = str;
    data.len = len;

    ref = hash_insert(&strtab, &data);
    return *ref;
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
