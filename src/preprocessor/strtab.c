#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "strtab.h"
#include <lacc/hash.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define STRTAB_SIZE 1024

static struct hash_table strtab;

/* Buffer used to concatenate strings before registering them. */
static char *catbuf;
static size_t catlen;

static int initialized;

/*
 * Every unique string encountered, being identifiers or literals, is
 * kept for the lifetime of the program. To save allocations, store the
 * raw string buffer in the same allocation as the struct.
 *
 *  _________ String ________    ________ const char [] ________
 * |                          | |                               |
 * [ <ptr to data> | <len>  x ] [ 'H', 'e', 'l', 'l', 'o', '\0' ]
 */
static void *str_hash_add(void *ref)
{
    String *s;
    char *buf, *str;
    size_t l;

    s = (String *) ref;
    l = str_len(*s);

    assert(!IS_SHORT_STRING(*s));
    assert(sizeof(String) == 16);
    buf = malloc(sizeof(String) + l + 1);
    str = buf + sizeof(String);
    memcpy(str, str_raw(*s), l);
    str[l] = '\0';

    s = (String *) buf;
    str_set(s, str, l);
    return s;
}

static String str_hash_key(void *ref)
{
    String *str = (String *) ref;
    return *str;
}

INTERNAL void strtab_reset(void)
{
    if (initialized) {
        hash_destroy(&strtab);
        initialized = 0;
    }

    free(catbuf);
    catbuf = NULL;
    catlen = 0;
}

INTERNAL String str_register(const char *str, size_t len)
{
    String data, *ref;

    str_set(&data, str, len);
    if (IS_SHORT_STRING(data)) {
        return data;
    }

    if (!initialized) {
        hash_init(&strtab, STRTAB_SIZE, str_hash_key, str_hash_add, free);
        initialized = 1;
    }

    ref = hash_insert(&strtab, &data);
    return *ref;
}

INTERNAL String str_cat(String a, String b)
{
    size_t len, la, lb;

    la = str_len(a);
    lb = str_len(b);
    len = la + lb;
    if (len > catlen) {
        catlen = len;
        catbuf = realloc(catbuf, catlen);
    }

    memcpy(catbuf, str_raw(a), la);
    memcpy(catbuf + la, str_raw(b), lb);
    return str_register(catbuf, len);
}
