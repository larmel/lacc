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
 * [ <len> | <ptr to data>    ] [ 'H', 'e', 'l', 'l', 'o', '\0' ]
 */
static void *str_hash_add(void *ref)
{
    String *s;
    char *buffer;
    unsigned short l;

    s = (String *) ref;
    l = s->p.len;
    buffer = malloc(sizeof(String) + l + 1);
    buffer[sizeof(String) + l] = '\0';
    memcpy(buffer + sizeof(String), s->p.str, l);
    s = (String *) buffer;
    s->p.str = buffer + sizeof(*s);
    s->p.len = l;
    return s;
}

static String str_hash_key(void *ref)
{
    String *str = (String *) ref;
    return *str;
}

void clear_string_table(void)
{
    if (initialized) {
        hash_destroy(&strtab);
        initialized = 0;
    }

    free(catbuf);
}

String str_register(const char *str, size_t len)
{
    String data = {0}, *ref;
    assert(len >= 0);

    if (len < SHORT_STRING_LEN) {
        memcpy(data.a.str, str, len);
        data.a.len = len;
        ref = &data;
    } else {
        if (!initialized) {
            hash_init(
                &strtab,
                STRTAB_SIZE,
                str_hash_key,
                str_hash_add,
                free);
            initialized = 1;
        }
        data.p.str = str;
        data.p.len = len;
        ref = hash_insert(&strtab, &data);
    }

    return *ref;
}

String str_cat(String a, String b)
{
    size_t len;

    len = a.len + b.len;
    if (len > catlen) {
        catlen = len;
        catbuf = realloc(catbuf, catlen);
    }

    memcpy(catbuf, str_raw(a), a.len);
    memcpy(catbuf + a.len, str_raw(b), b.len);
    return str_register(catbuf, len);
}
