#include "strtab.h"
#include <lacc/hash.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define STRTAB_SIZE 1024

static struct hash_table strtab;

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

static void strtab_free(void)
{
    hash_destroy(&strtab);
}

String str_register(const char *str, size_t len)
{
    static int initialized;
    String data, *ref;
    assert(len >= 0);

    if (len < SHORT_STRING_LEN) {
        memcpy(data.a.str, str, len);
        data.a.str[len] = '\0';
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

            atexit(strtab_free);
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
    static char buf[SHORT_STRING_LEN + 1];
    int len;
    char *str;
    String s;

    len = a.len + b.len;
    if (len < SHORT_STRING_LEN) {
        str = buf;
    } else {
        str = calloc(len + 1, sizeof(*str));
    }

    memcpy(str, str_raw(a), a.len);
    memcpy(str + a.len, str_raw(b), b.len);
    s = str_register(str, len);
    if (str != buf) {
        free(str);
    }

    return s;
}
