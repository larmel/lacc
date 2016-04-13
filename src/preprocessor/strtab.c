#include "strtab.h"
#include <lacc/hash.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define STRTAB_SIZE 1024

static struct hash_table strtab;

/* Every unique string encountered, being identifiers or literals, is
 * kept for the lifetime of the program. To save allocations, store the
 * raw string buffer in the same allocation as the struct.
 *
 *  _____ struct string _____    ________ const char [] ________
 * |                          | |                               |
 * [ <ptr to data> | <length> ] [ 'H', 'e', 'l', 'l', 'o', '\0' ]
 */
static void *str_hash_add(void *ref)
{
    char *buffer;
    struct string *s;
    size_t l;

    s = (struct string *) ref;
    l = s->len;
    buffer = malloc(sizeof(struct string) + l + 1);
    buffer[sizeof(struct string) + l] = '\0';
    memcpy(buffer + sizeof(struct string), s->str, l);
    s = (struct string *) buffer;
    s->str = buffer + sizeof(*s);
    s->len = l;
    return s;
}

static struct string str_hash_key(void *ref)
{
    struct string *str = (struct string *) ref;
    return *str;
}

static void strtab_free(void)
{
    hash_destroy(&strtab);
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
            free);

        atexit(strtab_free);
        initialized = 1;
    }

    data.str = str;
    data.len = len;

    ref = hash_insert(&strtab, &data);
    return *ref;
}
