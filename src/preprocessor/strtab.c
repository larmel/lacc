#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "strtab.h"
#include <lacc/array.h>
#include <lacc/context.h>
#include <lacc/hash.h>

#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define STRTAB_CAPACITY_INITIAL 2048
#define STRTAB_CAPACITY_MAX INT_MAX

/*
 * Global structure containing a singleton instance of all unique string
 * values encountered in the translation.
 */
static struct {
    /*
     * Number of slots in table. Require to be a power of 2, in order to
     * do efficient modulo calculation by bitwise and.
     */
    int capacity;

    /* Number of entries currently in table. */
    int count;

    struct strtab_entry {
        int hash;
        int length;
        char *value;
    } *entries;
} strtab;

/* Buffer used to concatenate strings before registering them. */
static char *catbuf;
static size_t catlen;

/* List of all long double values encountered. */
static array_of(long double) long_double_values;

INTERNAL void strtab_reset(void)
{
    int i;

    for (i = 0; i < strtab.capacity; ++i) {
        free(strtab.entries[i].value);
    }

    free(strtab.entries);
    memset(&strtab, 0, sizeof(strtab));
    free(catbuf);
    catbuf = NULL;
    catlen = 0;
    array_clear(&long_double_values);
}

/*
 * Hash algorithm is adapted from http://www.cse.yorku.ca/~oz/hash.html.
 */
static int djb2_hash(const char *str, size_t len)
{
    int c, hash = 5381;
    const char *p = str, *q = str + len;

    while (p < q) {
        c = *p++;
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }

    return hash;
}

static int strtab_match_entry(
    const struct strtab_entry *entry,
    const char *value,
    int length,
    int hash)
{
    return !entry->value || (entry->hash == hash
        && entry->length == length
        && !memcmp(entry->value, value, length));
}

static struct strtab_entry *strtab_find_entry(
    const char *value,
    int length,
    int hash)
{
    int i, j;
    struct strtab_entry *entry;

    assert(strtab.capacity > 0);
    i = hash & (strtab.capacity - 1);
    entry = &strtab.entries[i];
    if (strtab_match_entry(entry, value, length, hash)) {
        return entry;
    }

    for (j = i + 1; j < strtab.capacity; ++j) {
        entry = &strtab.entries[j];
        if (strtab_match_entry(entry, value, length, hash)) {
            return entry;
        }
    }

    for (j = 0; j < i; ++j) {
        entry = &strtab.entries[j];
        if (strtab_match_entry(entry, value, length, hash)) {
            return entry;
        }
    }

    assert(0);
    return NULL;
}

static int strtab_is_full(void)
{
    return strtab.count >= strtab.capacity / 2;
}

static void strtab_expand(void)
{
    int i, cap;
    struct strtab_entry *tab, *entry;

    tab = strtab.entries;
    cap = strtab.capacity;
    if (!cap) {
        strtab.capacity = STRTAB_CAPACITY_INITIAL;
    } else if (strtab.capacity < STRTAB_CAPACITY_MAX) {
        strtab.capacity = strtab.capacity * 2;
    } else {
        error("Reached max number of strings after %d elements.", strtab.count);
        exit(1);
    }

    strtab.entries = calloc(strtab.capacity, sizeof(struct strtab_entry));
    for (i = 0; i < cap; ++i) {
        if (tab[i].value) {
            entry = strtab_find_entry(tab[i].value, tab[i].length, tab[i].hash);
            *entry = tab[i];
        }
    }

    free(tab);
}

INTERNAL String str_intern(const char *buf, size_t len)
{
    int hash, i;
    struct strtab_entry *entry;
    String str = {0};

    if (len <= SHORT_STRING_LEN) {
        for (i = 0; i < len; ++i) {
            str.small.buf[i] = buf[i];
        }

        str.small.cap = SHORT_STRING_LEN - len;
        assert(IS_SHORT_STRING(str));
        assert(str_len(str) == len);
        return str;
    }

    if (len > MAX_STRING_LEN) {
        error("String length %lu exceeds maximum supported size.", len);
        exit(1);
    }

    if (strtab_is_full()) {
        strtab_expand();
    }

    assert(!strtab_is_full());
    assert(strtab.capacity > 0);
    assert(strtab.count < strtab.capacity - 1);
    hash = djb2_hash(buf, len);
    entry = strtab_find_entry(buf, len, hash);

    if (!entry->value) {
        strtab.count++;
        entry->hash = hash;
        entry->length = len;
        entry->value = malloc(len + 1);
        memcpy(entry->value, buf, len);
        entry->value[len] = '\0';
    }

    assert(entry->hash == hash);
    assert(entry->length == len);
    assert(!memcmp(entry->value, buf, len));
    assert(len > SHORT_STRING_LEN);
    assert(len <= MAX_STRING_LEN);

    str.large.ptr = entry->value;
    str.large.len = len;
    str.small.cap = -1;
    assert(!IS_SHORT_STRING(str));
    assert(str_len(str) == len);
    return str;
}

INTERNAL String str_c(const char *s)
{
    return str_intern(s, strlen(s));
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
    return str_intern(catbuf, len);
}

INTERNAL union value put_long_double(long double ld)
{
    int i;
    long double x;
    union value val = {0};

    for (i = 0; i < array_len(&long_double_values); ++i) {
        x = array_get(&long_double_values, i);
        if (x == ld) {
            val.ld = i;
            return val;
        }
    }

    val.ld = i;
    array_push_back(&long_double_values, ld);
    assert(get_long_double(val) == ld);
    return val;
}

INTERNAL long double get_long_double(union value value)
{
    assert(value.ld < array_len(&long_double_values));

    return array_get(&long_double_values, value.ld);
}
