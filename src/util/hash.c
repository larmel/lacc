#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include <lacc/context.h>
#include <lacc/hash.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define HASH_CAPACITY_INITIAL 16
#define HASH_CAPACITY_MAX INT_MAX

struct hash_entry {
    int hash;
    int deleted;
    String key;

    /*
     * We don't own the data, only keep pointers to some block of memory
     * controlled by the client.
     */
    void *value;
};

INTERNAL void hash_clear(struct hash_table *tab, void (*del)(void *))
{
    int i;
    struct hash_entry *entry;

    if (del) {
        for (i = 0; i < tab->capacity; ++i) {
            entry = &tab->entries[i];
            if (entry->value) {
                del(entry->value);
            }
        }
    }

    tab->count = 0;
    memset(tab->entries, 0, tab->capacity * sizeof(*tab->entries));
}

INTERNAL void hash_destroy(struct hash_table *tab)
{
    free(tab->entries);
    memset(tab, 0, sizeof(*tab));
}

static int hash_match_entry(
    const struct hash_entry *entry,
    String key,
    int hash)
{
    if (entry->deleted)
        return 0;

    return !entry->value || (entry->hash == hash && str_eq(entry->key, key));
}

static struct hash_entry *hash_find_entry(
    struct hash_table *tab,
    String key,
    int hash)
{
    int i, j;
    struct hash_entry *entry;

    assert(tab->capacity > 0);
    i = hash & (tab->capacity - 1);
    entry = &tab->entries[i];
    if (hash_match_entry(entry, key, hash)) {
        return entry;
    }

    for (j = i + 1; j < tab->capacity; ++j) {
        entry = &tab->entries[j];
        if (hash_match_entry(entry, key, hash)) {
            return entry;
        }
    }

    for (j = 0; j < i; ++j) {
        entry = &tab->entries[j];
        if (hash_match_entry(entry, key, hash)) {
            return entry;
        }
    }

    assert(0);
    return NULL;
}

static void hash_expand(struct hash_table *tab)
{
    int i;
    struct hash_entry *a, *b;
    struct hash_table copy = {0};

    if (!tab->capacity) {
        copy.capacity = HASH_CAPACITY_INITIAL;
    } else if (tab->capacity < HASH_CAPACITY_MAX) {
        copy.capacity = tab->capacity * 2;
    } else {
        error("Reached hash table size limit after %d elements.", tab->count);
        exit(1);
    }

    copy.count = tab->count;
    copy.entries = calloc(copy.capacity, sizeof(struct hash_entry));
    for (i = 0; i < tab->capacity; ++i) {
        a = &tab->entries[i];
        if (!a->value || a->deleted)
            continue;

        b = hash_find_entry(&copy, a->key, a->hash);
        b->hash = a->hash;
        b->key = a->key;
        b->value = a->value;
    }

    free(tab->entries);
    *tab = copy;
}

static int hash_is_full(struct hash_table *tab)
{
    return tab->count >= tab->capacity / 2;
}

INTERNAL void *hash_insert(
    struct hash_table *tab,
    String key,
    void *value,
    void *(*add)(void *, String *))
{
    int hash;
    struct hash_entry *entry;

    if (hash_is_full(tab)) {
        hash_expand(tab);
        assert(!hash_is_full(tab));
    }

    assert(tab->capacity > 0);
    assert(tab->count < tab->capacity - 1);
    hash = str_hash(key);
    entry = hash_find_entry(tab, key, hash);

    assert(!entry->deleted);
    if (!entry->value) {
        tab->count++;
        entry->hash = hash;
        if (add) {
            entry->value = add(value, &entry->key);
        } else {
            entry->key = key;
            entry->value = value;
        }
    }

    assert(entry->hash == hash && str_eq(entry->key, key));
    return entry->value;
}

INTERNAL void *hash_lookup(struct hash_table *tab, String key)
{
    int hash;
    struct hash_entry *entry;

    if (!tab->capacity)
        return NULL;

    hash = str_hash(key);
    entry = hash_find_entry(tab, key, hash);
    return entry->value;
}

INTERNAL void hash_remove(
    struct hash_table *tab,
    String key,
    void (*del)(void *))
{
    int hash;
    struct hash_entry *entry;

    if (!tab->capacity)
        return;

    hash = str_hash(key);
    entry = hash_find_entry(tab, key, hash);
    if (entry->value) {
        entry->deleted = 1;
        entry->hash = 0;
        if (del) {
            del(entry->value);
        }

        entry->value = NULL;
    }
}
