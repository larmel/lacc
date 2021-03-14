#ifndef HASH_H
#define HASH_H
#if !defined(INTERNAL) || !defined(EXTERNAL)
# error Missing amalgamation macros
#endif

#include "string.h"

struct hash_table {
    int capacity;
    int count;
    struct hash_entry *entries;
};

/* Reset table, clearing all values. Does not deallocate memory. */
INTERNAL void hash_clear(struct hash_table *tab, void (*del)(void *));

/* Free resources owned by table. */
INTERNAL void hash_destroy(struct hash_table *tab);

/*
 * Insert element, or return existing with the same key.
 *
 * Initializer, if provided, is invoked when data is added.
 */
INTERNAL void *hash_insert(
    struct hash_table *tab,
    String key,
    void *value,
    void *(*add)(void *, String *));

/* Retrieve element matching key, or NULL if not found. */
INTERNAL void *hash_lookup(struct hash_table *tab, String key);

/*
 * Remove element matching key.
 *
 * Finalizer, if provider, is invoked when data is removed.
 */
INTERNAL void hash_remove(
    struct hash_table *tab,
    String key,
    void (*del)(void *));

#endif
