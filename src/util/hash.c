#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include <lacc/hash.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>

struct hash_entry {
    /*
     * We don't own the data, only keep pointers to some block of memory
     * controlled by the client.
     */
    void *data;

    unsigned long hash;
    struct hash_entry *next;
};

enum hash_op {
    HASH_LOOKUP,
    HASH_INSERT,
    HASH_REMOVE
};

/*
 * Hash algorithm is adapted from http://www.cse.yorku.ca/~oz/hash.html.
 */
static unsigned long djb2_hash(String str)
{
    int c;
    unsigned long hash = 5381;
    const char
        *p = str_raw(str),
        *q = p + str.len;

    while (p < q) {
        c = *p++;
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }

    return hash;
}

static struct hash_entry *hash_alloc_entry(struct hash_table *tab)
{
    struct hash_entry *ref;

    ref = tab->table[tab->capacity].next;
    if (ref) {
        tab->table[tab->capacity].next = ref->next;
        memset(ref, 0, sizeof(*ref));
    } else {
        ref = calloc(1, sizeof(*ref));
    }

    return ref;
}

static void hash_free_entry(struct hash_table *tab, struct hash_entry *ref)
{
    ref->next = tab->table[tab->capacity].next;
    tab->table[tab->capacity].next = ref;
}

static struct hash_entry *hash_walk(
    struct hash_table *tab,
    enum hash_op op,
    String key)
{
    /*
     * Sentinel to hold return value from deleted entries, as we want to
     * have the same interface for all invocations.
     */
    static struct hash_entry deleted;

    struct hash_entry *ref, *pre;
    unsigned long
        hash = djb2_hash(key),
        pos = hash % tab->capacity;

    pre = NULL;
    ref = &tab->table[pos];
    while (ref && ref->data) {
        if (ref->hash == hash && !str_cmp(tab->key(ref->data), key))
            break;

        if (!ref->next && op == HASH_INSERT)
            ref->next = hash_alloc_entry(tab);

        pre = ref;
        ref = ref->next;
    }

    if (op == HASH_INSERT) {
        assert(!ref->data || ref->hash == hash);
        ref->hash = hash;
    } else if (op == HASH_REMOVE && ref) {
        if (!ref->data) {
            assert(!pre);
            assert(!ref->hash);

            /* Delete in first slot, but no data. Nothing to do. */
            ref = NULL;
        } else {
            deleted.data = ref->data;
            deleted.hash = ref->hash;

            if (!pre) {
                /*
                 * Delete entry in first slot, moving the next element
                 * in if it exists, or reset to zero.
                 */
                if (ref->next)
                    *ref = *(ref->next);
                else
                    memset(ref, 0, sizeof(*ref));
            } else {
                pre->next = ref->next;
                hash_free_entry(tab, ref);
            }

            /*
             * Entry is freed, but pointer to data as well as hash value
             * is still valid.
             */
            ref = &deleted;
        }
    }

    return ref;
}

static void *hash_add_identity(void *elem)
{
    return elem;
}

static void hash_del_noop(void *elem)
{
    return;
}

static void hash_chain_free(struct hash_entry *ref, void (*del)(void *))
{
    if (ref->next) {
        hash_chain_free(ref->next, del);
        free(ref->next);
    }

    del(ref->data);
}

static struct hash_entry *hash_chain_clear(
    struct hash_table *tab,
    struct hash_entry *ref)
{
    struct hash_entry *last = ref;
    assert(tab->table);
    assert(ref->data);

    if (ref->next) {
        last = hash_chain_clear(tab, ref->next);
    }

    tab->del(ref->data);
    return last;
}

INTERNAL struct hash_table *hash_init(
    struct hash_table *tab,
    unsigned cap,
    String (*key)(void *),
    void *(*add)(void *),
    void (*del)(void *))
{
    assert(cap > 0);
    assert(key);

    tab->capacity = cap;
    tab->key = key;
    tab->add = add ? add : hash_add_identity;
    tab->del = del ? del : hash_del_noop;
    tab->table = calloc(tab->capacity + 1, sizeof(*tab->table));
    return tab;
}

/*
 * Delete all values, putting hash_entry objects already allocated in
 * overflow slot at table[capacity].
 */
INTERNAL void hash_clear(struct hash_table *tab)
{
    unsigned i;
    struct hash_entry *ref, *last;
    assert(tab->table);

    for (i = 0; i < tab->capacity; ++i) {
        ref = &tab->table[i];
        if (ref->data) {
            tab->del(ref->data);
            if (ref->next) {
                /*
                 * Put chain of allocated hash_entry objects at the
                 * beginning of overflow slot.
                 */
                last = hash_chain_clear(tab, ref->next);
                last->next = tab->table[tab->capacity].next;
                tab->table[tab->capacity].next = ref->next;
            }
        }
    }

    memset(tab->table, 0, sizeof(*tab->table) * tab->capacity);
}

INTERNAL void hash_destroy(struct hash_table *tab)
{
    unsigned i;
    struct hash_entry *ref;

    assert(tab->table);
    for (i = 0; i < tab->capacity; ++i) {
        ref = &tab->table[i];

        if (ref->data)
            hash_chain_free(ref, tab->del);
        else
            assert(!ref->next);
    }

    hash_chain_free(&tab->table[tab->capacity], &hash_del_noop);
    free(tab->table);
}

INTERNAL void *hash_insert(struct hash_table *tab, void *val)
{
    struct hash_entry *ref;

    assert(val);
    assert(tab->table);

    ref = hash_walk(tab, HASH_INSERT, tab->key(val));
    if (!ref->data)
        ref->data = tab->add(val);

    return ref->data;
}

INTERNAL void *hash_lookup(struct hash_table *tab, String key)
{
    struct hash_entry *ref;

    ref = hash_walk(tab, HASH_LOOKUP, key);
    return ref ? ref->data : NULL;
}

INTERNAL void hash_remove(struct hash_table *tab, String key)
{
    struct hash_entry *ref;

    ref = hash_walk(tab, HASH_REMOVE, key);
    if (ref)
        tab->del(ref->data);
}
