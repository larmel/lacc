#include <lacc/hash.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>

struct hash_entry {
    /* We don't own the data, only keep pointers to some block of memory
     * controlled by the client. */
    void *data;

    unsigned long hash;
    struct hash_entry *next;
};

enum hash_op {
    HASH_LOOKUP,
    HASH_INSERT,
    HASH_REMOVE
};

/* Hash algorithm is adapted from http://www.cse.yorku.ca/~oz/hash.html.
 */
static unsigned long djb2_hash(struct string str)
{
    int c;
    unsigned long hash = 5381;
    const char
        *p = str.str,
        *q = p + str.len;

    while (p < q) {
        c = *p++;
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }

    return hash;
}

static struct hash_entry *hash_walk(
    struct hash_table *tab,
    enum hash_op op,
    struct string key)
{
    /* Sentinel to hold return value from deleted entries, as we want to
     * have the same interface for all invocations. */
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
            ref->next = calloc(1, sizeof(*ref));

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
                /* Delete entry in first slot, moving the next element
                 * in if it exists, or reset to zero. */
                if (ref->next)
                    *ref = *(ref->next);
                else
                    memset(ref, 0, sizeof(*ref));
            } else {
                pre->next = ref->next;
                free(ref);
            }

            /* Entry is freed, but pointer to data as well as hash value
             * is still valid. */
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

struct hash_table *hash_init(
    struct hash_table *tab,
    unsigned cap,
    struct string (*key)(void *),
    void *(*add)(void *),
    void (*del)(void *))
{
    assert(cap > 0);
    assert(key);

    tab->capacity = cap;
    tab->key = key;
    tab->add = add ? add : hash_add_identity;
    tab->del = del ? del : hash_del_noop;
    tab->table = calloc(tab->capacity, sizeof(*tab->table));
    return tab;
}

void hash_chain_free(struct hash_entry *ref, void (*del)(void *))
{
    assert(ref->data);

    if (ref->next) {
        hash_chain_free(ref->next, del);
        free(ref->next);
    }

    del(ref->data);
}

void hash_destroy(struct hash_table *tab)
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

    free(tab->table);
}

void *hash_insert(struct hash_table *tab, void *val)
{
    struct hash_entry *ref;

    assert(val);
    assert(tab->table);

    ref = hash_walk(tab, HASH_INSERT, tab->key(val));
    if (!ref->data)
        ref->data = tab->add(val);

    return ref->data;
}

void *hash_lookup(struct hash_table *tab, struct string key)
{
    struct hash_entry *ref;

    ref = hash_walk(tab, HASH_LOOKUP, key);
    return ref ? ref->data : NULL;
}

void hash_remove(struct hash_table *tab, struct string key)
{
    struct hash_entry *ref;

    ref = hash_walk(tab, HASH_REMOVE, key);
    if (ref)
        tab->del(ref->data);
}
