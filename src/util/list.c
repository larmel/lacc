#include <lacc/list.h>

#include <assert.h>
#include <stdlib.h>
#include <string.h>

static unsigned expand_list(struct list *lst)
{
    unsigned old = lst->capacity;

    if (!lst->capacity) {
        assert(!lst->data);
        lst->capacity = 16;
        lst->data = calloc(lst->capacity, sizeof(*lst->data));
    } else {
        lst->capacity *= 2;
        lst->data = realloc(lst->data, lst->capacity * sizeof(*lst->data));
    }

    assert(lst->capacity > list_len(lst));
    assert(lst->capacity > old);

    return lst->capacity - old;
}

unsigned list_len(struct list *lst)
{
    assert(lst->tail <= lst->capacity);
    assert(lst->head <= lst->tail);

    return lst->tail - lst->head;
}

void *list_push(struct list *lst, void *elem)
{
    unsigned n, len = list_len(lst);
    assert(elem);

    if (!lst->head) {
        n = lst->capacity - lst->tail;
        if (!n) {
            assert(len == lst->capacity);
            n = expand_list(lst);
        }

        /* Move buffer half way over newly allocated or existing memory,
         * allowing equal growth in each direction. */
        n = (n + 1) / 2;
        memmove(
            &lst->data[lst->head + n],
            &lst->data[lst->head],
            len * sizeof(*lst->data));
        lst->head += n;
        lst->tail += n;
    }

    lst->data[--lst->head] = elem;

    return elem;
}

void *list_push_back(struct list *lst, void *elem)
{
    unsigned n, len = list_len(lst);
    assert(elem);

    if (lst->capacity == lst->tail) {
        n = lst->head;
        if (!n) {
            assert(lst->capacity == len);
            expand_list(lst);
        } else {
            /* Move data half way back over existing free memory, to
             * allow both push and push_back without immediately having
             * to redo memory allocation. */
            n = (n + 1) / 2;
            memmove(
                &lst->data[lst->head - n],
                &lst->data[lst->head],
                len * sizeof(*lst->data));
            lst->head -= n;
            lst->tail -= n;
        }
    }

    assert(lst->capacity > lst->tail);
    lst->data[lst->tail++] = elem;

    return elem;
}

void *list_pop(struct list *lst)
{
    if (list_len(lst))
        return lst->data[lst->head++];

    return NULL;
}

void *list_get(struct list *lst, unsigned i)
{
    if (i < list_len(lst))
        return lst->data[lst->head + i];

    return NULL;
}

void list_clear(struct list *lst, void (*del)(void *))
{
    unsigned i;

    if (lst->capacity) {
        if (del && list_len(lst)) {
            for (i = lst->head; i < lst->tail; ++i)
                del(lst->data[i]);
        }

        free(lst->data);
        memset(lst, 0, sizeof(*lst));
    } else {
        assert(!lst->head);
        assert(!lst->tail);
    }
}
