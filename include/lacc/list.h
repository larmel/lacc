#ifndef LIST_H
#define LIST_H

struct list {
    unsigned capacity;

    /* Indices pointing to first, and one beyond last element in the
     * array, which is contigous between these points. Empty list start
     * with pointers to (0, 0), zeroing the structure is a valid
     * representation of an empty list.
     *
     *
     *          head                    tail
     *           |                       |
     *
     *       0   1   2   3   4   5   6   7   8
     *     [   | X | X | X | X | X | X |   |   ]
     *
     */
    unsigned head, tail;

    /* Realloc-safe list of pointers to arbitrary data. */
    void **data;
};

unsigned list_len(struct list *lst);

void *list_push(struct list *lst, void *elem);

void *list_push_back(struct list *lst, void *elem);

void *list_pop(struct list *lst);

void *list_get(struct list *lst, unsigned i);

void list_clear(struct list *lst, void (*del)(void *));

#endif
