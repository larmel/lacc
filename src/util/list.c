#include "list.h"

#include <assert.h>
#include <stdlib.h>

struct list
{
    void **elem;
    size_t length;
};

struct list *list_init(void)
{
    struct list *list = calloc(1, sizeof(*list));
    if (!list) {
        exit(1);
    }
    return list;
}

void list_finalize(struct list *list)
{
    assert(list);
    if (list->elem) {
        free(list->elem);
    }
    free(list);
}

void list_push_back(struct list *list, void *elem)
{
    assert(list);
    list->length++;
    list->elem = realloc(list->elem, sizeof(*list->elem) * list->length);
    list->elem[list->length - 1] = elem;
}

void *list_get(const struct list *list, size_t i)
{
    assert(i < list->length);
    return list->elem[i];
}

size_t list_length(const struct list *list)
{
    assert(list);
    return list->length;
}
