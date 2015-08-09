#include "list.h"

#include <assert.h>
#include <stdlib.h>

struct list
{
    void **elem;
    size_t length;
};

list_t *list_init(void)
{
    list_t *list = calloc(1, sizeof(*list));
    return list;
}

void list_finalize(list_t *list)
{
    assert(list);
    if (list->elem) {
        free(list->elem);
    }
    free(list);
}

void list_push_back(list_t *list, void *elem)
{
    assert(list);
    list->length++;
    list->elem = realloc(list->elem, sizeof(*list->elem) * list->length);
    list->elem[list->length - 1] = elem;
}

void *list_get(const list_t *list, size_t i)
{
    assert(i < list->length);
    return list->elem[i];
}

size_t list_length(const list_t *list)
{
    assert(list);
    return list->length;
}
