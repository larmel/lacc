#ifndef LIST_H
#define LIST_H

#include <stddef.h>

typedef struct list list_t;

list_t *list_init(void);

void list_push_back(list_t *list, void *elem);

void *list_get(const list_t *list, size_t i);

size_t list_length(const list_t *list);

void list_finalize(list_t *list);

#endif
