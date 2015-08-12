#ifndef LIST_H
#define LIST_H

#include <stddef.h>

struct list;

/* Create and initialize a list
 */
struct list *list_init(void);

/* Add an element to the end of the list.
 */
void list_push_back(struct list *list, void *elem);

/* Return element at position i.
 */
void *list_get(const struct list *list, size_t i);

/* Return number of elements.
 */
size_t list_length(const struct list *list);

/* Release resources.
 */
void list_finalize(struct list *list);

#endif
