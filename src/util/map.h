#ifndef MAP_H
#define MAP_H

#include <stdlib.h>
#include <string.h>

/* Store state representing a key-value map. Keys are of type const char*,
 * and values are pointers to arbitrary objects (void *). */
typedef struct map {
    const char **keys;
    void **values;

    size_t size;
} map_t;

void
map_init(map_t *map);

void *
map_lookup(map_t *map, const char *key);

void *
map_insert(map_t *map, const char *key, void *value);

void *
map_remove(map_t *map, const char *key);

void
map_finalize(map_t *map);

#endif
