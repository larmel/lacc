#include "map.h"

#include <stdlib.h>
#include <string.h>

map_t *
map_init()
{
	map_t *map = calloc(1, sizeof(map));
	return map;
}

void *
map_lookup(map_t *map, const char *key)
{
	int i;
	for (i = 0; i < map->size; ++i) {
		if (!strcmp(key, map->keys[i])) {
			return map->values[i];
		}
	}
	return NULL;
}

void *
map_insert(map_t *map, const char *key, void *value)
{
	int i;
	for (i = 0; i < map->size; ++i)
		if (!strcmp(key, map->keys[i]))
			break;
	if (i == map->size) {
		map->size++;
		map->keys = realloc(map->keys, map->size * sizeof(char*));
		map->values = realloc(map->values, map->size * sizeof(void*));
		map->keys[i] = strdup(key); /* create a copy of the key */
	}
	map->values[i] = value;
	return value;
}

void *
map_remove(map_t *map, const char *key)
{
	int i;
	void *value = NULL;
	for (i = 0; i < map->size; ++i) {
		if (!strcmp(key, map->keys[i])) {
			map->size--;
			value = map->values[i];
			free((void*) map->keys[i]);
			break;
		}
	}
	for ( ; i < map->size; ++i) {
		map->keys[i] = map->keys[i + 1];
		map->values[i] = map->values[i + 1];
	}
	return value;
}
