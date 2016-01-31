#ifndef STRTAB_H
#define STRTAB_H

#include "lacc/string.h"

#include <stddef.h>

/* Register a string and store it internally, allocating a new copy if needed.
 * Manages memory ownership for all string constants used at runtime.
 */
struct string str_register(const char *str, size_t len);

#endif
