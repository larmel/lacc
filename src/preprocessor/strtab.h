#ifndef STRTAB_H
#define STRTAB_H

#include <stddef.h>

/* Register a string and store it internally, allocating a new copy if needed.
 * Manages memory ownership for all string constants used at runtime.
 */
const char *str_register(const char *s);
const char *str_register_n(const char *s, size_t n);

#endif
