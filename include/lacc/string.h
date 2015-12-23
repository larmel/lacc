#ifndef STRING_H
#define STRING_H

/* Register a string and store it internally, allocating a new copy if needed.
 */
const char *str_register(const char *s);
const char *str_register_n(const char *s, size_t n);

#endif
