#ifndef STRING_H
#define STRING_H

/* Compute hash of string.
 */
unsigned long djb2_hash(const char *str);
unsigned long djb2_hash_p(const char *str, const char *endptr);

/* Register a string and store it internally, allocating a new copy if needed.
 */
const char *str_register(const char *s);
const char *str_register_n(const char *s, size_t n);

/* Return an existing, or generate a new unique label representing the provided
 * string. Labels are used verbatim for assembly tags.
 */
const char *strlabel(const char *);

#endif
