#ifndef STRING_H
#define STRING_H

#include <stdio.h>

/* Register a string and store it internally, allocating a new copy if needed.
 */
const char *str_register(const char *s);
const char *str_register_n(const char *s, size_t n);

/* Return an existing, or generate a new unique label representing the provided
 * string. Labels are used verbatim for assembly tags.
 */
const char *strlabel(const char *);

/* Output a single string.
 */
void output_string(FILE *, const char *);

/* Assemble strings readonly data section, GNU assembler syntax.
 */
void output_strings(FILE *);

#endif
