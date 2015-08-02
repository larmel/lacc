#ifndef STRING_H
#define STRING_H

#include <stdio.h>

/* Return an existing, or generate a new unique label representing the provided
 * string. Labels are used verbatim for assembly tags.
 */
const char *strlabel(const char *);

void output_string(FILE *, const char *);

void output_strings(FILE *);

#endif
