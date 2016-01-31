#ifndef STRING_H
#define STRING_H

#include <stddef.h>

struct string {
    const char *str;
    size_t len;
};

/* Initialize string, where the length can be determined by strlen.
 */
struct string str_init(const char *str);

/* Compare two strings, returning 0 if equal.
 */
int str_cmp(struct string s1, struct string s2);

#endif
