#include <lacc/hash.h>

/* 
 * Hash algorithm is adapted from http://www.cse.yorku.ca/~oz/hash.html.
 */

unsigned long djb2_hash(const char *str)
{
    unsigned long hash = 5381;
    int c;

    while ((c = *str++)) {
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }

    return hash;
}

unsigned long djb2_hash_p(const char *str, const char *endptr)
{
    unsigned long hash = 5381;
    int c;

    while (str < endptr) {
        c = *str++;
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }

    return hash;
}
