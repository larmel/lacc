#ifndef HASH_H
#define HASH_H

/* Compute hash of string.
 */
unsigned long djb2_hash(const char *str);

/* Compute hash of string, up to (but not including) endptr.
 */
unsigned long djb2_hash_p(const char *str, const char *endptr);

#endif
