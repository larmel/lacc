#ifndef MEMOIZE_H
#define MEMOIZE_H

struct memo;

struct memo *memoize_init(void);

void memoize_free(struct memo *memo);

/* Return non-zero if input identifier has not yet been added to memoization set,
 * and add the string. Otherwise return zero. Intended usage is to have a guard
 * at the beginning of memoized functions.
 * 
 *  if (memoize_guard(memo, str)) { ... }
 * 
 */
int memoize_guard(struct memo *memo, const char *input);

/* Return non-zero if the input case is not memoized.
 */
int is_memoized(const struct memo *memo, const char *input);

#endif
