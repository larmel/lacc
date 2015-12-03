#ifndef TOKENIZE_H
#define TOKENIZE_H

#include <lacc/token.h>

/* Mapping to ASCII indexed token strings.
 */
extern const char *reserved[128];

/* Parse and return next preprocessing token from given line. Assume comments
 * are removed and line continuations are applied. endptr is set to point to
 * one index past the last character producing the token.
 *
 * Destructively overwrites input buffer for string constants.
 */
struct token tokenize(char *in, char **endptr);

/* Global instances of tokens representing end of input, and end of line,
 * respectively.
 */
extern struct token
    token_end,
    token_newline;

#endif
