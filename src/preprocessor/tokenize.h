#ifndef TOKENIZE_H
#define TOKENIZE_H

#include <lacc/token.h>

/* Mapping to ASCII indexed token strings.
 */
extern const struct string reserved[128];

/* Global instances of tokens representing end of input, and end of line,
 * respectively.
 */
extern const struct token
    token_end,
    token_newline;

/* Parse and return next preprocessing token from given line. Assume comments
 * are removed and line continuations are applied. endptr is set to point to
 * one index past the last character producing the token.
 *
 * Destructively overwrites input buffer for string constants.
 */
struct token tokenize(char *in, char **endptr);

#endif
