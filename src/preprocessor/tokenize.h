#ifndef TOKENIZE_H
#define TOKENIZE_H

#include <lacc/token.h>

/* Table is indexed by ASCII value, which is also assigned to
 * corresponding token type. To get a token of a particular type,
 * access basic_token[type].
 */
extern const struct token basic_token[128];

/* String representation of token.
 */
struct string tokstr(struct token tok);

/* Concatenate two tokens to produce a new string token.
 */
struct token pastetok(struct token a, struct token b);

/* Parse and return next preprocessing token from given line. Assume
 * comments are removed and line continuations are applied. endptr is
 * set to point to one index past the last character producing the
 * token.
 *
 * Destructively overwrites input buffer for string constants.
 */
struct token tokenize(char *in, char **endptr);

#endif
