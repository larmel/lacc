#ifndef PREPROCESS_H
#define PREPROCESS_H

#include "tokenize.h"

#include <stdio.h>

/* Peek lookahead of 1.
 */
struct token peek(void);

/* Peek lookahead of n.
 */
struct token peekn(unsigned n);

/* Consume and return next token.
 */
struct token next(void);

/* Consume and return next token, or fail of not of expected type.
 */
struct token consume(enum token_type type);

/* Output preprocessed input to provided stream, toggled by -E program option.
 */
void preprocess(FILE *output);

#endif
