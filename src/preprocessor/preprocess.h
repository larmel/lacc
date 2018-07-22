#ifndef PREPROCESS_H
#define PREPROCESS_H

#include <stdio.h>

/*
 * Output preprocessed input to provided stream, toggled by -E program
 * option.
 */
INTERNAL void preprocess(FILE *output);

/*
 * Preprocess a single line, adding any resulting tokens to the
 * lookahead buffer. This should happen before any input file is read.
 *
 * The string is destructively tokenized. It should contain no newlines,
 * comments, or line continuations.
 */
INTERNAL void inject_line(char *line);

/* Initialize data structures used for preprocessing. */
INTERNAL void preprocess_reset(void);

/* Free memory used for preprocessing. */
INTERNAL void preprocess_finalize(void);

#endif
