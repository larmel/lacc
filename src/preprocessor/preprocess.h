#ifndef PREPROCESS_H
#define PREPROCESS_H

#include <stdio.h>

/*
 * Output preprocessed input to provided stream, toggled by -E program
 * option.
 */
void preprocess(FILE *output);

/*
 * Preprocess a single line, adding any resulting tokens to the
 * lookahead buffer. This should happen before any input file is read.
 *
 * The string is destructively tokenized. It should contain no newlines,
 * comments, or line continuations.
 */
void inject_line(char *line);

#endif
