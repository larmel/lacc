#ifndef PREPROCESS_H
#define PREPROCESS_H

#include <stdio.h>

/*
 * Output preprocessed input to provided stream, toggled by -E program
 * option.
 */
void preprocess(FILE *output);

/*
 * Preprocess line specified by command line parameter. This should
 * happen before any input file is read.
 *
 * The string is destructively tokenized. It should contain no newlines,
 * comments, or line continuations.
 */
void preprocess_parameter_directive(char *line);

#endif
