#ifndef PREPROCESS_H
#define PREPROCESS_H

#include <stdio.h>

/* Output preprocessed input to provided stream, toggled by -E program
 * option.
 */
void preprocess(FILE *output);

#endif
