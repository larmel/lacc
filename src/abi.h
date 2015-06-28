#ifndef ABI_H
#define ABI_H

#include "type.h"

#define N_EIGHTBYTES(t) (((t)->size + 7) / 8)

/* Parameter class of an 8-byte slice of an object.
 */
enum param_class {
    PC_NO_CLASS = 0,
    PC_INTEGER,
    PC_SSE,
    PC_SSEUP,
    PC_MEMORY
};

/* */
enum param_class **
classify_call(const struct typetree **args,
              const struct typetree *ret,
              int n_args,
              enum param_class **out);

/* */
enum param_class **
classify_signature(const struct typetree *func, enum param_class **out);

/* DEBUG */
void dump_classification(const enum param_class *c, const struct typetree *t);

#endif
