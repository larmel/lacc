#ifndef ABI_H
#define ABI_H

#include "instructions.h"
#include "../../core/symbol.h"

/* Registers used for passing INTEGER parameters.
 */
enum reg param_int_reg[6];

/* Registers used for returning INTEGER retult.
 */
enum reg ret_int_reg[2];

/* Number of eightbytes required for a given type.
 */
#define N_EIGHTBYTES(t) ((size_of(t) + 7) / 8)

/* Parameter class of an 8-byte slice of an object.
 */
enum param_class
{
    PC_NO_CLASS = 0,
    PC_INTEGER,
    PC_SSE,
    PC_SSEUP,
    PC_MEMORY
};

/* Classify parameter as a series of eightbytes used for parameter passing and
 * return value. If the first element is not PC_MEMORY, the number of elements
 * in the list can be determined by N_EIGHTBYTES(t).
 *
 * Returns a list of parameter classes, allocated dynamically. Caller should
 * free memory.
 */
enum param_class *classify(const struct typetree *t);

/* Classify function call, required as separate from classifying signature
 * because of variable length argument list.
 *
 * Returns a list of dynamically allocated classifications for parameters, and
 * writes return type class to output argument. Both must be free'd by caller.
 */
enum param_class **classify_call(
    const struct typetree **args,
    const struct typetree *ret,
    int n_args,
    enum param_class **res);

/* Classify parameters and return value of function type.
 *
 * Returns a list of dynamically allocated classifications for parameters, and
 * writes return type class to output argument. Both must be free'd by caller.
 */
enum param_class **classify_signature(
    const struct typetree *func,
    enum param_class **res);

/* Alignment of symbol in bytes.
 */
int sym_alignment(const struct symbol *sym);

/* DEBUG
 */
void dump_classification(const enum param_class *c, const struct typetree *t);

#endif
