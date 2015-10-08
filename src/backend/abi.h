#ifndef ABI_H
#define ABI_H

#include "core/symbol.h"

/* Registers in x86_64 assembly.
 */
enum reg {
    AX = 0,
    BX,
    CX,
    DX,
    BP,
    SP,
    SI,
    DI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15
};

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
enum param_class {
    PC_NO_CLASS = 0,
    PC_INTEGER,
    PC_SSE,
    PC_SSEUP,
    PC_MEMORY
};

/* Classify a type as a list of eightbytes, used for parameter passing and
 * return values. Algorithm specified in System V ABI (3.2.3), part of calling
 * convention of the x86_64 architecture.
 * Returns a list of parameter classes, allocated dynamically. Caller should
 * free memory.
 */
enum param_class *classify(const struct typetree *t);

/*
 */
enum param_class **classify_call(
    const struct typetree **args,
    const struct typetree *ret,
    int n_args,
    enum param_class **out);

/*
 */
enum param_class **classify_signature(
    const struct typetree *func,
    enum param_class **out);

/* Alignment of symbol in bytes.
 */
int sym_alignment(const struct symbol *sym);

/* DEBUG
 */
void dump_classification(const enum param_class *c, const struct typetree *t);

#endif
