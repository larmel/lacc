#ifndef ABI_H
#define ABI_H

#include <lacc/array.h>
#include <lacc/symbol.h>

/* Start with %rax = 1 to make sure 0 is invalid. This is used in
 * address representation, but unfortunately crashes with instruction
 * encoding using 0b000 for AX.
 */
enum reg {
    AX  = 1, /* 0b000 */
    CX  = 2, /* 0b001 */
    DX  = 3, /* 0b010 */
    BX  = 4, /* 0b011 */
    SP  = 5, /* 0b100 */
    BP  = 6, /* 0b101 */
    SI  = 7, /* 0b110 */
    DI  = 8, /* 0b111 */
    R8  = 9,
    R9  = 10,
    R10 = 11,
    R11 = 12,
    R12 = 13,
    R13 = 14,
    R14 = 15,
    R15 = 16,

    /* Instruction pointer. */
    IP,

    /* Floating point registers. */
    XMM0, XMM1,  XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15
};

/* Registers used for passing INTEGER parameters.
 */
extern enum reg param_int_reg[6];

/* Registers used for returning INTEGER retult.
 */
extern enum reg ret_int_reg[2];

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

typedef array_of(enum param_class) ParamClass;

/* Parameter classification as described in System V ABI (3.2.3), with
 * some simplifications. Classify parameter as a series of eightbytes
 * used for parameter passing and return value.
 */
void classify(const struct typetree *t, ParamClass *pc);

/* Classify function call, computing parameter class of arguments and
 * return value.
 */
void classify_call(
    const struct typetree **args,
    const struct typetree *ret,
    unsigned n_args,
    ParamClass *params,
    ParamClass *res);

/* Alignment of symbol in bytes.
 */
int sym_alignment(const struct symbol *sym);

/* DEBUG
 */
void dump_classification(const enum param_class *c, const struct typetree *t);

#endif
