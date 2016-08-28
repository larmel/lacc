#ifndef ABI_H
#define ABI_H

#include <lacc/symbol.h>

#define MAX_INTEGER_ARGS 6
#define MAX_SSE_ARGS 8
#define MAX_REGISTER_ARGS (MAX_INTEGER_ARGS + MAX_SSE_ARGS)
#define MAX_INTEGER_RET 2
#define MAX_SSE_RET 2

/*
 * Parameter class of an 8-byte slice of an object. Objects which take
 * up more than 4 eightbytes automatically get class PC_MEMORY.
 */
struct param_class {
    unsigned char eightbyte[4];
};

#define PC_NO_CLASS     0x00
#define PC_INTEGER      0x01
#define PC_SSE          0x02
#define PC_SSEUP        0x04
#define PC_X87          0x08
#define PC_X87UP        0x10
#define PC_COMPLEX_X87  0x20
#define PC_MEMORY       0x40

/*
 * Calculate how many eightbytes is necessary to pass an object of the
 * given type as a parameter or return value.
 */
#define EIGHTBYTES(t) ((size_of(t) + 7) / 8)

/*
 * Parameter classification as described in System V ABI (3.2.3), with
 * some simplifications. Classify parameter as a series of eightbytes
 * used for parameter passing and return value.
 */
struct param_class classify(const struct typetree *type);

/* Alignment of symbol in bytes. */
int sym_alignment(const struct symbol *sym);

#endif
