#ifndef INSTR_H
#define INSTR_H

#include "abi.h"
#include <lacc/symbol.h>

/* Register and width, which is 1, 2, 4 or 8.
 */
struct registr {
    enum reg r;
    int w;
};

/* Full addressing is disp(base register, offset register, scalar
 * multiplier). Displacement can be relative to symbol, for example
 * foo+3(%rip)
 */
struct address {
    const struct symbol *sym;
    int disp;
    enum reg base;
    enum reg offset;
    int mult;
};

/* Memory location; address and width.
 */
struct memory {
    struct address addr;
    int w;
};

/* Immediates can be numeric (fit in registers), or references to static
 * string values. Expressions like "hello" + 1 can result in for ex
 * $.LC1+1 in GNU assembly.
 */
struct immediate {
    enum {
        IMM_INT,    /* 1, 2, 4 or 8 byte signed number */
        IMM_ADDR,   /* Symbol-relative address, label etc */
        IMM_STRING  /* string value, only used for initialization */
    } type;
    int w;
    union {
        char byte;
        short word;
        int dword;
        long qword;
        struct address addr;
        struct string string;
    } d;
};

enum opcode {
    INSTR_ADD,
    INSTR_SUB,
    INSTR_NOT,
    INSTR_MUL,
    INSTR_XOR,
    INSTR_DIV,
    INSTR_AND,
    INSTR_OR,
    INSTR_SHL,
    INSTR_SHR,
    INSTR_SAR,
    INSTR_TEST,
    INSTR_MOV,
    INSTR_MOVZX,
    INSTR_MOVSX,
    INSTR_MOVAPS,
    INSTR_SETZ,
    INSTR_SETA,
    INSTR_SETG,
    INSTR_SETAE,
    INSTR_SETGE,
    INSTR_CMP,
    INSTR_LEA,
    INSTR_PUSH,
    INSTR_JMP,
    INSTR_JA,
    INSTR_JG,
    INSTR_JZ,
    INSTR_JAE,
    INSTR_JGE,
    INSTR_CALL,
    INSTR_LEAVE,
    INSTR_RET,
    INSTR_REP_MOVSQ
};

/* Instructions with register, memory or immediate operands.
 */
struct instruction {
    enum opcode opcode;
    enum instr_optype {
        OPT_NONE = 0,
        OPT_IMM,
        OPT_REG,
        OPT_MEM,
        OPT_REG_REG,
        OPT_REG_MEM,
        OPT_MEM_REG,
        OPT_IMM_REG,
        OPT_IMM_MEM
    } optype;
    union operand {
        struct registr reg;
        struct memory mem;
        struct immediate imm;
    } source, dest;
};

/* According to Intel reference manual, instructions can contain the following
 * fields, for a combined maximum length of 18 bytes:
 *
 *  [Legacy Prefixes] [REX] [Opcode] [ModR/M] [SIB] [Displacement] [Immediate]
 *   (up to 4 bytes)   (1)    (3)      (1)     (1)       (4)           (4)
 *
 */
struct code {
    unsigned char val[18];
    int len;
};

/* Convert instruction to binary format.
 */
struct code encode(struct instruction instr);

#endif
