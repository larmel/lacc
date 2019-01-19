#ifndef INSTR_H
#define INSTR_H

#include "abi.h"
#include <lacc/symbol.h>

/*
 * Start with %rax = 1 to make sure 0 is invalid. This is used in
 * address representation, but unfortunately crashes with instruction
 * encoding using 0b000 for AX.
 */
enum reg {
    AX  = 1,    /* 0b000 */
    CX  = 2,    /* 0b001 */
    DX  = 3,    /* 0b010 */
    BX  = 4,    /* 0b011 */
    SP  = 5,    /* 0b100 */
    BP  = 6,    /* 0b101 */
    SI  = 7,    /* 0b110 */
    DI  = 8,    /* 0b111 */
    R8  = 9,
    R9  = 10,
    R10 = 11,
    R11 = 12,
    R12 = 13,
    R13 = 14,
    R14 = 15,
    R15 = 16,

    /*
     * Floating point registers. REX prefix needed for XMM8 through 15,
     * which are index 24 (11000) through 31 (0b11111).
     */
    XMM0, XMM1,  XMM2,  XMM3,  XMM4,  XMM5,  XMM6,  XMM7,
    XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15,

    /* Instruction pointer. */
    IP,

    /* X87 extended precision floating point registers. */
    ST0,
    ST1,
    ST2,
    ST3,
    ST4,
    ST5,
    ST6,
    ST7
};

/*
 */
INTERNAL int x87_stack_pos(enum reg r);

/* Register and width, which is 1, 2, 4 or 8. */
struct registr {
    int width;
    enum reg r;
};

/*
 * Full addressing is displacement(%base, %index, scale), producing the
 * address %base + (%index * scale) + displacement.
 *
 * Displacement can be relative to symbol.
 *
 * foo+3(%rip)
 *     Address of foo[3].
 *
 * global@GOTPCREL(%rip)
 *     Address of address of global, found in global offset table.
 *
 * foo@PLT
 *     Function address through trampoline in procedure linkage table.
 *
 */
struct address {
    enum {
        ADDR_NORMAL,
        ADDR_GLOBAL_OFFSET,
        ADDR_PLT
    } type;

    const struct symbol *sym;
    int displacement;

    enum reg base;
    enum reg index;
    unsigned scale;
};

/* Memory location; address and width. */
struct memory {
    int width;
    struct address addr;
};

/*
 * Immediates can be numeric (fit in registers), or references to static
 * string values. Expressions like "hello" + 1 can result in for ex
 * .LC1+1 in GNU assembly.
 */
struct immediate {
    int width;
    enum {
        IMM_INT,    /* 1, 2, 4 or 8 byte signed number. */
        IMM_ADDR,   /* Symbol-relative address, label etc. */
        IMM_STRING  /* string value, only used for initialization. */
    } type;
    union {
        char byte;
        short word;
        int dword;
        long qword;
        struct address addr;
        String string;
    } d;
};

enum prefix {
    PREFIX_NONE = 0x0,
    PREFIX_REP = 0xF3,
    PREFIX_REPE = 0xF3,
    PREFIX_REPNE = 0xF2
};

enum opcode {
    INSTR_ADD,
    INSTR_ADDS,         /* Add floating point. */
    INSTR_CVTSI2S,      /* Convert int to floating point. */
    INSTR_CVTS2S,       /* Convert between float and double. */
    INSTR_CVTTS2SI,     /* Convert floating point to int with truncation. */
    INSTR_Cxy,          /* Sign extend %[e/r]ax to %[e|r]dx:%[e|r]ax. */
    INSTR_SUB,
    INSTR_SUBS,         /* Subtract floating point. */
    INSTR_NOT,
    INSTR_XOR,
    INSTR_DIV,
    INSTR_IDIV,         /* Signed division. */
    INSTR_DIVS,
    INSTR_AND,
    INSTR_OR,
    INSTR_SHL,
    INSTR_SHR,
    INSTR_SAR,
    INSTR_TEST,
    INSTR_MOV,
    INSTR_MOVZX,
    INSTR_MOVSX,
    INSTR_MOVAP,        /* Move aligned packed floating point. */
    INSTR_MOVS,         /* Move floating point. */
    INSTR_MUL,
    INSTR_MULS,         /* Multiply floating point. */
    INSTR_SETcc,        /* Set flag (combined with tttn). */
    INSTR_UCOMIS,       /* Compare floating point and set EFLAGS. */
    INSTR_CMP,
    INSTR_LEA,
    INSTR_PUSH,
    INSTR_POP,
    INSTR_PXOR,         /* Bitwise xor with xmm register. */
    INSTR_JMP,
    INSTR_Jcc,          /* Jump on condition (combined with tttn) */
    INSTR_CALL,
    INSTR_LEAVE,
    INSTR_RET,
    INSTR_MOV_STR,      /* Move string, optionally with REP prefix. */
    INSTR_FLD,          /* Load x87 real to ST(0). */
    INSTR_FILD,         /* Load integer to ST(0). */
    INSTR_FISTP,        /* Store integer and pop. */
    INSTR_FSTP,         /* Store x87 real from ST(0) to mem and pop. */
    INSTR_FUCOMIP,      /* Compare x87 floating point. */
    INSTR_FXCH,         /* Swap ST(0) with ST(i). */
    INSTR_FNSTCW,       /* Store x87 FPU control word. */
    INSTR_FLDCW,        /* Load x87 FPU control word. */
    INSTR_FADDP,        /* Add x87 ST(0) to ST(i) and pop. */
    INSTR_FSUBRP,       /* Subtract and pop. */
    INSTR_FMULP,        /* Multiply and pop. */
    INSTR_FDIVRP        /* Divide and pop. */
};

/*
 * Conditional test/jump codes. A nice reference is
 * http://unixwiz.net/techtips/x86-jumps.html
 */
enum tttn {
    CC_O = 0x0,
    CC_NO = 0x1,
    CC_NAE = 0x2,
    CC_AE = 0x3,
    CC_E = 0x4,
    CC_NE = 0x5,
    CC_NA = 0x6,
    CC_A = 0x7,
    CC_S = 0x8,
    CC_NS = 0x9,
    CC_P = 0xA,
    CC_NP = 0xB,
    CC_NGE = 0xC,
    CC_GE = 0xD,
    CC_NG = 0xE,
    CC_G = 0xF
};

/* Instructions with register, memory or immediate operands. */
struct instruction {
    enum opcode opcode;
    enum tttn cc;
    enum prefix prefix;
    enum instr_optype {
        OPT_NONE = 0,
        OPT_IMM = 1,
        OPT_REG = 2,
        OPT_MEM = 4,
        OPT_REG_REG = 8,
        OPT_REG_MEM = 16,
        OPT_MEM_REG = 32,
        OPT_IMM_REG = 64,
        OPT_IMM_MEM = 128
    } optype;
    union operand {
        int width;
        struct registr reg;
        struct memory mem;
        struct immediate imm;
    } source, dest;
};

/*
 * According to Intel reference manual, instructions can contain the
 * following fields, for a combined maximum length of 18 bytes:
 *
 *  [Legacy Prefixes] [REX] [Opcode] [ModR/M] [SIB] [Displ] [Immediate]
 *   (up to 4 bytes)   (1)    (3)      (1)     (1)    (4)       (4)
 *
 */
struct code {
    unsigned char val[18];
    int len;
};

/* Convert instruction to binary format. */
INTERNAL struct code encode(struct instruction instr);

#endif
