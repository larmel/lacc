#ifndef ENCODING_H
#define ENCODING_H

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

/*
 * Opcodes map to first entry in encoding table.
 */
enum opcode {
    INSTR_ADD = 0,
    INSTR_AND = INSTR_ADD + 2,
    INSTR_CALL = INSTR_AND + 3,
    INSTR_CMP = INSTR_CALL + 2,
    INSTR_Cxy = INSTR_CMP + 3,          /* Sign extend %[e/r]ax to %[e|r]dx:%[e|r]ax. */
    INSTR_DIV = INSTR_Cxy + 2,
    INSTR_IDIV = INSTR_DIV + 1,         /* Signed division. */
    INSTR_Jcc = INSTR_IDIV + 1,         /* Jump on condition (combined with tttn) */
    INSTR_JMP = INSTR_Jcc + 1,
    INSTR_LEA = INSTR_JMP + 1,
    INSTR_LEAVE = INSTR_LEA + 1,
    INSTR_MOV = INSTR_LEAVE + 1,
    INSTR_MOV_STR = INSTR_MOV + 5,      /* Move string, optionally with REP prefix. */
    INSTR_MOVSX = INSTR_MOV_STR + 1,
    INSTR_MOVZX = INSTR_MOVSX + 2,
    INSTR_MUL = INSTR_MOVZX + 2,
    INSTR_NOT = INSTR_MUL + 1,
    INSTR_OR = INSTR_NOT + 1,
    INSTR_POP = INSTR_OR + 2,
    INSTR_PUSH = INSTR_POP + 1,
    INSTR_RET = INSTR_PUSH + 3,
    INSTR_SAR = INSTR_RET + 1,
    INSTR_SETcc = INSTR_SAR + 2,        /* Set flag (combined with tttn). */
    INSTR_SHL = INSTR_SETcc + 1,
    INSTR_SHR = INSTR_SHL + 2,
    INSTR_SUB = INSTR_SHR + 2,
    INSTR_TEST = INSTR_SUB + 2,
    INSTR_XOR = INSTR_TEST + 2,

    INSTR_ADDS = INSTR_XOR + 2,         /* Add floating point. */
    INSTR_CVTSI2S = INSTR_ADDS + 6,     /* Convert int to floating point. */
    INSTR_CVTS2S = INSTR_CVTSI2S + 2,   /* Convert between float and double. */
    INSTR_CVTTS2SI = INSTR_CVTS2S + 2,  /* Convert floating point to int with truncation. */
    INSTR_DIVS = INSTR_CVTTS2SI + 2,
    INSTR_MULS = INSTR_DIVS + 2,        /* Multiply floating point. */
    INSTR_SUBS = INSTR_MULS + 3,        /* Subtract floating point. */
    INSTR_MOVAP = INSTR_SUBS + 2,       /* Move aligned packed floating point. */
    INSTR_MOVS = INSTR_MOVAP + 2,       /* Move floating point. */
    INSTR_UCOMIS = INSTR_MOVS + 6,      /* Compare floating point and set EFLAGS. */
    INSTR_PXOR = INSTR_UCOMIS + 2,      /* Bitwise xor with xmm register. */

    INSTR_FADDP = INSTR_PXOR + 1,       /* Add x87 ST(0) to ST(i) and pop. */
    INSTR_FDIVRP = INSTR_FADDP + 1,     /* Divide and pop. */
    INSTR_FILD = INSTR_FDIVRP + 1,      /* Load integer to ST(0). */
    INSTR_FISTP = INSTR_FILD + 3,       /* Store integer and pop. */
    INSTR_FLD = INSTR_FISTP + 3,        /* Load x87 real to ST(0). */
    INSTR_FLDZ = INSTR_FLD + 4,         /* Push +0.0. */
    INSTR_FLDCW = INSTR_FLDZ + 1,       /* Load x87 FPU control word. */
    INSTR_FMULP = INSTR_FLDCW + 1,      /* Multiply and pop. */
    INSTR_FNSTCW = INSTR_FMULP + 1,     /* Store x87 FPU control word. */
    INSTR_FSTP = INSTR_FNSTCW + 1,      /* Store x87 real from ST(0) to mem and pop. */
    INSTR_FSUBRP = INSTR_FSTP + 4,      /* Subtract and pop. */
    INSTR_FUCOMIP = INSTR_FSUBRP + 1,   /* Compare x87 floating point. */
    INSTR_FXCH = INSTR_FUCOMIP + 1      /* Swap ST(0) with ST(i). */
};

enum prefix {
    PREFIX_NONE = 0x0,
    PREFIX_REP = 0xF3,
    PREFIX_REPE = 0xF3,
    PREFIX_REPNE = 0xF2
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
 * following fields:
 *
 *  [Legacy Prefixes] [REX] [Opcode] [ModR/M] [SIB] [Displ] [Immediate]
 *   (up to 4 bytes)   (1)    (3)      (1)     (1)    (4)       (4)
 *
 *
 * At most 15 bytes can be used for one instruction.
 */
struct code {
    unsigned char val[15];
    signed char len;
};

/* Convert abstract instruction to binary. */
INTERNAL struct code encode(struct instruction instr);

/* Lookup instruction mnemonic for textual assembly. */
INTERNAL void get_mnemonic(struct instruction instr, char *buf);

/*
 * Lookup best matching instruction from textual assembly mnemonic and
 * operands.
 */
INTERNAL int mnemonic_match_operands(
    const char *mnemonic,
    size_t length,
    struct instruction *instr);

#endif
