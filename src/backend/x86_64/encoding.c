#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif

#include "encoding.h"
#include "elf.h"
#include <lacc/context.h>

#include <assert.h>

#define PREFIX_OPERAND_SIZE 0x66

#define reg3(arg) ((arg - 1) & 0x7)
#define is_64_bit_reg(arg) (((arg) >= R8 && (arg) <= R15) \
    || ((arg) >= XMM8 && (arg) <= XMM15))

#define in_byte_range(arg) ((arg) >= -128 && (arg) <= 127)
#define in_32bit_range(arg) ((arg) >= -2147483648 && (arg) <= 2147483647)

/*
 * REX prefix contains bits [0, 1, 0, 0, W, R, X, B]
 * W: 1 if operands are 64 bit. 
 * R: Extension of ModRM reg field (most significant bit)
 * X: Extension of SIB index field
 * B: Extension of ModRM r/m field, SIB base field, or Opcode reg field
 */
#define REX 0x40
#define W(arg) ((arg == 8) << 3)
#define R(arg) (is_64_bit_reg(arg) << 2)
#define X(arg) (is_64_bit_reg((arg).index) << 1)
#define B(arg) is_64_bit_reg(arg)

/* Hack to enable REX.W on certain SSE instructions. */
#define is_general(op) (op <= INSTR_XOR)
#define enable_rex_w(op) \
    (is_general(op) || op == INSTR_CVTTS2SI || op == INSTR_CVTSI2S)

/*
 * Some additional information can be encoded in the last opcode byte:
 *
 *   w: Set to 1 if operand size should be full size (32 or 64),
 *      otherwise operand size is 1 byte. Normally least significant bit
 *      of last opcode byte, unless OPX_REG is set, in which case it is
 *      the 4th bit (after reg).
 *
 *   s: Set to 1 to sign extend immediate into operand size. Bit 2 of
 *      last opcode byte.
 *
 *   d: Direction bit, reverse mem/reg operands. Bit 2 of last opcode
 *      byte.
 *
 */
enum opextra {
    OPX_NONE = 0,
    OPX_W = 1,
    OPX_S = 2,
    OPX_SW = OPX_S | OPX_W,
    OPX_D = 4,
    OPX_DW = OPX_D | OPX_W,
    OPX_tttn = 8,
    OPX_REG = 16,
    OPX_WREG = OPX_W | OPX_REG
};

/*
 * Some instructions have fixed value of one operand, determined by the
 * opcode.
 *
 * Example: Shift with register->register only works with %cl as source,
 *          thus 'shl %cl, %rax' does not encode %cl explicitly.
 */
enum implicit {
    IMPL_NONE = 0,
    IMPL_CX = 1,
    IMPL_AX = 2
};

static struct encoding {
    enum opcode opc;

    struct {
        const char *str;
        unsigned int suffix : 1;
    } mnemonic;

    /*
     * Prefixes are separated into four groups, and there can be only
     * one prefix from each group, in any order.
     *
     *   Group 1: 0xF0, 0xF2, 0xF3
     *   Group 2: 0x2E, 0x36, 0x3E, 0x26, 0x64, 0x65, 0x2E, 0x3E
     *   Group 3: 0x66 (Operand size override, set 16 bit)
     *   Group 4: 0x67 (Address size override)
     *
     */
    unsigned char prefix[4];

    /* There can be up to 3 opcode bytes. */
    unsigned char opcode[3];

    /* Optionally extra stuff in opcode byte. */
    unsigned char opextra;

    /* Base pattern for ModR/M byte. */
    unsigned char modrm;

    unsigned char optype;

    /* Restrictions and encoding information for each operand. */
    struct {
        unsigned int widths : 4;
        unsigned int implicit : 4;
    } openc[2];

    /* Some instructions have their operands reversed. */ 
    unsigned int reverse : 1;

    /*
     * Mark that immediate operand is a relative displacement, and not a
     * direct address. Used for example in jump instructions.
     *
     * Also used to restrict encoding of integer constants that can fit
     * in 32 bit, for instructions that implicitly extend to 64 bit.
     */
    unsigned int is_displacement_or_dword : 1;
} encodings[] = {
    {INSTR_ADD, {"add", 1}, {0}, {0x00}, OPX_DW, 0x00, OPT_REG_REG | OPT_MEM_REG | OPT_REG_MEM},
    {INSTR_ADD, {"add", 1}, {0}, {0x80}, OPX_SW, 0x00, OPT_IMM_REG | OPT_IMM_MEM, {0}, 0, 1},

    {INSTR_AND, {"and"}, {0}, {0x20}, OPX_DW, 0x00, OPT_REG_REG | OPT_MEM_REG | OPT_REG_MEM},
    {INSTR_AND, {"and"}, {0}, {0x24}, OPX_W, 0x00, OPT_IMM_REG, {{0}, {0, IMPL_AX}}, 0, 1},
    {INSTR_AND, {"and"}, {0}, {0x80}, OPX_SW, 0x20, OPT_IMM_REG | OPT_IMM_MEM, {0}, 0, 1},

    {INSTR_CALL, {"call"}, {0}, {0xE8}, OPX_NONE, 0x00, OPT_IMM, {8}},
    {INSTR_CALL, {"call", 1}, {0}, {0xFF}, OPX_NONE, 0x10, OPT_REG | OPT_MEM, {8}},

    {INSTR_CMP, {"cmp"}, {0}, {0x38}, OPX_DW, 0x00, OPT_REG_REG | OPT_REG_MEM | OPT_MEM_REG},
    {INSTR_CMP, {"cmp"}, {0}, {0x3C}, OPX_W, 0x00, OPT_IMM_REG, {{1 | 2 | 4}, {1 | 2 | 4, IMPL_AX}}},
    {INSTR_CMP, {"cmp"}, {0}, {0x80}, OPX_SW, 0x38, OPT_IMM_REG | OPT_IMM_MEM},

    {INSTR_Cxy, {"cdq"}, {0}, {0x99}, OPX_NONE, 0x00, OPT_NONE, {4}},
    {INSTR_Cxy, {"cqo"}, {0}, {0x99}, OPX_NONE, 0x00, OPT_NONE, {8}},

    {INSTR_DIV, {"div"}, {0}, {0xF6}, OPX_W, 0x30, OPT_REG | OPT_MEM},

    {INSTR_IDIV, {"idiv"}, {0}, {0xF6}, OPX_W, 0x38, OPT_REG | OPT_MEM},

    {INSTR_Jcc, {"j"}, {0}, {0x0F, 0x80}, OPX_tttn, 0x00, OPT_IMM, {8}, 0, 1},

    {INSTR_JMP, {"jmp"}, {0}, {0xE9}, OPX_S, 0x00, OPT_IMM, {8}, 0, 1},

    {INSTR_LEA, {"lea", 1}, {0}, {0x8D}, OPX_NONE, 0x00, OPT_MEM_REG, {{8}, {8}}},

    {INSTR_LEAVE, {"leave"}, {0}, {0xC9}, OPX_NONE, 0x00, OPT_NONE},

    {INSTR_MOV, {"mov", 1}, {0}, {0x88}, OPX_DW, 0x00, OPT_REG_REG | OPT_MEM_REG | OPT_REG_MEM},
    {INSTR_MOV, {"mov", 1}, {0}, {0xB8}, OPX_WREG, 0x00, OPT_IMM_REG, {{1 | 2 | 4}, {1 | 2 | 4}}},
    {INSTR_MOV, {"mov", 1}, {0}, {0xC6}, OPX_W, 0x00, OPT_IMM_REG, {0}, 0, 1},
    {INSTR_MOV, {"movq"}, {0}, {0xB8}, OPX_WREG, 0x00, OPT_IMM_REG, {{8}, {8}}},
    {INSTR_MOV, {"mov", 1}, {0}, {0xC6}, OPX_W, 0x00, OPT_IMM_MEM, {0}, 0, 1},

    {INSTR_MOV_STR, {"movs"}, {0}, {0xA4}, OPX_W},

    {INSTR_MOVSX, {"movslq"}, {0}, {0x63}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {8}}, 1},
    {INSTR_MOVSX, {"movs"}, {0}, {0x0F, 0xBE}, OPX_W, 0x00, OPT_REG_REG | OPT_MEM_REG, {{1 | 2}, {4 | 8}}, 1},

    {INSTR_MOVZX, {"movzlq"}, {0}, {0x0F, 0xB7}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {8}}, 1},
    {INSTR_MOVZX, {"movz"}, {0}, {0x0F, 0xB6}, OPX_W, 0x00, OPT_REG_REG | OPT_MEM_REG, {{1 | 2}, {4 | 8}}, 1},

    {INSTR_MUL, {"mul"}, {0}, {0xF6}, OPX_W, 0x20, OPT_REG | OPT_MEM},

    {INSTR_NOT, {"not"}, {0}, {0xF6}, OPX_W, 0x10, OPT_REG | OPT_MEM},

    {INSTR_OR, {"or"}, {0}, {0x08}, OPX_DW, 0x00, OPT_REG_REG | OPT_MEM_REG | OPT_REG_MEM},
    {INSTR_OR, {"or"}, {0}, {0x80}, OPX_SW, 0x08, OPT_IMM_REG | OPT_IMM_MEM, {0}, 0, 1},

    {INSTR_POP, {"pop"}, {0}, {0x58}, OPX_REG, 0x00, OPT_REG, {8}},

    {INSTR_PUSH, {"push"}, {0}, {0x50}, OPX_REG, 0x00, OPT_REG, {8}},
    {INSTR_PUSH, {"push"}, {0}, {0x68}, OPX_NONE, 0x00, OPT_IMM, {8}, 0, 1},
    {INSTR_PUSH, {"push"}, {0}, {0xFF}, OPX_NONE, 0x30, OPT_MEM, {8}},

    {INSTR_RET, {"ret"}, {0}, {0xC3}, OPX_NONE, 0x00, OPT_NONE},

    {INSTR_SAR, {"sar"}, {0}, {0xC0}, OPX_W, 0xF8, OPT_IMM_REG, {1}},
    {INSTR_SAR, {"sar"}, {0}, {0xD2}, OPX_W, 0xF8, OPT_REG_REG, {1, IMPL_CX}},

    {INSTR_SETcc, {"set"}, {0}, {0x0F, 0x90}, OPX_tttn, 0xC0, OPT_REG, {1}},

    {INSTR_SHL, {"shl"}, {0}, {0xC0}, OPX_W, 0xE0, OPT_IMM_REG, {1}},
    {INSTR_SHL, {"shl"}, {0}, {0xD2}, OPX_W, 0xE0, OPT_REG_REG, {{1, IMPL_CX}}},

    {INSTR_SHR, {"shr"}, {0}, {0xC0}, OPX_W, 0xE8, OPT_IMM_REG, {1}},
    {INSTR_SHR, {"shr"}, {0}, {0xD2}, OPX_W, 0xE8, OPT_REG_REG, {1, IMPL_CX}},

    {INSTR_SUB, {"sub"}, {0}, {0x28}, OPX_SW, 0x00, OPT_REG_REG | OPT_MEM_REG | OPT_REG_MEM},
    {INSTR_SUB, {"sub"}, {0}, {0x80}, OPX_SW, 0x28, OPT_IMM_REG | OPT_IMM_MEM, {0}, 0, 1},

    {INSTR_TEST, {"test"}, {0}, {0x84}, OPX_W, 0x00, OPT_REG_REG | OPT_MEM_REG | OPT_REG_MEM},
    {INSTR_TEST, {"test"}, {0}, {0xF6}, OPX_W, 0xC0, OPT_IMM_REG},

    {INSTR_XOR, {"xor"}, {0}, {0x30}, OPX_DW, 0x00, OPT_REG_REG | OPT_MEM_REG | OPT_REG_MEM},
    {INSTR_XOR, {"xor"}, {0}, {0x80}, OPX_SW, 0xF0, OPT_IMM_REG | OPT_IMM_MEM, {0}, 0, 1},

    /* SSE */

    {INSTR_ADDS, {"addss"}, {0xF3}, {0x0F, 0x58}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},
    {INSTR_ADDS, {"addsd"}, {0xF2}, {0x0F, 0x58}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{8}, {8}}, 1},

    {0, {"andnps"}, {0}, {0x0F, 0x55}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},

    {0, {"andps"}, {0}, {0x0F, 0x54}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},

    {0, {"cvtps2dq"}, {0x66}, {0x0F, 0x5B}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},

    {0, {"cvtdq2ps"}, {0}, {0x0F, 0x5B}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},

    {INSTR_CVTSI2S, {"cvtsi2ss"}, {0xF3}, {0x0F, 0x2A}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4 | 8}, {4}}, 1},
    {INSTR_CVTSI2S, {"cvtsi2sd"}, {0xF2}, {0x0F, 0x2A}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4 | 8}, {8}}, 1},

    {INSTR_CVTS2S, {"cvtss2sd"}, {0xF3}, {0x0F, 0x5A}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {8}}, 1},
    {INSTR_CVTS2S, {"cvtsd2ss"}, {0xF2}, {0x0F, 0x5A}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{8}, {4}}, 1},

    {INSTR_CVTTS2SI, {"cvttss2si"}, {0xF3}, {0x0F, 0x2C}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4 | 8}}, 1},
    {INSTR_CVTTS2SI, {"cvttsd2si"}, {0xF2}, {0x0F, 0x2C}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{8}, {4 | 8}}, 1},

    {INSTR_DIVS, {"divss"}, {0xF3}, {0x0F, 0x5E}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},
    {INSTR_DIVS, {"divsd"}, {0xF2}, {0x0F, 0x5E}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{8}, {8}}, 1},

    {INSTR_MULS, {"mulss"}, {0xF3}, {0x0F, 0x59}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},
    {INSTR_MULS, {"mulsd"}, {0xF2}, {0x0F, 0x59}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{8}, {8}}, 1},

    {0, {"orps"}, {0}, {0x0F, 0x56}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},

    {INSTR_SUBS, {"subss"}, {0xF3}, {0x0F, 0x5C}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},
    {INSTR_SUBS, {"subsd"}, {0xF2}, {0x0F, 0x5C}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{8}, {8}}, 1},

    {INSTR_MOVAP, {"movaps"}, {0}, {0x0F, 0x28}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},
    {INSTR_MOVAP, {"movaps"}, {0}, {0x0F, 0x29}, OPX_NONE, 0x00, OPT_REG_MEM, {{4}, {4}}},

    {INSTR_MOVS, {"movss"}, {0xF3}, {0x0F, 0x10}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},
    {INSTR_MOVS, {"movss"}, {0xF3}, {0x0F, 0x11}, OPX_NONE, 0x00, OPT_REG_MEM, {{4}, {4}}},
    {INSTR_MOVS, {"movsd"}, {0xF2}, {0x0F, 0x10}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{8}, {8}}, 1},
    {INSTR_MOVS, {"movsd"}, {0xF2}, {0x0F, 0x11}, OPX_NONE, 0x00, OPT_REG_MEM, {{8}, {8}}},

    {0, {"movups"}, {0}, {0x0F, 0x10}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},
    {0, {"movups"}, {0}, {0x0F, 0x11}, OPX_NONE, 0x00, OPT_REG_MEM, {{4}, {4}}},

    {INSTR_UCOMIS, {"ucomiss"}, {0}, {0x0F, 0x2E}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{4}, {4}}, 1},
    {INSTR_UCOMIS, {"ucomisd"}, {0x66}, {0x0F, 0x2E}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{8}, {8}}, 1},

    {INSTR_PXOR, {"pxor"}, {0x66}, {0x0F, 0xEF}, OPX_NONE, 0x00, OPT_REG_REG | OPT_MEM_REG, {{8}, {8}}, 1},

    /* x87 */ 

    {INSTR_FADDP, {"faddp"}, {0}, {0xD8 | 6}, OPX_NONE, 0x00, OPT_REG},

    {INSTR_FDIVRP, {"fdivrp"}, {0}, {0xD8 | 6}, OPX_NONE, 0x38, OPT_REG},

    {INSTR_FILD, {"filds"}, {0}, {0xD8 | 7}, OPX_NONE, 0x00, OPT_MEM, {{2}, {2}}},
    {INSTR_FILD, {"fildl"}, {0}, {0xD8 | 3}, OPX_NONE, 0x00, OPT_MEM, {{4}, {4}}},
    {INSTR_FILD, {"fildq"}, {0}, {0xD8 | 7}, OPX_NONE, 0x28, OPT_MEM, {{8}, {8}}},

    {INSTR_FISTP, {"fistps"}, {0}, {0xD8 | 7}, OPX_NONE, 0x18, OPT_MEM, {{2}, {2}}},
    {INSTR_FISTP, {"fistpl"}, {0}, {0xD8 | 3}, OPX_NONE, 0x18, OPT_MEM, {{4}, {4}}},
    {INSTR_FISTP, {"fistpq"}, {0}, {0xD8 | 7}, OPX_NONE, 0x38, OPT_MEM, {{8}, {8}}},

    {INSTR_FLD, {"flds"}, {0}, {0xD8 | 1}, OPX_NONE, 0x00, OPT_MEM, {{4}, {4}}},
    {INSTR_FLD, {"fldl"}, {0}, {0xD8 | 5}, OPX_NONE, 0x00, OPT_MEM, {{8}, {8}}},
    {INSTR_FLD, {"fldt"}, {0}, {0xD8 | 3}, OPX_NONE, 0x28, OPT_MEM}, /* 80 bit */
    {INSTR_FLD, {"fld"}, {0}, {0xD8 | 1}, OPX_NONE, 0x00, OPT_REG},

    {INSTR_FLDZ, {"fldz"}, {0}, {0xD8 | 1, 0xEE}, OPX_NONE, 0x00, OPT_NONE},

    {INSTR_FLDCW, {"fldcw"}, {0}, {0xD8 | 1}, OPX_NONE, 0x28, OPT_MEM},

    {INSTR_FMULP, {"fmulp"}, {0}, {0xD8 | 6}, OPX_NONE, 0x08, OPT_REG},

    {INSTR_FNSTCW, {"fnstcw"}, {0}, {0xD8 | 1}, OPX_NONE, 0x38, OPT_MEM},

    {INSTR_FSTP, {"fstps"}, {0}, {0xD8 | 1}, OPX_NONE, 0x18, OPT_MEM, {{4}, {8}}},
    {INSTR_FSTP, {"fstpl"}, {0}, {0xD8 | 5}, OPX_NONE, 0x18, OPT_MEM, {{8}, {8}}},
    {INSTR_FSTP, {"fstpt"}, {0}, {0xD8 | 3}, OPX_NONE, 0x38, OPT_MEM}, /* 80 bit */
    {INSTR_FSTP, {"fstp"}, {0}, {0xD8 | 5}, OPX_NONE, 0x18, OPT_REG},

    {INSTR_FSUBRP, {"fsubrp"}, {0}, {0xD8 | 6}, OPX_NONE, 0x28, OPT_REG},

    {INSTR_FUCOMIP, {"fucomip"}, {0}, {0xD8 | 7}, OPX_NONE, 0x28, OPT_REG},

    {INSTR_FXCH, {"fxch"}, {0}, {0xD8 | 1}, OPX_NONE, 0x08, OPT_REG},
    {0},
};

static int encode_modrm_sib_disp(
    struct code *c,
    unsigned int reg,
    struct address addr)
{
    int has_sib, has_displacement;
    int len = c->len;

    assert(!addr.sym);
    assert(addr.base || !addr.displacement);

    /* Scale is 2 bits, representing 1, 2, 4 or 8. */
    assert(addr.scale == 0
        || addr.scale == 1
        || addr.scale == 2
        || addr.scale == 4
        || addr.scale == 8);

    /* SP is used as sentinel for SIB, and R12 overlaps. */
    has_sib = addr.index
        || !addr.base || reg3(addr.base) == reg3(SP) || addr.scale > 1;

    /* Explicit displacement must be used with BP or R13. */
    has_displacement = !addr.base
        || addr.displacement || reg3(addr.base) == reg3(BP);

    /* ModR/M */
    c->val[c->len++] = ((reg & 0x7) << 3) | (has_sib ? 4 : reg3(addr.base));
    if (!in_byte_range(addr.displacement)) {
        c->val[c->len - 1] |= 0x80;
    } else if (has_displacement && addr.base) {
        c->val[c->len - 1] |= 0x40;
    }

    /* SIB */
    if (has_sib) {
        c->val[c->len] = (addr.index) ? reg3(addr.index) : reg3(SP);
        c->val[c->len] = c->val[c->len] << 3;
        c->val[c->len] |= (
            addr.scale == 2 ? 1 :
            addr.scale == 4 ? 2 :
            addr.scale == 8 ? 3 : 0) << 6;
        c->val[c->len] |= addr.base ? reg3(addr.base) : 5;
        c->len++;
    }

    /* Displacement */
    if (!in_byte_range(addr.displacement) || !addr.base) {
        memcpy(&c->val[c->len], &addr.displacement, 4);
        c->len += 4;
    } else if (has_displacement) {
        c->val[c->len++] = addr.displacement;
    }

    return c->len - len;
}

/*
 * Encode address using ModR/M, SIB and Displacement bytes. Based on
 * Table 2.2 and Table 2.3 in reference manual. Symbol references are
 * encoded as %rip- relative addresses, section 2.2.1.6.
 *
 * Addend is to account for additional bytes in the instruction after
 * relocation offset is written.
 */
static int encode_address(
    struct code *c,
    unsigned int reg,
    struct address addr,
    int addend)
{
    enum rel_type reloc;

    if (addr.sym) {
        c->val[c->len++] = ((reg & 0x7) << 3) | 0x5;
        if (addr.type == ADDR_GLOBAL_OFFSET) {
            reloc = R_X86_64_GOTPCREL;
        } else {
            assert(addr.type == ADDR_NORMAL);
            reloc = R_X86_64_PC32;
        }

        elf_add_relocation(section.rela_text,
            addr.sym, reloc, c->len, addr.displacement - addend);
        memset(&c->val[c->len], 0, 4);
        c->len += 4;
        return 5;
    } else {
        return encode_modrm_sib_disp(c, reg, addr);
    }
}

/*
 * In 64-bit mode, all instructions have default operand size of 32 bits
 * except branches and instructions like push/pop which implicitly
 * references stack pointer.
 */
static int default_operand_size(enum opcode opc)
{
    switch (opc) {
    case INSTR_CALL:
    case INSTR_PUSH:
    case INSTR_POP:
    case INSTR_JMP:
    case INSTR_Jcc:
        return 8;
    default:
        return 4;
    }
}

static void encode_opcode(struct code *c, struct encoding enc)
{
    int i;

    c->val[c->len++] = enc.opcode[0];
    for (i = 1; enc.opcode[i] && i < 2; ++i) {
        c->val[c->len++] = enc.opcode[i];
    }
}

static void encode_reg_reg(
    struct code *c,
    struct encoding enc,
    int ws,
    int w,
    enum reg a,
    enum reg b)
{
    unsigned char rex;

    rex = REX | B(b);

    if (enable_rex_w(enc.opc) && w != default_operand_size(enc.opc)) {
        rex = rex | W(w);
    }

    assert(!enc.openc[1].implicit);
    if (!enc.openc[0].implicit) {
        rex |= R(a);
    }

    if (rex != REX) {
        c->val[c->len++] = rex;
    }

    encode_opcode(c, enc);
    if ((enc.opextra & OPX_W) == OPX_W) {
        if (enc.openc[0].implicit) {
            ws = w;
        }
        c->val[c->len - 1] |= ws != 1;
    }

    c->val[c->len++] = 0xC0 | enc.modrm | reg3(b);
    if (!enc.openc[0].implicit) {
        assert(!enc.modrm);
        c->val[c->len - 1] |= reg3(a) << 3;
    }
}

static void encode_mem_reg(
    struct code *c,
    struct encoding enc,
    int w,
    int d,
    struct memory mem,
    enum reg b)
{
    unsigned char rex;

    rex = REX | R(b) | X(mem.addr) | B(mem.addr.base);
    if (enable_rex_w(enc.opc) && w != default_operand_size(enc.opc)) {
        rex = rex | W(w);
    }

    assert(!enc.openc[1].implicit);
    if (rex != REX || ((b == SI || b == DI) && w == 1)) {
        c->val[c->len++] = rex;
    }

    encode_opcode(c, enc);

    /* Specify full width in last opcode bit. */
    if ((enc.opextra & OPX_W) == OPX_W) {
        c->val[c->len - 1] |= mem.width != 1;
    }

    /* Direction bit, determine source and destination. */
    if ((enc.opextra & OPX_D) == OPX_D) {
        c->val[c->len - 1] |= (d == 1) << 1;
    }

    encode_address(c, reg3(b), mem.addr, 0);
}

static int encode_immediate(
    struct code *c,
    struct immediate imm,
    int sx_byte_offset,
    int is_displacement_or_dword)
{
    int disp, len;
    struct address addr;
    enum rel_type reloc;

    len = c->len;
    switch (imm.type) {
    case IMM_INT:
        if (imm.width == 1 || (sx_byte_offset && in_byte_range(imm.d.qword))) {
            if (sx_byte_offset) {
                c->val[c->len - sx_byte_offset] |= 0x02;
            }
            c->val[c->len++] = imm.d.byte;
        } else if (imm.width == 2) {
            memcpy(c->val + c->len, &imm.d.word, 2);
            c->len += 2;
        } else if (imm.width == 4 || is_displacement_or_dword) {
            assert(imm.width == 4 || in_32bit_range(imm.d.qword));
            memcpy(c->val + c->len, &imm.d.dword, 4);
            c->len += 4;
        } else {
            assert(imm.width == 8);
            memcpy(c->val + c->len, &imm.d.qword, 8);
            c->len += 8;
        }
        break;
    case IMM_ADDR:
        addr = imm.d.addr;
        assert(addr.sym);
        if (is_displacement_or_dword) {
            assert(addr.type == ADDR_NORMAL);
            disp = elf_text_displacement(addr.sym, c->len) + addr.displacement - 4;
            memcpy(c->val + c->len, &disp, 4);
        } else {
            assert(addr.type == ADDR_NORMAL || addr.type == ADDR_PLT);
            reloc = addr.type == ADDR_NORMAL ? R_X86_64_PC32 : R_X86_64_PLT32;
            elf_add_relocation(section.rela_text,
                addr.sym, reloc, c->len, addr.displacement);
        }
        c->len += 4;
        break;
    case IMM_STRING:
        assert(0);
        break;
    }

    return c->len - len;
}

static void encode_imm_reg(
    struct code *c,
    struct encoding enc,
    int w,
    struct immediate a,
    enum reg b)
{
    int d;
    unsigned char rex;

    rex = REX | W(w) | B(b);
    if (rex != REX || ((b == SI || b == DI) && w == 1)) {
        c->val[c->len++] = rex;
    }

    encode_opcode(c, enc);
    if ((enc.opextra & OPX_W) == OPX_W) {
        w = w != 1;
        if ((enc.opextra & OPX_REG) == OPX_REG) {
            w <<= 3;
        }

        c->val[c->len - 1] |= w;
    }

    if ((enc.opextra & OPX_REG) == OPX_REG) {
        assert(!enc.modrm);
        assert(!enc.openc[1].implicit);
        c->val[c->len - 1] |= reg3(b);
        d = 1;
    } else if (!enc.openc[1].implicit) {
        c->val[c->len++] = 0xC0 | enc.modrm | reg3(b);
        d = 2;
    } else {
        d = 0;
    }

    /*
     * Set zero if not supporting 1 byte immediate, or if explicitly 1
     * byte operand.
     */
    if (((enc.opextra & OPX_S) != OPX_S) || w == 0) {
        d = 0;
    }

    encode_immediate(c, a, d, enc.is_displacement_or_dword);
}

static int imm_encoding_width(
    struct immediate imm,
    int allow_sign_extend,
    int max_32bit)
{
    switch (imm.type) {
    case IMM_INT:
        if (imm.width == 1
            || (allow_sign_extend && in_byte_range(imm.d.qword)))
        {
            return 1;
        } else if (imm.width == 2) {
            return 2;
        } else if (imm.width == 4 || max_32bit) {
            return 4;
        } else {
            return 8;
        }
        break;
    case IMM_ADDR:
        return 4;
    default:
        return 0;
    }
}

static void encode_imm_mem(
    struct code *c,
    struct encoding enc,
    int w,
    struct immediate a,
    struct address b)
{
    int d, s;
    unsigned char rex;

    rex = REX | W(w) | X(b) | B(b.base);
    if (rex != REX) {
        c->val[c->len++] = rex;
    }

    encode_opcode(c, enc);
    if ((enc.opextra & OPX_W) == OPX_W) {
        c->val[c->len - 1] |= w != 1;
    }

    /* If allowing sign extend of 8 bit data. */
    s = (enc.opextra & OPX_S) == OPX_S;

    /*
     * We need to know how many bytes will be used in this instruction
     * after the address, to compute the correct relocation relative to
     * the next instruction.
     */
    w = imm_encoding_width(a, s, enc.is_displacement_or_dword);
    d = encode_address(c, (enc.modrm >> 3) & 0x7, b, w) + 1;

    /*
     * d is number of bytes back to opcode byte to set sign extend bit.
     * Set zero if not supporting 1 byte immediate.
     */
    if (!s) {
        d = 0;
    }

    d = encode_immediate(c, a, d, enc.is_displacement_or_dword);
    assert(d == w);
}

static void encode_none(struct code *c, struct encoding enc, int w)
{
    unsigned char rex;

    rex = REX;
    if (enable_rex_w(enc.opc) && w != default_operand_size(enc.opc)) {
        rex = rex | W(w);
        if (rex != REX) {
            c->val[c->len++] = rex;
        }
    }

    encode_opcode(c, enc);
    if ((enc.opextra & OPX_W) == OPX_W) {
        c->val[c->len - 1] |= w != 1;
    }
}

static void encode_reg(
    struct code *c,
    struct encoding enc,
    int w,
    enum reg r,
    enum tttn cc)
{
    unsigned char rex;

    rex = REX | B(r);
    if (enable_rex_w(enc.opc) && w != default_operand_size(enc.opc)) {
        rex = rex | W(w);
    }

    if (rex != REX) {
        c->val[c->len++] = rex;
    }

    encode_opcode(c, enc);
    if ((enc.opextra & OPX_W) == OPX_W) {
        w = w != 1;
        if ((enc.opextra & OPX_REG) == OPX_REG) {
            w <<= 3;
        }

        c->val[c->len - 1] |= w;
    }

    if ((enc.opextra & OPX_tttn) == OPX_tttn) {
        assert(enc.opextra == OPX_tttn);
        c->val[c->len - 1] |= cc;
    }

    if ((enc.opextra & OPX_REG) == OPX_REG) {
        assert(!enc.modrm);
        c->val[c->len - 1] |= reg3(r);
    } else {
        c->val[c->len++] = 0xC0 | enc.modrm | reg3(r);
    }
}

static void encode_mem(
    struct code *c,
    struct encoding enc,
    int w,
    struct address addr)
{
    unsigned char rex;

    rex = REX | X(addr) | B(addr.base);
    if (enable_rex_w(enc.opc) && w != default_operand_size(enc.opc)) {
        rex = rex | W(w);
    }

    if (rex != REX) {
        c->val[c->len++] = rex;
    }

    encode_opcode(c, enc);
    if ((enc.opextra & OPX_W) == OPX_W) {
        c->val[c->len - 1] |= w != 1;
    }

    encode_address(c, (enc.modrm >> 3) & 0x7, addr, 0);
}

static void encode_imm(
    struct code *c,
    struct encoding enc,
    int w,
    struct immediate imm,
    enum tttn cc)
{
    unsigned char rex;

    rex = REX;
    if (enable_rex_w(enc.opc) && w != default_operand_size(enc.opc)) {
        rex = rex | W(w);
    }

    if (rex != REX) {
        c->val[c->len++] = rex;
    }

    encode_opcode(c, enc);
    if ((enc.opextra & OPX_tttn) == OPX_tttn) {
        assert(enc.opextra == OPX_tttn);
        c->val[c->len - 1] |= cc;
    }

    encode_immediate(c, imm, 1, enc.is_displacement_or_dword);
}

static int operand_size(struct instruction instr)
{
    switch (instr.optype) {
    case OPT_NONE:
    case OPT_IMM:
    case OPT_REG:
    case OPT_MEM:
        return instr.source.width;
    case OPT_REG_MEM:
    case OPT_MEM_REG:
    case OPT_REG_REG:
    case OPT_IMM_REG:
    case OPT_IMM_MEM:
        if (instr.opcode == INSTR_CVTSI2S) {
            return instr.source.width;
        }
        return instr.dest.width;
    default: assert(0);
        break;
    }

    return 4;
}

static int is_32_bit_imm(struct immediate imm)
{
    if (imm.width <= 4) return 1;
    switch (imm.type) {
    case IMM_INT:
        return in_32bit_range(imm.d.qword);
    case IMM_ADDR:
        return 1; /* displacement */
    default: return 0;
    }
}

static int match_instruction_encoding(
    const struct instruction *instr,
    const struct encoding *enc)
{
    int i;
    union operand op;

    assert(instr->opcode == enc->opc || enc->opc == 0);
    if (instr->optype != (enc->optype & instr->optype))
        return 0;

    switch (instr->optype) {
    case OPT_IMM:
    case OPT_IMM_REG:
    case OPT_IMM_MEM:
        if (enc->is_displacement_or_dword && !is_32_bit_imm(instr->source.imm))
            return 0;
    default:
        break;
    }

    for (i = 0; i < 2; ++i) {
        op = i == 0 ? instr->source : instr->dest;
        switch (enc->openc[i].implicit) {
        default: assert(0);
        case IMPL_NONE: break;
        case IMPL_AX:
            if (op.reg.r != AX) return 0;
            break;
        case IMPL_CX:
            if (op.reg.r != CX) return 0;
            break;
        }
    }

    if (enc->openc[0].widths
        && (enc->openc[0].widths & instr->source.width) == 0)
        return 0;

    if (instr->dest.width && enc->openc[1].widths
        && ((enc->openc[1].widths & instr->dest.width) == 0))
        return 0;

    return 1;
}

static struct encoding find_encoding(struct instruction instr)
{
    int i;

    i = instr.opcode;
    assert(encodings[i].opc == 0 || encodings[i].opc == instr.opcode);
    assert(instr.opcode == 0 || encodings[i - 1].opc != instr.opcode);

    do {
        if (match_instruction_encoding(&instr, encodings + i)) {
            return encodings[i];
        }
        i++;
    } while (encodings[i].opc == instr.opcode);

    error("Unsupported instruction.");
    exit(1);
}

static int is_single_width(unsigned int w)
{
    return w == 1 || w == 2 || w == 4 || w == 8 || w == 16;
}

/*
 * Lookup best matching instruction from textual assembly mnemonic.
 *
 * Also consider widths of operands if known, for example in case of
 * register operands.
 *
 *   add %eax, %ebx
 *
 * Cases that do not have unambiguous operand sizes are resolved by
 * copying the known operand width, or looking at instruction suffix.
 *
 *   add $4, %ax
 *   addw $4, (%ebp)
 *
 */
INTERNAL int mnemonic_match_operands(
    const char *mnemonic,
    size_t length,
    struct instruction *instr)
{
    int i;
    int ws, wd, sfx;
    size_t mlen;
    struct encoding *enc;
    struct instruction candidate = {0};
    unsigned int w0, w1;

    assert(length > 0);
    for (i = 0; i < sizeof(encodings) / sizeof(encodings[0]); ++i) {
        enc = &encodings[i];
        if (!enc->mnemonic.str)
            break;

        if ((enc->optype & instr->optype) != instr->optype)
            continue;

        w0 = enc->openc[0].widths;
        w1 = enc->openc[1].widths;
        ws = instr->source.width;
        wd = instr->dest.width;
        sfx = 0;

        /* lea breaks rule with inferring memory operand size. */
        if (enc->opc == INSTR_LEA) {
            ws = w0;
        }

        /* Disregard encoding if widths do not fit. */
        if ((ws && w0 && (ws & w0) == 0) || (wd && w1 && (wd & w1) == 0))
            continue;

        /* Coerce operand widths if there is only one choice. */
        if (is_single_width(w0)) ws = w0;
        if (is_single_width(w1)) wd = w1;

        /* Encoding mnemonic can lack integer width suffix. */
        mlen = strlen(enc->mnemonic.str);
        if (length != mlen || strncmp(mnemonic, enc->mnemonic.str, length)) {
            if (enc->mnemonic.suffix) switch (mnemonic[length - 1]) {
            case 'b': sfx = 1; break;
            case 'w': sfx = 2; break;
            case 'l': sfx = 4; break;
            case 'q': sfx = 8; break;
            default: continue;
            } else continue;

            if (mlen == length - 1 && !strncmp(mnemonic, enc->mnemonic.str, length - 1)) {
                /* Match found. */
            } else {
                sfx = 0;
                continue;
            }
        }

        switch (instr->optype) {
        case OPT_NONE:
            break;
        case OPT_REG:
        case OPT_MEM:
        case OPT_IMM:
            break;
        case OPT_REG_REG:
        case OPT_REG_MEM:
        case OPT_MEM_REG:
        case OPT_IMM_REG:
        case OPT_IMM_MEM:
            if (!ws) {
                if (!wd) {
                    if (!sfx) {
                        error("Unable to determine instruction operand width.");
                        exit(1);
                    }
                    ws = wd = sfx;
                } else {
                    ws = wd;
                }
            } else if (!wd) {
                wd = ws;
            }
            break;
        }

        candidate = *instr;
        candidate.opcode = enc->opc;
        candidate.source.width = ws;
        candidate.dest.width = wd;
        if (match_instruction_encoding(&candidate, enc)) {
            instr->source.width = ws;
            instr->dest.width = wd;
            instr->opcode = enc->opc ? enc->opc : i;
            return 1;
        }
    }

    return 0;
}

INTERNAL void get_mnemonic(struct instruction instr, char *buf)
{
    const char *ptr;
    unsigned int w0, w1;
    int ws, wd;
    struct encoding enc;

    /* Only integer suffixes are omitted in the encoding table. */
    static char suffix[] = {'\0', 'b', 'w', '\0', 'l', '\0', '\0', '\0', 'q'};

    /* Map condition codes to (one valid) mnemonic. */
    static char *tttn_mnemonics[] = {
        "o", "no", "nae", "ae",
        "e", "ne", "na", "a",
        "s", "ns", "p", "np",
        "nge", "ge", "ng", "g"
    };

    enc = find_encoding(instr);
    ptr = enc.mnemonic.str;

    while (*ptr) {
        *buf++ = *ptr++;
    }

    if ((enc.opextra & OPX_tttn) == OPX_tttn) {
        assert(instr.cc >= 0);
        assert(instr.cc < 16);
        ptr = tttn_mnemonics[instr.cc];
        while (*ptr) {
            *buf++ = *ptr++;
        }
    }

    w0 = enc.openc[0].widths;
    w1 = enc.openc[1].widths;
    ws = instr.source.width;
    wd = instr.dest.width;

    switch (instr.optype) {
    case OPT_REG: /* Implicitly given by register. */
        break;
    case OPT_NONE:
        if (!instr.source.width) {
            break;
        }
    case OPT_IMM:
    case OPT_MEM:
        if (!is_single_width(w0) && ws <= 8) {
            *buf++ = suffix[ws];
        }
        break;
    case OPT_REG_REG:
    case OPT_REG_MEM:
    case OPT_MEM_REG:
    case OPT_IMM_REG:
    case OPT_IMM_MEM:
        if (!is_single_width(w0) && ws <= 8) {
            *buf++ = suffix[ws];
            if (ws != wd && !is_single_width(w1) && wd <= 8) {
                *buf++ = suffix[wd];
            }
        } else if (!is_single_width(w1) && wd <= 8) {
            *buf++ = suffix[wd];
        }
        break;
    }
}

INTERNAL struct code encode(struct instruction instr)
{
    struct code c = {{0}};
    struct encoding enc;
    int i, ws, w;

    enc = find_encoding(instr);
    for (i = 0; i < 4 && enc.prefix[i]; ++i) {
        c.val[c.len++] = enc.prefix[i];
    }

    if (instr.prefix) {
        c.val[c.len++] = instr.prefix;
    }

    w = operand_size(instr);
    if (w == 2) {
        c.val[c.len++] = PREFIX_OPERAND_SIZE;
    }

    switch (instr.optype) {
    default: assert(0);
    case OPT_NONE:
        encode_none(&c, enc, w);
        break;
    case OPT_REG:
        encode_reg(&c, enc, w, instr.source.reg.r, instr.cc);
        break;
    case OPT_MEM:
        encode_mem(&c, enc, w, instr.source.mem.addr);
        break;
    case OPT_IMM:
        encode_imm(&c, enc, w, instr.source.imm, instr.cc);
        break;
    case OPT_REG_REG:
        ws = instr.source.width;
        if (enc.reverse) {
            encode_reg_reg(&c, enc, ws, w, instr.dest.reg.r, instr.source.reg.r);
        } else {
            encode_reg_reg(&c, enc, ws, w, instr.source.reg.r, instr.dest.reg.r);
        }
        break;
    case OPT_MEM_REG:
        encode_mem_reg(&c, enc, w, 1, instr.source.mem, instr.dest.reg.r);
        break;
    case OPT_REG_MEM:
        encode_mem_reg(&c, enc, w, 0, instr.dest.mem, instr.source.reg.r);
        break;
    case OPT_IMM_REG:
        encode_imm_reg(&c, enc, w, instr.source.imm, instr.dest.reg.r);
        break;
    case OPT_IMM_MEM:
        encode_imm_mem(&c, enc, w, instr.source.imm, instr.dest.mem.addr);
        break;
    }

    return c;
}
