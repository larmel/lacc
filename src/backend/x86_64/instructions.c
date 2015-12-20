#include "instructions.h"

#include <assert.h>

/* Map register enum values to register encoding. Depends on register
 * enumeration values.
 */
#define reg(arg) ((arg).r - 1)
#define is_64_bit(arg) ((arg).w >> 3)
#define is_32_bit(arg) (((arg).w >> 2) & 1)
#define is_16_bit(arg) (((arg).w >> 1) & 1)
#define is_64_bit_reg(arg) ((arg).r > DI)

/* Legacy Prefix
 * Valid options are 0x66, 0x67, 0xF2 and 0xF3
 */

/* REX prefix contains bits [0, 1, 0, 0, W, R, X, B]
 * W: 1 if operands are 64 bit. 
 * R: Extension of ModRM reg field (most significant bit)
 * X: Extension of SIB index field
 * B: Extension of ModRM r/m field, SIB base field, or Opcode reg field
 */
#define REX 0x40
#define W(arg) (is_64_bit(arg) << 3)
#define R(arg) (is_64_bit_reg(arg) << 2)
#define X(arg) 0
#define B(arg) is_64_bit_reg(arg)

/* Operand size bit, 0 for 8 bit operand and 1 for 32 bit operand, when default
 * is 32 bit. [Table B-6]
 */
#define w(arg) (~(arg).w & 1)

/* ModR/M    [   mod,    reg,     r/m    ]
 *              2 bit   3 bit    3 bit
 *
 * mod: combines with the r/m field to form 32 possible values: eight
 *      registers and 24 addressing modes.
 * reg: specifies either a register number or three more bits of opcode
 *      information.
 * r/m: specify a register as an operand or it can be combined with the mod
 *      field to encode an addressing mode.
 */

/* SIB       [   scale,   index,   base   ]
 *                2bit     3bit    3bit
 *
 * scale: specifies the scale factor.
 * index: specifies the register number of the index register.
 * base: specifies the register number of the base register.
 */

/* Determine if integer value can be encoded with certain width.
 */
#define in_byte_range(arg) ((arg) >= -128 && (arg) <= 127)
#define in_32bit_range(arg) ((arg) >= -2147483648 && (arg) <= 2147483647)

/* Conditional test field.
 */
enum tttn {
    TEST_AE = 0x3,
    TEST_Z = 0x4,
    TEST_A = 0x7,
    TEST_GE = 0xD,
    TEST_G = 0xF
};

/* Encode address using ModR/M, SIB and Displacement bytes. Based on Table 2.2
 * and Table 2.3 in reference manual.
 *
 * Only using ModR/M for now.
 */
static int encode_address(unsigned char *code, enum reg r, struct address addr)
{
    /* Discard most significant bit, which is encoded in REX prefix. */
    code[0]  = ((r - 1) & 0x7) << 3;
    code[0] |= (addr.base - 1) & 0x7;

    if (addr.disp) {
        if (in_byte_range(addr.disp)) {
            code[0] |= 0x40;
            code[1] = addr.disp;
            return 2;
        }
        code[0] |= 0x80;
        memcpy(&code[1], &addr.disp, 4);
        return 5;
    }

    return 1;
}

static int is_byte_imm(struct immediate imm)
{
    return imm.type == IMM_BYTE ||
        (imm.type == IMM_WORD && in_byte_range(imm.d.word)) ||
        (imm.type == IMM_DWORD && in_byte_range(imm.d.dword)) ||
        (imm.type == IMM_QUAD && in_byte_range(imm.d.quad));
}

static int is_32bit_imm(struct immediate imm)
{
    return imm.type == IMM_BYTE ||
        imm.type == IMM_WORD ||
        imm.type == IMM_DWORD ||
        (imm.type == IMM_QUAD && in_32bit_range(imm.d.quad));
}

static struct code nop(void)
{
    struct code c = {{0x90}, 1};
    return c;
}

static struct code mov(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = nop();
    switch (optype) {
    case OPT_IMM_REG:
        /* Alternative encoding (shorter). */
        c.len = 0;
        if (is_64_bit(b.reg))
            c.val[c.len++] = REX | W(b.reg) | B(b.reg);
        c.val[c.len++] = 0xB8 | w(b.reg) << 3 | reg(b.reg);
        if (a.imm.type == IMM_BYTE) {
            c.val[c.len++] = a.imm.d.byte;
        } else if (a.imm.type == IMM_WORD) {
            memcpy(&c.val[c.len], &a.imm.d.word, 2);
            c.len += 2;
        } else if (is_32bit_imm(a.imm)) {
            if (is_64_bit(b.reg)) {
                /* Special case, not using alternative encoding. */
                c.val[1] = 0xC7;
                c.val[2] = 0xC0 | reg(b.reg);
                c.len = 3;
            }
            memcpy(&c.val[c.len], &a.imm.d.dword, 4);
            c.len += 4;
        } else if (a.imm.type == IMM_QUAD) {
            memcpy(&c.val[c.len], &a.imm.d.quad, 8);
            c.len += 8;
        }
        break;
    case OPT_REG_REG:
        assert(a.reg.w == b.reg.w);
        c.len = 3;
        c.val[0] = REX | W(a.reg) | R(a.reg) | B(b.reg);
        c.val[1] = 0x88 + is_64_bit(a.reg);
        c.val[2] = 0xC0 | reg(a.reg) << 3 | reg(b.reg);
        break;
    case OPT_REG_MEM:
        c.len = 0;
        if (is_16_bit(a.reg))
            c.val[c.len++] = 0x66; /* Legacy prefix */
        else if (is_64_bit(a.reg))
            c.val[c.len++] = REX | W(a.reg) | R(a.reg);
        c.val[c.len++] = 0x88 + w(a.reg);
        c.len += encode_address(&c.val[c.len], a.reg.r, b.mem.addr);
        break;
    case OPT_MEM_REG:
        c.len = 0;
        if (is_64_bit(a.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg);
        }
        c.val[c.len++] = 0x8A + w(b.reg);
        c.len += encode_address(&c.val[c.len], b.reg.r, a.mem.addr);
        break;
    default:
        break;
    }
    return c;
}

static struct code movsx(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = nop();
    assert(optype == OPT_MEM_REG);

    c.len = 0;
    if (is_64_bit(b.reg))
        c.val[c.len++] = REX | W(b.reg) | R(b.reg);
    if (is_32_bit(a.mem) && is_64_bit(b.reg)) {
        c.val[c.len++] = 0x63;
    } else {
        c.val[c.len++] = 0x0F;
        c.val[c.len++] = 0xBE | w(a.mem);
    }
    c.len += encode_address(&c.val[c.len], b.reg.r, a.mem.addr);
    return c;
}

static struct code movzx(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};

    if (is_64_bit(b.reg))
        c.val[c.len++] = REX | W(b.reg) | R(b.reg);
    c.val[c.len++] = 0x0F;
    if (optype == OPT_REG_REG) {
        c.val[c.len++] = 0xB6 | w(a.reg);
        c.val[c.len++] = 0xC0 | reg(b.reg) << 3 | reg(a.reg);
    } else {
        assert(optype == OPT_MEM_REG);
        c.val[c.len++] = 0xB6 | w(a.mem);
        c.len += encode_address(&c.val[c.len], b.reg.r, a.mem.addr);
    }

    return c;
}

static struct code push(enum instr_optype optype, union operand op)
{
    struct code c = nop();
    if (optype == OPT_REG)
        c.val[0] = 0x50 + reg(op.reg);
    return c;
}

static struct code sub(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = nop();

    switch (optype) {
    case OPT_IMM_REG:
        assert(is_64_bit(b.reg));
        c.val[0] = REX | W(b.reg) | B(b.reg);
        c.val[1] = 0x81 | is_byte_imm(a.imm) << 1;
        c.val[2] = 0xE8 | reg(b.reg);
        if (is_byte_imm(a.imm)) {
            c.val[3] = a.imm.d.byte;
            c.len = 4;
        } else if (is_32bit_imm(a.imm)) {
            assert(a.imm.type == IMM_DWORD || a.imm.type == IMM_QUAD);
            memcpy(&c.val[3], &a.imm.d.dword, 4);
            c.len = 7;
        }
        break;
    case OPT_REG_REG:
        c.len = 0;
        if (is_64_bit(a.reg)) {
            c.val[0] = REX | W(a.reg) | R(a.reg) | B(b.reg);
            c.len = 1;
        }
        c.val[c.len++] = 0x28 | w(a.reg);
        c.val[c.len++] = 0xC0 | reg(a.reg) << 3 | reg(b.reg);
        break;
    default:
        assert(0);
        break;
    }

    return c;
}

static struct code add(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = nop();

    switch (optype) {
    case OPT_REG_REG:
        c.len = 0;
        if (is_64_bit(a.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = 0x00 | w(a.reg);
        c.val[c.len++] = 0xC0 | reg(a.reg) << 3 | reg(b.reg);
        break;
    case OPT_IMM_REG:
        break;
    case OPT_IMM_MEM:
        break;
    default:
        assert(0);
        break;
    }

    return c;
}

static struct code leave(void)
{
    struct code c = {{0xC9}, 1};
    return c;
}

static struct code ret(void)
{
    /* Only 'Near return' is used, returning to a function with address in the
     * same segment, and not popping any bytes from stack. */
    struct code c = {{0xC3}, 1};
    return c;
}

static struct code setcc(
    enum instr_optype optype,
    enum tttn cond,
    union operand op)
{
    struct code c = {{0}};
    assert(optype == OPT_REG && !is_64_bit(op.reg));

    c.val[c.len++] = 0x0F;
    c.val[c.len++] = 0x90 | cond;
    c.val[c.len++] = 0xC0 | reg(op.reg);
    return c;
}

struct code encode(struct instruction instr)
{
    switch (instr.opcode) {
    case INSTR_ADD:
        return add(instr.optype, instr.source, instr.dest);
    case INSTR_MOV:
        return mov(instr.optype, instr.source, instr.dest);
    case INSTR_MOVSX:
        return movsx(instr.optype, instr.source, instr.dest);
    case INSTR_MOVZX:
        return movzx(instr.optype, instr.source, instr.dest);
    case INSTR_PUSH:
        return push(instr.optype, instr.source);
    case INSTR_SUB:
        return sub(instr.optype, instr.source, instr.dest);
    case INSTR_LEAVE:
        return leave();
    case INSTR_RET:
        return ret();
    case INSTR_SETZ:
        return setcc(instr.optype, TEST_Z, instr.source);
    case INSTR_SETA:
        return setcc(instr.optype, TEST_A, instr.source);
    case INSTR_SETG:
        return setcc(instr.optype, TEST_G, instr.source);
    case INSTR_SETAE:
        return setcc(instr.optype, TEST_AE, instr.source);
    case INSTR_SETGE:
        return setcc(instr.optype, TEST_GE, instr.source);
    default:
        return nop();
    }
}
