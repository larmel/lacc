#include "instructions.h"

#include <assert.h>

/* Map register enum values to register encoding. Depends on register
 * enumeration values.
 */
#define reg(arg) ((arg).r - 1)
#define is_64_bit(arg) ((arg).w >> 3)
#define is_64_bit_reg(arg) ((arg).r > DI)

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
        if (a.imm.type == IMM_DWORD) {
            assert(b.reg.w == 4);
            c.val[0] = 0xB8 + reg(b.reg);
            memcpy(&c.val[1], &a.imm.d.dword, 4);
            c.len = 5;
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
        if (is_64_bit(a.reg)) {
            /*c.val[0] = REX | W(a.reg) | R(a.reg);*/
            /* todo */
        } else {
            c.val[0] = 0x88 + w(a.reg);
            c.len = 1 + encode_address(&c.val[1], a.reg.r, b.mem.addr);
        }
        break;
    case OPT_MEM_REG:
        if (is_64_bit(a.reg)) {
            /*c.val[0] = REX | W(a.reg) | R(a.reg);*/
            /* todo */
        } else {
            c.val[0] = 0x8A + w(b.reg);
            c.len = 1 + encode_address(&c.val[1], b.reg.r, a.mem.addr);
        }
        break;
    default:
        break;
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

struct code encode(struct instruction instr)
{
    switch (instr.opcode) {
    case INSTR_ADD:
        return add(instr.optype, instr.source, instr.dest);
    case INSTR_MOV:
        return mov(instr.optype, instr.source, instr.dest);
    case INSTR_PUSH:
        return push(instr.optype, instr.source);
    case INSTR_SUB:
        return sub(instr.optype, instr.source, instr.dest);
    case INSTR_LEAVE:
        return leave();
    case INSTR_RET:
        return ret();
    default:
        return nop();
    }
}
