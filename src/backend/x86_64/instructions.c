#include "instructions.h"

#include <assert.h>

/* Map register enum values to register encoding. Depends on register
 * enumeration values.
 */
#define reg(arg) ((arg).r - 1)
#define is_64_bit_reg(arg) ((arg).r > DI)

/* REX prefix contains bits [0, 1, 0, 0, W, R, X, B]
 * W: 1 if operands are 64 bit. 
 * R: Extension of ModRM reg field (most significant bit)
 * X: Extension of SIB index field
 * B: Extension of ModRM r/m field, SIB base field, or Opcode reg field
 */
#define REX 0x40
#define R(arg) (is_64_bit_reg(arg) << 3)
#define B(arg) is_64_bit_reg(arg)

/* w is a bit used to separate between 8 bit and 16/32 bit in the opcode, set
 * value to 1 iff width is 1 byte.
 */
#define w(arg) ((arg).w & 1)

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
        c.val[0] = REX | R(a.reg) | B(b.reg);
        c.val[2] = 0xC0 | reg(a.reg) << 3 | reg(b.reg);
        if (a.reg.w == 8) {
            c.val[0] |= (1 << 3);
            c.val[1] = 0x89;
        } else
            c.val[1] = 0x88 | w(a.reg);
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
    case INSTR_MOV:
        return mov(instr.optype, instr.source, instr.dest);
    case INSTR_PUSH:
        return push(instr.optype, instr.source);
    case INSTR_LEAVE:
        return leave();
    case INSTR_RET:
        return ret();
    default:
        return nop();
    }
}
