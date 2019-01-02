#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "elf.h"

#include <assert.h>

/*
 * Map register enum values to register encoding. Depends on register
 * enumeration values.
 */
#define reg3(arg) ((arg - 1) & 0x7)
#define regi(arg) (((arg).r - 1) % 8)
#define is_64_bit(arg) ((arg).width >> 3)
#define is_32_bit(arg) (((arg).width >> 2) & 1)
#define is_16_bit(arg) (((arg).width >> 1) & 1)
#define is_8_bit(arg) ((arg).width & 1)
#define is_64_bit_reg(arg) (((arg) >= R8 && (arg) <= R15) \
    || ((arg) >= XMM8 && (arg) <= XMM15))
#define is_sse_reg(arg) ((arg).r >= XMM0 && (arg).r <= XMM8)

/* Determine if register or memory argument requires REX prefix. */
#define rrex(arg) \
    ((is_64_bit(arg) && (arg).r < XMM0) \
        || is_64_bit_reg(arg.r) \
        || (arg.width == 1 && (arg.r == DI || arg.r == SI)))
#define mrex(arg) \
    (!arg.sym && ((is_64_bit_reg(arg.base) || is_64_bit_reg(arg.index))))

#define PREFIX_SSE 0x0F
#define PREFIX_OPERAND_SIZE 0x66
#define PREFIX_X87 0xD8

/*
 * REX prefix contains bits [0, 1, 0, 0, W, R, X, B]
 * W: 1 if operands are 64 bit. 
 * R: Extension of ModRM reg field (most significant bit)
 * X: Extension of SIB index field
 * B: Extension of ModRM r/m field, SIB base field, or Opcode reg field
 */
#define REX 0x40
#define W(arg) (is_64_bit(arg) << 3)
#define R(arg) (is_64_bit_reg((arg).r) << 2)
#define X(arg) (is_64_bit_reg((arg).index) << 1)
#define B(arg) is_64_bit_reg((arg).r)

/*
 * Operand size bit, 0 for 8 bit operand and 1 for 32 bit operand, when
 * default is 32 bit. [Table B-6]
 */
#define w(arg) (~(arg).width & 1)

/*
 * ModR/M    [   mod,    reg,     r/m    ]
 *              2 bit   3 bit    3 bit
 *
 * mod: combines with the r/m field to form 32 possible values: eight
 *      registers and 24 addressing modes.
 * reg: specifies either a register number or three more bits of opcode
 *      information.
 * r/m: specify a register as an operand or it can be combined with the
 *      mod field to encode an addressing mode.
 */

/*
 * SIB       [   scale,   index,   base   ]
 *                2bit     3bit    3bit
 *
 * scale: specifies the scale factor.
 * index: specifies the register number of the index register.
 * base: specifies the register number of the base register.
 */

/*
 * Determine if integer value can be encoded with certain width.
 */
#define in_byte_range(arg) ((arg) >= -128 && (arg) <= 127)
#define in_32bit_range(arg) ((arg) >= -2147483648 && (arg) <= 2147483647)

/*
 * X87 registers are commonly referred to with ST(i) syntax, where ST(0)
 * represents top of stack. Compute based on current stack position.
 */
#define ST(reg) (x87_stack_pos(reg.r))

/*
 * Conditional test/jump codes. A nice reference is
 * http://unixwiz.net/techtips/x86-jumps.html
 */
enum tttn {
    TEST_NAE = 0x2,
    TEST_AE = 0x3,
    TEST_E = 0x4,
    TEST_NE = 0x5,
    TEST_NA = 0x6,
    TEST_A = 0x7,
    TEST_S = 0x8,
    TEST_NS = 0x9,
    TEST_P = 0xA,
    TEST_NP = 0xB,
    TEST_NGE = 0xC,
    TEST_GE = 0xD,
    TEST_NG = 0xE,
    TEST_G = 0xF
};

/*
 * Encode address using ModR/M, SIB and Displacement bytes. Based on
 * Table 2.2 and Table 2.3 in reference manual. Symbol references are
 * encoded as %rip- relative addresses, section 2.2.1.6.
 *
 * Addend is to account for additional bytes in the instruction after
 * relocation offset is written.
 */
static void encode_addr(
    struct code *c,
    unsigned int reg,
    struct address addr,
    int addend)
{
    enum rel_type reloc;
    int has_sib, has_displacement;

    /* Scale is 2 bits, representing 1, 2, 4 or 8. */
    assert(addr.scale == 0
        || addr.scale == 1
        || addr.scale == 2
        || addr.scale == 4
        || addr.scale == 8);

    if (addr.sym) {
        c->val[c->len++] = ((reg & 0x7) << 3) | 0x5;
        if (addr.type == ADDR_GLOBAL_OFFSET) {
            reloc = R_X86_64_GOTPCREL;
        } else {
            assert(addr.type == ADDR_NORMAL);
            reloc = R_X86_64_PC32;
        }
        elf_add_reloc_text(addr.sym, reloc, c->len, addr.displacement - addend);
        memset(&c->val[c->len], 0, 4);
        c->len += 4;
    } else {
        assert(addr.base || !addr.displacement);

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
    }
}

static int is_byte_imm(struct immediate imm)
{
    return imm.type == IMM_INT && (imm.width == 1 || 
        (imm.width == 2 && in_byte_range(imm.d.word)) ||
        (imm.width == 4 && in_byte_range(imm.d.dword)) ||
        (in_byte_range(imm.d.qword)));
}

static int is_32bit_imm(struct immediate imm)
{
    return imm.type == IMM_INT && (imm.width < 8 || in_32bit_range(imm.d.qword));
}

static struct code mov(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {0};
    struct address addr;

    assert(a.width == b.width);
    switch (optype) {
    case OPT_IMM_REG:
        /* Alternative encoding (shorter). */
        if (rrex(b.reg)) {
            c.val[c.len++] = REX | W(b.reg) | B(b.reg);
        }
        c.val[c.len++] = 0xB8 | w(b.reg) << 3 | regi(b.reg);
        if (a.width == 1) {
            assert(a.imm.type == IMM_INT);
            c.val[c.len++] = a.imm.d.byte;
        } else if (a.width == 2) {
            assert(a.imm.type == IMM_INT);
            memcpy(&c.val[c.len], &a.imm.d.word, 2);
            c.len += 2;
        } else if (is_32bit_imm(a.imm) || a.imm.type == IMM_ADDR) {
            if (is_64_bit(b.reg)) {
                /* Special case, not using alternative encoding. */
                c.val[1] = 0xC7;
                c.val[2] = 0xC0 | regi(b.reg);
                c.len = 3;
            }
            if (a.imm.type == IMM_INT) {
                memcpy(&c.val[c.len], &a.imm.d.dword, 4);
                c.len += 4;
            } else {
                assert(a.imm.type == IMM_ADDR);
                assert(a.imm.d.addr.type == ADDR_NORMAL);
                addr = a.imm.d.addr;
                elf_add_reloc_text(
                    addr.sym, R_X86_64_PC32, c.len, addr.displacement);
                memset(&c.val[c.len], 0, 4);
                c.len += 4;
            }
        } else {
            assert(a.width == 8);
            assert(a.imm.type == IMM_INT);
            memcpy(&c.val[c.len], &a.imm.d.qword, 8);
            c.len += 8;
        }
        break;
    case OPT_REG_REG:
        if (rrex(a.reg) || rrex(b.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = 0x88 | w(a.reg);
        c.val[c.len++] = 0xC0 | regi(a.reg) << 3 | regi(b.reg);
        break;
    case OPT_REG_MEM:
        if (is_16_bit(a.reg)) {
            c.val[c.len++] = 0x66; /* Legacy prefix */
        }
        if (rrex(a.reg) || mrex(b.mem.addr)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | mrex(b.mem.addr);
        }
        c.val[c.len++] = 0x88 | w(a.reg);
        encode_addr(&c, regi(a.reg), b.mem.addr, 0);
        break;
    case OPT_MEM_REG:
        if (rrex(b.reg) || mrex(a.mem.addr)) {
            c.val[c.len++] = REX | W(b.reg) | R(b.reg) | mrex(a.mem.addr);
        }
        c.val[c.len++] = 0x8A + w(b.reg);
        encode_addr(&c, regi(b.reg), a.mem.addr, 0);
        break;
    case OPT_IMM_MEM:
        assert(a.imm.type == IMM_INT);
        assert(is_32bit_imm(a.imm));
        if (b.width == 2) {
            c.val[c.len++] = PREFIX_OPERAND_SIZE;
        }
        if (a.width == 8 || mrex(b.mem.addr)) {
            c.val[c.len++] = REX | W(a.imm) | mrex(b.mem.addr);
        }
        c.val[c.len++] = 0xC6 | w(b.mem);
        if (b.width == 1) {
            encode_addr(&c, 0, b.mem.addr, 1);
            c.val[c.len++] = a.imm.d.byte;
        } else if (b.width == 2) {
            encode_addr(&c, 0, b.mem.addr, 2);
            memcpy(&c.val[c.len], &a.imm.d.word, 2);
            c.len += 2;
        } else {
            encode_addr(&c, 0, b.mem.addr, 4);
            memcpy(&c.val[c.len], &a.imm.d.dword, 4);
            c.len += 4;
        }
        break;
    default: assert(0);
    }

    return c;
}

static struct code movsx(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {0};

    switch (optype) {
    default: assert(0);
    case OPT_REG_REG:
        c.val[c.len++] = REX | W(b.reg) | R(b.reg) | B(a.reg);
        if (a.width == 4) {
            c.val[c.len++] = 0x63;
            c.val[c.len++] = 0xC0 | regi(b.reg) << 3 | regi(a.reg);
        } else {
            assert(a.width == 1 || a.width == 2);
            c.val[c.len++] = 0x0F;
            c.val[c.len++] = 0xBE | w(a.reg);
            c.val[c.len++] = 0xC0 | regi(b.reg) << 3 | regi(a.reg);
        }
        break;
    case OPT_MEM_REG:
        if (rrex(b.reg) || mrex(a.mem.addr)) {
            c.val[c.len] = REX | W(b.reg) | R(b.reg);
            c.val[c.len++] |= is_64_bit_reg(a.mem.addr.base);
        }
        if (is_32_bit(a.mem) && is_64_bit(b.reg)) {
            c.val[c.len++] = 0x63;
        } else {
            c.val[c.len++] = 0x0F;
            c.val[c.len++] = 0xBE | w(a.mem);
        }
        encode_addr(&c, regi(b.reg), a.mem.addr, 0);
        break;
    }

    return c;
}

static struct code movzx(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {0};

    if (optype == OPT_REG_REG) {
        if (rrex(a.reg) || rrex(b.reg)) {
            c.val[c.len++] = REX | W(b.reg) | R(b.reg) | B(a.reg);
        }
        c.val[c.len++] = 0x0F;
        c.val[c.len++] = 0xB6 | w(a.reg);
        c.val[c.len++] = 0xC0 | regi(b.reg) << 3 | regi(a.reg);
    } else if (optype == OPT_MEM_REG) {
        if (mrex(a.mem.addr) || rrex(b.reg)) {
            c.val[c.len] = REX | W(b.reg) | R(b.reg);
            c.val[c.len++] |= is_64_bit_reg(a.mem.addr.base);
        }
        c.val[c.len++] = 0x0F;
        c.val[c.len++] = 0xB6 | w(a.mem);
        encode_addr(&c, regi(b.reg), a.mem.addr, 0);
    } else
        assert(0);

    return c;
}

static struct code encode_push(enum instr_optype optype, union operand op)
{
    struct code c = {0};

    switch (optype) {
    case OPT_REG:
        assert(op.width == 8);
        if (is_64_bit_reg(op.reg.r)) {
            c.val[c.len++] = REX | B(op.reg);
        }
        c.val[c.len++] = 0x50 | R(op.reg) | regi(op.reg);
        break;
    case OPT_IMM:
        assert(op.width == 8);
        if (is_byte_imm(op.imm)) {
            c.val[c.len++] = 0x6A;
            c.val[c.len++] = op.imm.d.byte;
        } else if (is_32bit_imm(op.imm)) {
            c.val[c.len++] = 0x68;
            memcpy(&c.val[c.len], &op.imm.d.dword, 4);
            c.len += 4;
        } else assert(0);
        break;
    case OPT_MEM:
        assert(op.width == 8);
        if (mrex(op.mem.addr)) {
            c.val[c.len++] = REX  | W(op.mem);
        }
        c.val[c.len++] = 0xFF;
        encode_addr(&c, 0x6, op.mem.addr, 0);
        break;
    default: assert(0);
    }

    return c;
}

static struct code encode_pop(enum instr_optype optype, union operand op)
{
    struct code c = {0};
    assert(optype == OPT_REG);
    assert(op.width == 8);

    if (is_64_bit_reg(op.reg.r)) {
        c.val[c.len++] = REX | B(op.reg);
    }

    c.val[c.len++] = 0x58 | W(op.reg) | regi(op.reg);
    return c;
}

static struct code encode_sub(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};

    switch (optype) {
    case OPT_IMM_REG:
        assert(is_64_bit(b.reg));
        c.val[c.len++] = REX | W(b.reg) | B(b.reg);
        c.val[c.len++] = 0x81 | is_byte_imm(a.imm) << 1;
        c.val[c.len++] = 0xE8 | regi(b.reg);
        if (is_byte_imm(a.imm)) {
            c.val[c.len++] = a.imm.d.byte;
        } else if (is_32bit_imm(a.imm)) {
            memcpy(&c.val[c.len], &a.imm.d.dword, 4);
            c.len += 4;
        } else assert(0);
        break;
    case OPT_REG_REG:
        if (rrex(a.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = 0x28 | w(a.reg);
        c.val[c.len++] = 0xC0 | regi(a.reg) << 3 | regi(b.reg);
        break;
    default: assert(0);
    }

    return c;
}

static struct code encode_add(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {0};

    switch (optype) {
    default: assert(0);
    case OPT_REG_REG:
        if (rrex(a.reg) || rrex(b.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = 0x00 | w(a.reg);
        c.val[c.len++] = 0xC0 | regi(a.reg) << 3 | regi(b.reg);
        break;
    case OPT_IMM_REG:
        assert(a.imm.type == IMM_INT);
        if (rrex(b.reg))
            c.val[c.len++] = REX | W(b.reg) | B(b.reg);
        c.val[c.len++] = 0x80 | is_byte_imm(a.imm) << 1 | w(b.reg);
        c.val[c.len++] = 0xC0 | regi(b.reg);
        if (is_byte_imm(a.imm)) {
            c.val[c.len++] = a.imm.d.byte;
        } else {
            assert(is_32bit_imm(a.imm));
            memcpy(&c.val[c.len], &a.imm.d.dword, 4);
            c.len += 4;
        }
        break;
    case OPT_IMM_MEM:
        assert(a.imm.type == IMM_INT);
        assert(!mrex(b.mem.addr));
        if (b.width > 4) {
            c.val[c.len++] = REX | W(b.mem);
        }
        c.val[c.len++] = 0x80 | is_byte_imm(a.imm) << 1 | w(b.mem);
        if (is_byte_imm(a.imm)) {
            encode_addr(&c, 0, b.mem.addr, 1);
            c.val[c.len++] = a.imm.d.byte;
        } else {
            assert(is_32bit_imm(a.imm));
            encode_addr(&c, 0, b.mem.addr, 4);
            memcpy(&c.val[c.len], &a.imm.d.dword, 4);
            c.len += 4;
        }
        break;
    case OPT_REG_MEM:
        assert(!mrex(b.mem.addr));
        if (rrex(a.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg);
        }
        c.val[c.len++] = 0x00 | is_64_bit_reg(a.reg.r) << 1 | w(b.mem);
        encode_addr(&c, regi(a.reg), b.mem.addr, 0);
        break;
    case OPT_MEM_REG:
        assert(a.width == b.width);
        if (rrex(b.reg) || mrex(a.mem.addr)) {
            c.val[c.len++] = REX | W(b.reg) | R(b.reg) | mrex(a.mem.addr);
        }
        c.val[c.len++] = 0x00;
        if (!is_64_bit_reg(b.reg.r)) {
            c.val[c.len - 1] |= 2 | w(b.reg);
        }
        encode_addr(&c, regi(b.reg), a.mem.addr, 0);
        break;
    }

    return c;
}

static struct code encode_call(enum instr_optype optype, union operand op)
{
    enum rel_type reloc;
    struct address addr;
    struct code c = {{0}};

    if (optype == OPT_IMM) {
        assert(op.imm.type == IMM_ADDR);
        assert(op.imm.d.addr.sym);
        c.val[c.len++] = 0xE8;
        switch (op.imm.d.addr.type) {
        case ADDR_PLT:
            reloc = R_X86_64_PLT32;
            break;
        default: assert(0);
        case ADDR_NORMAL:
            reloc = R_X86_64_PC32;
            break;
        }
        addr = op.imm.d.addr;
        elf_add_reloc_text(addr.sym, reloc, c.len, addr.displacement);
        c.len += 4;
    } else {
        assert(optype == OPT_REG);
        assert(is_64_bit_reg(op.reg.r));
        c.val[c.len++] = REX | w(op.reg);
        c.val[c.len++] = 0xFF;
        c.val[c.len++] = 0xD0 | regi(op.reg);
    }

    return c;
}

static struct code cmp(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {0};

    switch (optype) {
    case OPT_IMM_REG:
        if (rrex(b.reg)) {
            c.val[c.len++] = REX | W(a.imm) | B(b.reg);
        } else if (is_16_bit(b.reg)) {
            c.val[c.len++] = PREFIX_OPERAND_SIZE;
        } else if (is_8_bit(b.reg)) {
            assert(b.reg.r == AX);
            assert(is_byte_imm(a.imm));
            c.val[c.len++] = 0x3C;
            c.val[c.len++] = a.imm.d.byte;
            break;
        }
        c.val[c.len++] = 0x80 | w(b.reg) | (is_byte_imm(a.imm) << 1);
        c.val[c.len++] = 0xF8 | regi(b.reg);
        if (is_byte_imm(a.imm)) {
            /* Sign extend bit is set. */
            c.val[c.len++] = a.imm.d.byte;
        } else {
            assert(a.width == 4);
            memcpy(&c.val[c.len], &a.imm.d.dword, 4);
            c.len += 4;
        }
        break;
    case OPT_REG_REG:
        assert(a.width == b.width);
        if (rrex(a.reg) || rrex(b.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = 0x38 | w(a.reg);
        c.val[c.len++] = 0xC0 | regi(a.reg) << 3 | regi(b.reg);
        break;
    case OPT_IMM_MEM:
        assert(!mrex(b.mem.addr));
        c.val[c.len++] = 0x80 | is_byte_imm(a.imm) << 1 | w(b.mem);
        if (is_byte_imm(a.imm)) {
            encode_addr(&c, 0x7, b.mem.addr, 1);
            c.val[c.len++] = a.imm.d.byte;
        } else {
            assert(is_32bit_imm(a.imm));
            encode_addr(&c, 0x7, b.mem.addr, 4);
            memcpy(&c.val[c.len], &a.imm.d.dword, 4);
            c.len += 4;
        }
        break;
    case OPT_MEM_REG:
        if (mrex(a.mem.addr) || rrex(b.reg)) {
            c.val[c.len++] = REX | W(b.reg) | R(b.reg) | mrex(a.mem.addr);
        }
        c.val[c.len++] = 0x3A | w(b.reg);
        encode_addr(&c, regi(b.reg), a.mem.addr, 0);
        break;
    case OPT_REG_MEM:
        if (mrex(b.mem.addr) || rrex(a.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | mrex(b.mem.addr);
        }
        c.val[c.len++] = 0x38 | w(a.reg);
        encode_addr(&c, regi(a.reg), b.mem.addr, 0);
        break;
    default: assert(0);
    }

    return c;
}

static struct code jcc(
    enum instr_optype optype,
    enum tttn cond,
    union operand op)
{
    int disp, *ptr;
    struct code c = {{0x0F, 0x80}, 2};
    const struct address *addr = &op.imm.d.addr;

    assert(optype == OPT_IMM);
    assert(addr->sym);

    c.val[1] |= cond;

    /*
     * Existing value will be added to offset. Subtract 4 to account for
     * instruction length, offset is counted after the immediate.
     */
    disp = elf_text_displacement(addr->sym, c.len) + addr->displacement - 4;
    ptr = (int *) (c.val + c.len);
    *ptr = disp;
    c.len += 4;
    return c;
}

static struct code jmp(enum instr_optype optype, union operand op)
{
    int disp, *ptr;
    struct code c = {{0xE9}, 1};
    const struct address *addr = &op.imm.d.addr;

    assert(optype == OPT_IMM);
    assert(addr->sym);

    disp = elf_text_displacement(addr->sym, c.len) + addr->displacement - 4;
    ptr = (int *) (c.val + c.len);
    *ptr = disp;
    c.len += 4;
    return c;
}

static struct code leave(void)
{
    struct code c = {{0xC9}, 1};
    return c;
}

static struct code lea(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {0};
    assert(optype == OPT_MEM_REG);
    assert(is_64_bit(b.reg));

    c.val[c.len++] = REX | W(b.reg) | R(b.reg) | X(a.mem.addr);
    c.val[c.len++] = 0x8D;
    encode_addr(&c, regi(b.reg), a.mem.addr, 0);
    return c;
}

static struct code rep_movsq(void)
{
    struct code c = {{0xF3, REX + 8, 0xA5}, 3};
    return c;
}

/*
 * Only 'near return' is used, returning to a function with address in
 * the same segment, and not popping any bytes from stack.
 */
static struct code ret(void)
{
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
    c.val[c.len++] = 0xC0 | regi(op.reg);
    return c;
}

static struct code encode_test(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};
    assert(optype == OPT_REG_REG && !is_64_bit_reg(a.reg.r));

    if (rrex(a.reg) || rrex(b.reg)) {
        c.val[c.len++] = REX | W(a.reg) | R(a.reg) | B(b.reg);
    }
    c.val[c.len++] = 0x84 | w(a.reg);
    c.val[c.len++] = 0xC0 | regi(a.reg) << 3 | regi(b.reg);
    return c;
}

static struct code encode_not(enum instr_optype optype, union operand op)
{
    struct code c = {{0}};
    assert(optype == OPT_REG);

    if (is_64_bit_reg(op.reg.r) || op.width > 4)
        c.val[c.len++] = REX | W(op.reg) | B(op.reg);
    c.val[c.len++] = 0xF6 | w(op.reg);
    c.val[c.len++] = 0xD0 | regi(op.reg);
    return c;
}

static struct code encode_mul(enum instr_optype optype, union operand op)
{
    struct code c = {{0}};

    if (optype == OPT_REG) {
        if (is_64_bit_reg(op.reg.r) || op.width > 4)
            c.val[c.len++] = REX | W(op.reg) | B(op.reg);
        c.val[c.len++] = 0xF6 | w(op.reg);
        c.val[c.len++] = 0xE0 | regi(op.reg);
    } else {
        assert(optype == OPT_MEM);
        if (op.width > 4) {
            c.val[c.len++] = REX | W(op.mem) | is_64_bit_reg(op.mem.addr.base);
        }
        c.val[c.len++] = 0xF6 | w(op.mem);
        encode_addr(&c, 0x4, op.mem.addr, 0);
    }

    return c;
}

static struct code encode_div(enum instr_optype optype, union operand op)
{
    struct code c = {{0}};

    if (optype == OPT_REG) {
        if (rrex(op.reg)) {
            c.val[c.len++] = REX | W(op.reg) | B(op.reg);
        }
        c.val[c.len++] = 0xF6 | w(op.reg);
        c.val[c.len++] = 0xF0 | regi(op.reg);
    } else {
        assert(optype == OPT_MEM);
        if (mrex(op.mem.addr)) {
            c.val[c.len++] = REX | W(op.mem) | is_64_bit_reg(op.mem.addr.base);
        }
        c.val[c.len++] = 0xF6 | w(op.mem);
        encode_addr(&c, 0x6, op.mem.addr, 0);
    }

    return c;
}

static struct code encode_signed_div(enum instr_optype optype, union operand op)
{
    struct code c = {{0}};

    if (optype == OPT_REG) {
        if (rrex(op.reg)) {
            c.val[c.len++] = REX | W(op.reg) | B(op.reg);
        }
        c.val[c.len++] = 0xF6 | w(op.reg);
        c.val[c.len++] = 0xF8 | regi(op.reg);
    } else {
        assert(optype == OPT_MEM);
        if (mrex(op.mem.addr)) {
            c.val[c.len++] = REX | W(op.mem) | is_64_bit_reg(op.mem.addr.base);
        }
        c.val[c.len++] = 0xF6 | w(op.mem);
        encode_addr(&c, 0x7, op.mem.addr, 0);
    }

    return c;
}

/*
 * Common encoding for bitwise and, or, and xor operations. Use compact
 * representation for byte sized immediate and AX operands.
 */
static struct code encode_bitwise(
    enum instr_optype optype,
    union operand a,
    union operand b,
    unsigned char opcode)
{
    struct code c = {0};
    int s; /* sign-extend */

    switch (optype) {
    default: assert(0);
    case OPT_REG_REG:
        if (rrex(a.reg) || rrex(b.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = opcode | w(a.reg);
        c.val[c.len++] = 0xC0 | regi(a.reg) << 3 | regi(b.reg);
        break;
    case OPT_IMM_REG:
        assert(a.width <= b.width);
        assert(a.width == 1 || a.width == 2 || a.width == 4);
        if (a.width == 2) {
            c.val[c.len++] = PREFIX_OPERAND_SIZE;
        }
        if (is_64_bit_reg(b.reg.r)) {
            c.val[c.len++] = REX | B(b.reg);
        }
        if (b.reg.r == AX && (!is_byte_imm(a.imm) || b.width == 1)) {
            c.val[c.len++] = opcode | 0x04 | w(a.imm);
            memcpy(&c.val[c.len], &a.imm.d.dword, a.width);
            c.len += a.width;
        } else {
            /* Generic immediate-register. */
            s = is_byte_imm(a.imm) && b.width > 1;
            c.val[c.len++] = 0x80 | s << 1 | w(a.imm);
            c.val[c.len++] = (0xC0 + opcode) | regi(b.reg);
            if (is_byte_imm(a.imm)) {
                c.val[c.len++] = a.imm.d.byte;
            } else {
                memcpy(&c.val[c.len], &a.imm.d.dword, a.width);
                c.len += a.width;
            }
        }
        break;
    }

    return c;
}

/*
 * shl: 0x00
 * shr: 0x08
 * sar: 0x18
 */
static struct code encode_shift(
    enum instr_optype optype,
    union operand a,
    union operand b,
    unsigned char opcode)
{
    struct code c = {0};

    if (optype == OPT_REG_REG) {
        assert(a.reg.r == CX);
        assert(a.width == 1);
        if (rrex(b.reg)) {
            c.val[c.len++] = REX | W(b.reg) | B(b.reg);
        }
        c.val[c.len++] = 0xD2 | w(b.reg);
        c.val[c.len++] = (opcode + 0xE0) | regi(b.reg);
    } else {
        assert(optype == OPT_IMM_REG);
        assert(a.imm.type == IMM_INT);
        assert(is_byte_imm(a.imm));
        if (rrex(b.reg)) {
            c.val[c.len++] = REX | W(b.reg) | B(b.reg);
        }
        if (a.imm.d.byte == 1) {
            c.val[c.len++] = 0xD0 | w(b.reg);
            c.val[c.len++] = (opcode + 0xE0) | regi(b.reg);
        } else {
            c.val[c.len++] = 0xC0 | w(b.reg);
            c.val[c.len++] = (opcode + 0xE0) | regi(b.reg);
            c.val[c.len++] = a.imm.d.byte;
        }
    }

    return c;
}

static struct code movaps(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{PREFIX_SSE, 0x29}, 2};
    assert(optype == OPT_REG_MEM);
    assert(a.reg.r >= XMM0 && a.reg.r <= XMM7);

    encode_addr(&c, (a.reg.r - XMM0), b.mem.addr, 0);
    return c;
}

static struct code sse_mov(
    enum instr_optype optype,
    unsigned char opcode,
    union operand a,
    union operand b)
{
    struct code c = {{0}};

    c.val[c.len++] = opcode;
    switch (optype) {
    case OPT_REG_REG:
        if (rrex(a.reg) || rrex(b.reg)) {
            c.val[c.len++] = REX | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = PREFIX_SSE;
        c.val[c.len++] = 0x11;
        c.val[c.len++] = 0xC0 | (regi(a.reg) << 3) | regi(b.reg);
        break;
    case OPT_MEM_REG:
        if (rrex(b.reg) || mrex(a.mem.addr)) {
            c.val[c.len++] = REX | R(b.reg) | mrex(a.mem.addr);
        }
        c.val[c.len++] = PREFIX_SSE;
        c.val[c.len++] = 0x10;
        encode_addr(&c, regi(b.reg), a.mem.addr, 0);
        break;
    case OPT_REG_MEM:
        if (rrex(a.reg) || mrex(b.mem.addr)) {
            c.val[c.len++] = REX | R(a.reg) | mrex(b.mem.addr);
        }
        c.val[c.len++] = PREFIX_SSE;
        c.val[c.len++] = 0x11;
        encode_addr(&c, regi(a.reg), b.mem.addr, 0);
        break;
    default: assert(0);
    }

    return c;
}

static struct code ucomiss(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};
    assert(optype == OPT_REG_REG);

    c.val[c.len++] = PREFIX_SSE;
    c.val[c.len++] = 0x2E;
    c.val[c.len++] = 0xC0 | (regi(b.reg) << 3) | regi(a.reg);
    return c;
}

static struct code ucomisd(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};
    assert(optype == OPT_REG_REG);

    c.val[c.len++] = 0x66;
    c.val[c.len++] = PREFIX_SSE;
    c.val[c.len++] = 0x2E;
    c.val[c.len++] = 0xC0 | (regi(b.reg) << 3) | regi(a.reg);
    return c;
}

static struct code sse_enc(
    enum instr_optype optype,
    unsigned char opcode1,
    unsigned char opcode2,
    union operand a,
    union operand b,
    int is_int_load)
{
    struct code c = {0};

    c.val[c.len++] = opcode1;
    if (optype == OPT_MEM_REG) {
        if (rrex(b.reg) || mrex(a.mem.addr) || (is_int_load && a.width == 8)) {
            c.val[c.len++] = REX | W(a.mem) | R(b.reg) | mrex(a.mem.addr);
            if (!is_sse_reg(b.reg)) {
                c.val[c.len - 1] |= W(b.reg);
            }
        }
        c.val[c.len++] = PREFIX_SSE;
        c.val[c.len++] = opcode2;
        encode_addr(&c, regi(b.reg), a.mem.addr, 0);
    } else {
        assert(optype == OPT_REG_REG);
        if (rrex(a.reg) || rrex(b.reg)) {
            c.val[c.len++] = REX | W(a.reg) | W(b.reg) | B(a.reg);
        }
        c.val[c.len++] = PREFIX_SSE;
        c.val[c.len++] = opcode2;
        c.val[c.len++] = 0xC0 | (regi(b.reg) << 3) | regi(a.reg);
    }

    return c;
}

static struct code cdq(void)
{
    struct code c = {{0x99}, 1};
    return c;
}

static struct code cqo(void)
{
    struct code c = {{REX | 0x08, 0x99}, 2};
    return c;
}

static struct code pxor(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};

    assert(optype == OPT_REG_REG);
    assert(a.width == 8 || a.width == 4);
    assert(a.width == b.width);
    c.val[c.len++] = 0x66;
    c.val[c.len++] = 0x0F;
    c.val[c.len++] = 0xEF;
    c.val[c.len++] = 0xC0 | (regi(b.reg) << 3) | regi(a.reg);
    return c;
}

static struct code fucomip(union operand op)
{
    struct code c = {0};

    c.val[c.len++] = PREFIX_X87 | 0x07;
    c.val[c.len++] = 0xE8 | ST(op.reg);
    return c;
}

/*
 * fld:  0x00
 * fstp: 0x03
 */
static struct code x87_encode_transfer(unsigned int op, union operand m)
{
    struct code c = {0};

    switch (m.width) {
    case 4:
        c.val[c.len++] = PREFIX_X87 | 0x01;
        encode_addr(&c, op, m.mem.addr, 0);
        break;
    case 8:
        c.val[c.len++] = PREFIX_X87 | 0x05;
        encode_addr(&c, op, m.mem.addr, 0);
        break;
    default:
        c.val[c.len++] = PREFIX_X87 | 0x03;
        encode_addr(&c, op | 0x05, m.mem.addr, 0);
        break;
    }

    return c;
}

static struct code fstp(enum instr_optype optype, union operand op)
{
    struct code c = {0};

    if (optype == OPT_MEM) {
        c = x87_encode_transfer(0x03, op);
    } else {
        assert(optype == OPT_REG);
        c.val[c.len++] = PREFIX_X87 | 0x05;
        c.val[c.len++] = 0xD8 | ST(op.reg);
    }

    return c;
}

static struct code fild(unsigned int op, union operand m)
{
    struct code c = {0};

    switch (m.width) {
    case 2:
        c.val[c.len++] = PREFIX_X87 | 0x07;
        encode_addr(&c, op, m.mem.addr, 0);
        break;
    case 4:
        c.val[c.len++] = PREFIX_X87 | 0x03;
        encode_addr(&c, op, m.mem.addr, 0);
        break;
    case 8:
        c.val[c.len++] = PREFIX_X87 | 0x07;
        encode_addr(&c, op | 0x05, m.mem.addr, 0);
        break;
    }

    return c;
}

static struct code fxch(union operand op)
{
    struct code c = {0};

    c.val[c.len++] = PREFIX_X87 | 0x09;
    c.val[c.len++] = 0xC8 | ST(op.reg);
    return c;
}

static struct code fistp(union operand op)
{
    struct code c = {0};

    if (op.width == 2) {
        c.val[c.len++] = PREFIX_X87 | 0x07;
        encode_addr(&c, 0x03, op.mem.addr, 0);
    } else if (op.width == 4) {
        c.val[c.len++] = PREFIX_X87 | 0x03;
        encode_addr(&c, 0x03, op.mem.addr, 0);
    } else {
        assert(op.width == 8);
        c.val[c.len++] = PREFIX_X87 | 0x07;
        encode_addr(&c, 0x07, op.mem.addr, 0);
    }

    return c;
}

static struct code fnstcw(union operand op)
{
    struct code c = {0};

    c.val[c.len++] = PREFIX_X87 | 0x09;
    encode_addr(&c, 0x07, op.mem.addr, 0);
    return c;
}

static struct code fldcw(union operand op)
{
    struct code c = {0};

    c.val[c.len++] = PREFIX_X87 | 0x09;
    encode_addr(&c, 0x05, op.mem.addr, 0);
    return c;
}

/*
 * faddp:  0xC0
 * fsubrp: 0xE8
 * fmulp:  0xC8
 * fdivrp: 0xF8
 */
static struct code x87_encode_arithmetic(unsigned int op, union operand r)
{
    struct code c = {0};

    c.val[c.len++] = PREFIX_X87 | 0x06;
    c.val[c.len++] = op | ST(r.reg);
    return c;
}

INTERNAL struct code encode(struct instruction instr)
{
    switch (instr.opcode) {
    default: assert(0);
    case INSTR_ADD:
        return encode_add(instr.optype, instr.source, instr.dest);
    case INSTR_ADDSD:
        return sse_enc(instr.optype, 0xF2, 0x58, instr.source, instr.dest, 0);
    case INSTR_ADDSS:
        return sse_enc(instr.optype, 0xF3, 0x58, instr.source, instr.dest, 0);
    case INSTR_CVTSI2SS:
        return sse_enc(instr.optype, 0xF3, 0x2A, instr.source, instr.dest, 1);
    case INSTR_CVTSI2SD:
        return sse_enc(instr.optype, 0xF2, 0x2A, instr.source, instr.dest, 1);
    case INSTR_CVTSS2SD:
        return sse_enc(instr.optype, 0xF3, 0x5A, instr.source, instr.dest, 0);
    case INSTR_CVTSD2SS:
        return sse_enc(instr.optype, 0xF2, 0x5A, instr.source, instr.dest, 0);
    case INSTR_CVTTSD2SI:
        return sse_enc(instr.optype, 0xF2, 0x2C, instr.source, instr.dest, 0);
    case INSTR_CVTTSS2SI:
        return sse_enc(instr.optype, 0xF3, 0x2C, instr.source, instr.dest, 0);
    case INSTR_CDQ:
        return cdq();
    case INSTR_CQO:
        return cqo();
    case INSTR_NOT:
        return encode_not(instr.optype, instr.source);
    case INSTR_MUL:
        return encode_mul(instr.optype, instr.source);
    case INSTR_MULSD:
        return sse_enc(instr.optype, 0xF2, 0x59, instr.source, instr.dest, 0);
    case INSTR_MULSS:
        return sse_enc(instr.optype, 0xF3, 0x59, instr.source, instr.dest, 0);
    case INSTR_XOR:
        return encode_bitwise(instr.optype, instr.source, instr.dest, 0x30);
    case INSTR_DIV:
        return encode_div(instr.optype, instr.source);
    case INSTR_DIVSD:
        return sse_enc(instr.optype, 0xF2, 0x5E, instr.source, instr.dest, 0);
    case INSTR_DIVSS:
        return sse_enc(instr.optype, 0xF3, 0x5E, instr.source, instr.dest, 0);
    case INSTR_AND:
        return encode_bitwise(instr.optype, instr.source, instr.dest, 0x20);
    case INSTR_OR:
        return encode_bitwise(instr.optype, instr.source, instr.dest, 0x08);
    case INSTR_SHL:
        return encode_shift(instr.optype, instr.source, instr.dest, 0x00);
    case INSTR_SHR:
        return encode_shift(instr.optype, instr.source, instr.dest, 0x08);
    case INSTR_SAR:
        return encode_shift(instr.optype, instr.source, instr.dest, 0x18);
    case INSTR_CALL:
        return encode_call(instr.optype, instr.source);
    case INSTR_CMP:
        return cmp(instr.optype, instr.source, instr.dest);
    case INSTR_IDIV:
        return encode_signed_div(instr.optype, instr.source);
    case INSTR_MOV:
        return mov(instr.optype, instr.source, instr.dest);
    case INSTR_MOVSX:
        return movsx(instr.optype, instr.source, instr.dest);
    case INSTR_MOVZX:
        return movzx(instr.optype, instr.source, instr.dest);
    case INSTR_MOVAPS:
        return movaps(instr.optype, instr.source, instr.dest);
    case INSTR_MOVSD:
        return sse_mov(instr.optype, 0xF2, instr.source, instr.dest);
    case INSTR_MOVSS:
        return sse_mov(instr.optype, 0xF3, instr.source, instr.dest);
    case INSTR_PUSH:
        return encode_push(instr.optype, instr.source);
    case INSTR_POP:
        return encode_pop(instr.optype, instr.source);
    case INSTR_PXOR:
        return pxor(instr.optype, instr.source, instr.dest);
    case INSTR_SUB:
        return encode_sub(instr.optype, instr.source, instr.dest);
    case INSTR_SUBSD:
        return sse_enc(instr.optype, 0xF2, 0x5C, instr.source, instr.dest, 0);
    case INSTR_SUBSS:
        return sse_enc(instr.optype, 0xF3, 0x5C, instr.source, instr.dest, 0);
    case INSTR_UCOMISS:
        return ucomiss(instr.optype, instr.source, instr.dest);
    case INSTR_UCOMISD:
        return ucomisd(instr.optype, instr.source, instr.dest);
    case INSTR_LEA:
        return lea(instr.optype, instr.source, instr.dest);
    case INSTR_LEAVE:
        return leave();
    case INSTR_REP_MOVSQ:
        assert(instr.optype == OPT_NONE);
        return rep_movsq();
    case INSTR_RET:
        return ret();
    case INSTR_JMP:
        return jmp(instr.optype, instr.source);
    case INSTR_JA:
        return jcc(instr.optype, TEST_A, instr.source);
    case INSTR_JNA:
        return jcc(instr.optype, TEST_NA, instr.source);
    case INSTR_JP:
        return jcc(instr.optype, TEST_P, instr.source);
    case INSTR_JG:
        return jcc(instr.optype, TEST_G, instr.source);
    case INSTR_JNG:
        return jcc(instr.optype, TEST_NG, instr.source);
    case INSTR_JE:
        return jcc(instr.optype, TEST_E, instr.source);
    case INSTR_JS:
        return jcc(instr.optype, TEST_S, instr.source);
    case INSTR_JAE:
        return jcc(instr.optype, TEST_AE, instr.source);
    case INSTR_JNAE:
        return jcc(instr.optype, TEST_NAE, instr.source);
    case INSTR_JGE:
        return jcc(instr.optype, TEST_GE, instr.source);
    case INSTR_JNGE:
        return jcc(instr.optype, TEST_NGE, instr.source);
    case INSTR_JNE:
        return jcc(instr.optype, TEST_NE, instr.source);
    case INSTR_JNS:
        return jcc(instr.optype, TEST_NS, instr.source);
    case INSTR_SETE:
        return setcc(instr.optype, TEST_E, instr.source);
    case INSTR_SETA:
        return setcc(instr.optype, TEST_A, instr.source);
    case INSTR_SETNA:
        return setcc(instr.optype, TEST_NA, instr.source);
    case INSTR_SETG:
        return setcc(instr.optype, TEST_G, instr.source);
    case INSTR_SETNG:
        return setcc(instr.optype, TEST_NG, instr.source);
    case INSTR_SETP:
        return setcc(instr.optype, TEST_P, instr.source);
    case INSTR_SETNE:
        return setcc(instr.optype, TEST_NE, instr.source);
    case INSTR_SETAE:
        return setcc(instr.optype, TEST_AE, instr.source);
    case INSTR_SETNAE:
        return setcc(instr.optype, TEST_NAE, instr.source);
    case INSTR_SETGE:
        return setcc(instr.optype, TEST_GE, instr.source);
    case INSTR_SETNGE:
        return setcc(instr.optype, TEST_NGE, instr.source);
    case INSTR_SETNP:
        return setcc(instr.optype, TEST_NP, instr.source);
    case INSTR_TEST:
        return encode_test(instr.optype, instr.source, instr.dest);
    case INSTR_FLD:
        return x87_encode_transfer(0x00, instr.source);
    case INSTR_FILD:
        return fild(0x00, instr.source);
    case INSTR_FSTP:
        return fstp(instr.optype, instr.source);
    case INSTR_FISTP:
        assert(instr.optype == OPT_MEM);
        return fistp(instr.source);
    case INSTR_FNSTCW:
        assert(instr.optype == OPT_MEM);
        return fnstcw(instr.source);
    case INSTR_FLDCW:
        assert(instr.optype == OPT_MEM);
        return fldcw(instr.source);
    case INSTR_FUCOMIP:
        assert(instr.optype == OPT_REG);
        return fucomip(instr.source);
    case INSTR_FXCH:
        assert(instr.optype == OPT_REG);
        return fxch(instr.source);
    case INSTR_FADDP:
        return x87_encode_arithmetic(0xC0, instr.source);
    case INSTR_FSUBRP:
        return x87_encode_arithmetic(0xE8, instr.source);
    case INSTR_FMULP:
        return x87_encode_arithmetic(0xC8, instr.source);
    case INSTR_FDIVRP:
        return x87_encode_arithmetic(0xF8, instr.source);
    }
}
