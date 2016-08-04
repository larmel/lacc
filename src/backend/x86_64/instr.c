#include "elf.h"

#include <assert.h>

/*
 * Map register enum values to register encoding. Depends on register
 * enumeration values.
 */
#define reg(arg) (((arg).r - 1) % 8)
#define is_64_bit(arg) ((arg).w >> 3)
#define is_32_bit(arg) (((arg).w >> 2) & 1)
#define is_16_bit(arg) (((arg).w >> 1) & 1)
#define is_64_bit_reg(arg) ((arg) >= R8 && (arg) <= R15)
#define is_sse_reg(arg) ((arg).r >= XMM0 && (arg).r <= XMM8)

/*
 * Determine if register or memory argument requires REX prefix.
 */
#define rrex(arg) \
    ((is_64_bit(arg) && (arg).r < XMM0) || is_64_bit_reg(arg.r) || \
        (arg.w == 1 && (arg.r == DI || arg.r == SI)))
#define mrex(arg) \
    (!arg.sym && ((is_64_bit_reg(arg.base) || is_64_bit_reg(arg.offset))))

/*
 */
#define PREFIX_SSE 0x0F

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
#define X(arg) 0
#define B(arg) is_64_bit_reg((arg).r)

/*
 * Operand size bit, 0 for 8 bit operand and 1 for 32 bit operand, when
 * default is 32 bit. [Table B-6]
 */
#define w(arg) (~(arg).w & 1)

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
 * Conditional test/jump codes. A nice reference is
 * http://unixwiz.net/techtips/x86-jumps.html
 */
enum tttn {
    TEST_AE = 0x3,
    TEST_Z = 0x4,
    TEST_NE = 0x5,
    TEST_A = 0x7,
    TEST_S = 0x8,
    TEST_P = 0xA,
    TEST_NP = 0xB,
    TEST_GE = 0xD,
    TEST_G = 0xF
};

/*
 * Encode address using ModR/M, SIB and Displacement bytes. Based on
 * Table 2.2 and Table 2.3 in reference manual. Symbol references are
 * encoded as %rip- relative addresses, section 2.2.1.6.
 */
static void encode_addr(
    struct code *c,
    unsigned char reg,
    struct address addr)
{
    assert(addr.mult == 1 || !addr.mult);

    if (addr.sym) {
        c->val[c->len++] = ((reg & 0x7) << 3) | 0x5;
        elf_add_reloc_text(addr.sym, R_X86_64_PC32, c->len, addr.disp);
        memset(&c->val[c->len], 0, 4);
        c->len += 4;
    } else {
        /* ModR/M */
        c->val[c->len++] =
            ((reg & 0x7) << 3) | ((!addr.offset) ? ((addr.base - 1) % 8) : 4);
        if (addr.disp && in_byte_range(addr.disp))
            c->val[c->len - 1] |= 0x40;
        else if (addr.disp)
            c->val[c->len - 1] |= 0x80;

        /* SIB */
        if (addr.offset)
            c->val[c->len++] = (addr.offset - 1) << 3 | (addr.base - 1);

        /* Displacement */
        if (addr.disp && in_byte_range(addr.disp))
            c->val[c->len++] = addr.disp;
        else if (addr.disp) {
            memcpy(&c->val[c->len], &addr.disp, 4);
            c->len += 4;
        }
    }
}

static int is_byte_imm(struct immediate imm)
{
    return imm.type == IMM_INT && (imm.w == 1 || 
        (imm.w == 2 && in_byte_range(imm.d.word)) ||
        (imm.w == 4 && in_byte_range(imm.d.dword)) ||
        (in_byte_range(imm.d.qword)));
}

static int is_32bit_imm(struct immediate imm)
{
    return imm.type == IMM_INT && (imm.w < 8 || in_32bit_range(imm.d.qword));
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
    struct code c = {{0}};
    switch (optype) {
    case OPT_IMM_REG:
        /* Alternative encoding (shorter). */
        if (rrex(b.reg)) {
            c.val[c.len++] = REX | W(b.reg) | B(b.reg);
        }
        c.val[c.len++] = 0xB8 | w(b.reg) << 3 | reg(b.reg);
        if (a.imm.w == 1) {
            assert(a.imm.type == IMM_INT);
            c.val[c.len++] = a.imm.d.byte;
        } else if (a.imm.w == 2) {
            assert(a.imm.type == IMM_INT);
            memcpy(&c.val[c.len], &a.imm.d.word, 2);
            c.len += 2;
        } else if (is_32bit_imm(a.imm) || a.imm.type == IMM_ADDR) {
            if (is_64_bit(b.reg)) {
                /* Special case, not using alternative encoding. */
                c.val[1] = 0xC7;
                c.val[2] = 0xC0 | reg(b.reg);
                c.len = 3;
            }
            if (a.imm.type == IMM_INT) {
                memcpy(&c.val[c.len], &a.imm.d.dword, 4);
                c.len += 4;
            } else {
                assert(a.imm.type == IMM_ADDR);
                elf_add_reloc_text(
                    a.imm.d.addr.sym, R_X86_64_32S, c.len, a.imm.d.addr.disp);
                memset(&c.val[c.len], 0, 4);
                c.len += 4;
            }
        } else {
            assert(a.imm.w == 8);
            assert(a.imm.type == IMM_INT);
            memcpy(&c.val[c.len], &a.imm.d.qword, 8);
            c.len += 8;
        }
        break;
    case OPT_REG_REG:
        assert(a.reg.w == b.reg.w);
        assert(a.reg.w == 4 || a.reg.w == 8);
        if (rrex(a.reg) || rrex(b.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = 0x88 | w(a.reg);
        c.val[c.len++] = 0xC0 | reg(a.reg) << 3 | reg(b.reg);
        break;
    case OPT_REG_MEM:
        if (is_16_bit(a.reg))
            c.val[c.len++] = 0x66; /* Legacy prefix */
        if (rrex(a.reg) || mrex(b.mem.addr)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | mrex(b.mem.addr);
        }
        c.val[c.len++] = 0x88 | w(a.reg);
        encode_addr(&c, reg(a.reg), b.mem.addr);
        break;
    case OPT_MEM_REG:
        if (rrex(b.reg) || mrex(a.mem.addr)) {
            c.val[c.len++] =
                REX | W(b.reg) | R(b.reg) | mrex(a.mem.addr);
        }
        c.val[c.len++] = 0x8A + w(b.reg);
        encode_addr(&c, reg(b.reg), a.mem.addr);
        break;
    case OPT_IMM_MEM:
        assert(a.imm.type == IMM_INT && a.imm.w == 4);
        c.val[c.len++] = 0xC6 | w(b.mem);
        encode_addr(&c, 0, b.mem.addr);
        memcpy(&c.val[c.len], &a.imm.d.dword, 4);
        c.len += 4;
        break;
    default:
        assert(0);
        break;
    }
    return c;
}

static struct code movsx(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};
    assert(optype == OPT_MEM_REG);

    if (rrex(b.reg) || mrex(a.mem.addr)) {
        c.val[c.len] = REX | W(b.reg) | R(b.reg);
        c.val[c.len++] |= is_64_bit_reg(a.mem.addr.base); /* B(..) */
    }
    if (is_32_bit(a.mem) && is_64_bit(b.reg)) {
        c.val[c.len++] = 0x63;
    } else {
        c.val[c.len++] = 0x0F;
        c.val[c.len++] = 0xBE | w(a.mem);
    }

    encode_addr(&c, reg(b.reg), a.mem.addr);
    return c;
}

static struct code movzx(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};

    if (optype == OPT_REG_REG) {
        if (rrex(a.reg) || rrex(b.reg))
            c.val[c.len++] = REX | W(b.reg) | R(b.reg);
        c.val[c.len++] = 0x0F;
        c.val[c.len++] = 0xB6 | w(a.reg);
        c.val[c.len++] = 0xC0 | reg(b.reg) << 3 | reg(a.reg);
    } else if (optype == OPT_MEM_REG) {
        if (mrex(a.mem.addr) || rrex(b.reg)) {
            c.val[c.len] = REX | W(b.reg) | R(b.reg);
            c.val[c.len++] |= is_64_bit_reg(a.mem.addr.base);
        }
        c.val[c.len++] = 0x0F;
        c.val[c.len++] = 0xB6 | w(a.mem);
        encode_addr(&c, reg(b.reg), a.mem.addr);
    } else
        assert(0);

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
            memcpy(&c.val[3], &a.imm.d.dword, 4);
            c.len = 7;
        }
        break;
    case OPT_REG_REG:
        c.len = 0;
        if (rrex(a.reg)) {
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
    struct code c = {{0}};

    switch (optype) {
    case OPT_REG_REG:
        if (rrex(a.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = 0x00 | w(a.reg);
        c.val[c.len++] = 0xC0 | reg(a.reg) << 3 | reg(b.reg);
        break;
    case OPT_IMM_REG:
        assert(a.imm.type == IMM_INT);
        if (rrex(b.reg))
            c.val[c.len++] = REX | W(b.reg) | B(b.reg);
        c.val[c.len++] = 0x80 | is_byte_imm(a.imm) << 1 | w(b.reg);
        c.val[c.len++] = 0xC0 | reg(b.reg);
        if (is_byte_imm(a.imm)) {
            c.val[c.len++] = a.imm.d.byte;
        } else {
            assert(is_32bit_imm(a.imm));
            memcpy(&c.val[c.len], &a.imm.d.dword, 4);
            c.len += 4;
        }
        break;
    case OPT_IMM_MEM:
        assert(a.imm.type == IMM_INT && a.imm.w == 4 && !mrex(b.mem.addr));
        c.val[c.len++] = 0x80 | is_byte_imm(a.imm) << 1 | w(b.mem);
        encode_addr(&c, 0, b.mem.addr);
        if (is_byte_imm(a.imm)) {
            c.val[c.len++] = a.imm.d.byte;
        } else {
            assert(is_32bit_imm(a.imm));
            memcpy(&c.val[c.len], &a.imm.d.dword, 4);
            c.len += 4;
        }
        break;
    default:
        assert(0);
        break;
    }

    return c;
}

static struct code call(enum instr_optype optype, union operand op)
{
    struct code c = {{0}};

    if (optype == OPT_IMM) {
        assert(op.imm.type == IMM_ADDR);
        assert(op.imm.d.addr.sym);

        c.val[c.len++] = 0xE8;
        elf_add_reloc_text(
            op.imm.d.addr.sym, R_X86_64_PC32, c.len, op.imm.d.addr.disp);
        c.len += 4;
    } else {
        assert(optype == OPT_REG);
        assert(is_64_bit_reg(op.reg.r));

        c.val[c.len++] = REX | w(op.reg);
        c.val[c.len++] = 0xFF;
        c.val[c.len++] = 0xD0 | reg(op.reg);
    }

    return c;
}

static struct code cmp(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};

    switch (optype) {
    case OPT_IMM_REG:
        if (!is_64_bit(b.reg) && !is_64_bit_reg(b.reg.r)) {
            c.val[c.len++] = 0x80 | w(b.reg);
            c.val[c.len++] = 0xF8 | reg(b.reg);
            if (is_byte_imm(a.imm)) {
                c.val[0] |= 2; /* Sign extend byte to 32 bit */
                c.val[c.len++] = a.imm.d.byte;
            } else {
                assert(a.imm.w == 4);
                memcpy(&c.val[c.len], &a.imm.d.dword, 4);
                c.len += 4;
            }
        } else {
            assert(0);
        }
        break;
    case OPT_REG_REG:
        assert(a.reg.w == b.reg.w);
        if (rrex(a.reg) || rrex(b.reg)) {
            c.val[c.len++] = REX | W(a.reg) | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = 0x38 | w(a.reg);
        c.val[c.len++] = 0xC0 | reg(a.reg) << 3 | reg(b.reg);
        break;
    default:
        assert(0);
        break;
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

    /* Existing value will be added to offset. Subtract 4 to account for
       instruction length, offset is counted after the immediate. */
    disp = elf_text_displacement(addr->sym, c.len) + addr->disp - 4;
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

    disp = elf_text_displacement(addr->sym, c.len) + addr->disp - 4;
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
    struct code c = {{0}};
    assert(optype == OPT_MEM_REG);
    assert(is_64_bit(b.reg));

    c.val[c.len++] = REX | W(b.reg) | R(b.reg);
    c.val[c.len++] = 0x8D;
    encode_addr(&c, reg(b.reg), a.mem.addr);

    return c;
}

static struct code rep_movsq(void)
{
    struct code c = {{0xF3, REX + 8, 0xA5}, 3};
    return c;
}

static struct code ret(void)
{
    /* Only 'Near return' is used, returning to a function with address
     * in the same segment, and not popping any bytes from stack. */
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

static struct code test(
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
    c.val[c.len++] = 0xC0 | reg(a.reg) << 3 | reg(b.reg);
    return c;
}

static struct code not(enum instr_optype optype, union operand op)
{
    struct code c = {{0}};
    assert(optype == OPT_REG);

    if (is_64_bit_reg(op.reg.r) || op.reg.w > 4)
        c.val[c.len++] = REX | W(op.reg) | B(op.reg);
    c.val[c.len++] = 0xF6 | w(op.reg);
    c.val[c.len++] = 0xD0 | reg(op.reg);
    return c;
}

static struct code mul(enum instr_optype optype, union operand op)
{
    struct code c = {{0}};

    if (optype == OPT_REG) {
        if (is_64_bit_reg(op.reg.r) || op.reg.w > 4)
            c.val[c.len++] = REX | W(op.reg) | B(op.reg);
        c.val[c.len++] = 0xF6 | w(op.reg);
        c.val[c.len++] = 0xE0 | reg(op.reg);
    } else {
        assert(optype == OPT_MEM);

        if (op.mem.w > 4)
            c.val[c.len++] = REX | W(op.mem) | is_64_bit_reg(op.mem.addr.base);
        c.val[c.len++] = 0xF6 | w(op.mem);
        encode_addr(&c, 0x4, op.mem.addr);
    }

    return c;
}

static struct code encode_div(enum instr_optype optype, union operand op)
{
    struct code c = {{0}};

    if (optype == OPT_REG) {
        if (is_64_bit_reg(op.reg.r) || op.reg.w > 4)
            c.val[c.len++] = REX | W(op.reg) | B(op.reg);
        c.val[c.len++] = 0xF6 | w(op.reg);
        c.val[c.len++] = 0xF0 | reg(op.reg);
    } else {
        assert(optype == OPT_MEM);

        if (op.mem.w > 4)
            c.val[c.len++] = REX | W(op.mem) | is_64_bit_reg(op.mem.addr.base);
        c.val[c.len++] = 0xF6 | w(op.mem);
        encode_addr(&c, 0x6, op.mem.addr);
    }

    return c;
}

static struct code encode_signed_div(enum instr_optype optype, union operand op)
{
    struct code c = {{0}};

    if (optype == OPT_REG) {
        c.val[c.len++] = REX | W(op.reg) | B(op.reg);
        c.val[c.len++] = 0xF6 | w(op.reg);
        c.val[c.len++] = 0xF8 | reg(op.reg);
    } else {
        assert(optype == OPT_MEM);
        c.val[c.len++] = REX | W(op.mem) | is_64_bit_reg(op.mem.addr.base);
        c.val[c.len++] = 0xF6 | w(op.mem);
        encode_addr(&c, 0x7, op.mem.addr);
    }

    return c;
}

static struct code basic_register_only_encode(
    unsigned int opcode,
    struct registr a,
    struct registr b)
{
    struct code c = {{0}};
    if (rrex(a))
        c.val[c.len++] = REX | W(a) | R(a) | B(b);
    c.val[c.len++] = opcode | w(a);
    c.val[c.len++] = 0xC0 | reg(a) << 3 | reg(b);
    return c;
}

static struct code xor(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    assert(optype == OPT_REG_REG);
    return basic_register_only_encode(0x30, a.reg, b.reg);
}

static struct code and(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};

    if (optype == OPT_REG_REG) {
        c = basic_register_only_encode(0x20, a.reg, b.reg);
    } else {
        assert(optype == OPT_IMM_REG);
        assert(a.imm.w == b.reg.w);
        assert(a.imm.w == 4 || a.imm.w == 1);

        if (b.reg.r == AX) {
            c.val[c.len++] = 0x24 | w(a.imm);
            memcpy(&c.val[c.len], &a.imm.d.dword, a.imm.w);
            c.len += a.imm.w;
        } else {
            assert(0);
        }
    }

    return c;
}

static struct code or(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    assert(optype == OPT_REG_REG);
    return basic_register_only_encode(0x08, a.reg, b.reg);
}

static struct code shl(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};
    assert(optype == OPT_REG_REG);
    assert(a.reg.r == CX && a.reg.w == 1);

    if (rrex(b.reg))
        c.val[c.len++] = REX | W(b.reg) | B(b.reg);
    c.val[c.len++] = 0xD2 | w(b.reg);
    c.val[c.len++] = 0xE0 | reg(b.reg);

    return c;
}

static struct code shr(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};

    /* First operand is shift amount. */
    if (optype == OPT_REG_REG) {
        assert(a.reg.r == CX);
        assert(a.reg.w == 1);

        if (rrex(b.reg)) {
            c.val[c.len++] = REX | W(b.reg) | B(b.reg);
        }
        c.val[c.len++] = 0xD2 | w(b.reg);
        c.val[c.len++] = 0xE8 | reg(b.reg);
    } else {
        assert(optype == OPT_IMM_REG);
        assert(a.imm.type == IMM_INT);
        assert(a.imm.w == 1);

        if (a.imm.d.dword == 1) {
            if (rrex(b.reg)) {
                c.val[c.len++] = REX | W(b.reg) | B(b.reg);
            }
            c.val[c.len++] = 0xD0 | w(b.reg);
            c.val[c.len++] = 0xE8 | reg(b.reg);
        } else {
            assert(0);
        }
    }

    return c;
}

static struct code sar(
    enum instr_optype optype,
    union operand a,
    union operand b)
{
    struct code c = {{0}};
    assert(optype == OPT_REG_REG);
    assert(a.reg.r == CX && a.reg.w == 1);

    if (rrex(b.reg))
        c.val[c.len++] = REX | W(b.reg) | B(b.reg);
    c.val[c.len++] = 0xD2 | w(b.reg);
    c.val[c.len++] = 0xF8 | reg(b.reg);

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

    encode_addr(&c, (a.reg.r - XMM0), b.mem.addr);
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
        c.val[c.len++] = PREFIX_SSE;
        c.val[c.len++] = 0x11;
        c.val[c.len++] = 0xC0 | (reg(a.reg) << 3) | reg(b.reg);
        break;
    case OPT_MEM_REG:
        if (rrex(b.reg) || mrex(a.mem.addr)) {
            c.val[c.len++] = REX | R(b.reg) | mrex(a.mem.addr);
        }
        c.val[c.len++] = PREFIX_SSE;
        c.val[c.len++] = 0x10;
        encode_addr(&c, reg(b.reg), a.mem.addr);
        break;
    case OPT_REG_MEM:
        if (rrex(a.reg) || mrex(b.mem.addr)) {
            c.val[c.len++] = REX | R(a.reg) | mrex(b.mem.addr);
        }
        c.val[c.len++] = PREFIX_SSE;
        c.val[c.len++] = 0x11;
        encode_addr(&c, reg(a.reg), b.mem.addr);
        break;
    default:
        assert(0);
        break;
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
    c.val[c.len++] = 0xC0 | (reg(b.reg) << 3) | reg(a.reg);

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
    c.val[c.len++] = 0xC0 | (reg(b.reg) << 3) | reg(a.reg);

    return c;
}

static struct code sse_generic(
    enum instr_optype optype,
    unsigned char opcode1,
    unsigned char opcode2,
    union operand a,
    union operand b)
{
    struct code c = {{0}};
    assert(optype == OPT_MEM_REG || optype == OPT_REG_REG);

    c.val[c.len++] = opcode1;
    if (optype == OPT_MEM_REG) {
        if (rrex(b.reg) || mrex(a.mem.addr) || a.mem.w == 8) {
            c.val[c.len++] = REX | W(a.mem) | R(b.reg) | mrex(a.mem.addr);
            if (!is_sse_reg(b.reg)) {
                c.val[c.len - 1] |= W(b.reg);
            }
        }
        c.val[c.len++] = PREFIX_SSE;
        c.val[c.len++] = opcode2;
        encode_addr(&c, reg(b.reg), a.mem.addr);
    } else {
        if (rrex(a.reg) || rrex(b.reg)) {
            c.val[c.len++] = REX | W(a.reg) | W(b.reg) | R(a.reg) | B(b.reg);
        }
        c.val[c.len++] = PREFIX_SSE;
        c.val[c.len++] = opcode2;
        c.val[c.len++] = 0xC0 | (reg(b.reg) << 3) | reg(a.reg);
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
    assert(a.reg.w == 8);
    assert(b.reg.w == 8);
    c.val[c.len++] = 0x66;
    c.val[c.len++] = 0x0F;
    c.val[c.len++] = 0xEF;
    c.val[c.len++] = 0xC0 | (reg(b.reg) << 3) | reg(a.reg);
    return c;
}

struct code encode(struct instruction instr)
{
    switch (instr.opcode) {
    case INSTR_ADD:
        return add(instr.optype, instr.source, instr.dest);
    case INSTR_ADDSD:
        return sse_generic(instr.optype, 0xF2, 0x58, instr.source, instr.dest);
    case INSTR_ADDSS:
        return sse_generic(instr.optype, 0xF3, 0x58, instr.source, instr.dest);
    case INSTR_CVTSI2SS:
        return sse_generic(instr.optype, 0xF3, 0x2A, instr.source, instr.dest);
    case INSTR_CVTSI2SD:
        return sse_generic(instr.optype, 0xF2, 0x2A, instr.source, instr.dest);
    case INSTR_CVTSS2SD:
        return sse_generic(instr.optype, 0xF3, 0x5A, instr.source, instr.dest);
    case INSTR_CVTSD2SS:
        return sse_generic(instr.optype, 0xF2, 0x5A, instr.source, instr.dest);
    case INSTR_CVTTSD2SI:
        return sse_generic(instr.optype, 0xF2, 0x2C, instr.source, instr.dest);
    case INSTR_CVTTSS2SI:
        return sse_generic(instr.optype, 0xF3, 0x2C, instr.source, instr.dest);
    case INSTR_CDQ:
        return cdq();
    case INSTR_CQO:
        return cqo();
    case INSTR_NOT:
        return not(instr.optype, instr.source);
    case INSTR_MUL:
        return mul(instr.optype, instr.source);
    case INSTR_MULSD:
        return sse_generic(instr.optype, 0xF2, 0x59, instr.source, instr.dest);
    case INSTR_MULSS:
        return sse_generic(instr.optype, 0xF3, 0x59, instr.source, instr.dest);
    case INSTR_XOR:
        return xor(instr.optype, instr.source, instr.dest);
    case INSTR_DIV:
        return encode_div(instr.optype, instr.source);
    case INSTR_DIVSD:
        return sse_generic(instr.optype, 0xF2, 0x5E, instr.source, instr.dest);
    case INSTR_DIVSS:
        return sse_generic(instr.optype, 0xF3, 0x5E, instr.source, instr.dest);
    case INSTR_AND:
        return and(instr.optype, instr.source, instr.dest);
    case INSTR_OR:
        return or(instr.optype, instr.source, instr.dest);
    case INSTR_SHL:
        return shl(instr.optype, instr.source, instr.dest);
    case INSTR_SHR:
        return shr(instr.optype, instr.source, instr.dest);
    case INSTR_SAR:
        return sar(instr.optype, instr.source, instr.dest);
    case INSTR_CALL:
        return call(instr.optype, instr.source);
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
        return push(instr.optype, instr.source);
    case INSTR_PXOR:
        return pxor(instr.optype, instr.source, instr.dest);
    case INSTR_SUB:
        return sub(instr.optype, instr.source, instr.dest);
    case INSTR_SUBSD:
        return sse_generic(instr.optype, 0xF2, 0x5C, instr.source, instr.dest);
    case INSTR_SUBSS:
        return sse_generic(instr.optype, 0xF3, 0x5C, instr.source, instr.dest);
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
    case INSTR_JP:
        return jcc(instr.optype, TEST_P, instr.source);
    case INSTR_JG:
        return jcc(instr.optype, TEST_G, instr.source);
    case INSTR_JZ:
        return jcc(instr.optype, TEST_Z, instr.source);
    case INSTR_JS:
        return jcc(instr.optype, TEST_S, instr.source);
    case INSTR_JAE:
        return jcc(instr.optype, TEST_AE, instr.source);
    case INSTR_JGE:
        return jcc(instr.optype, TEST_GE, instr.source);
    case INSTR_JNE:
        return jcc(instr.optype, TEST_NE, instr.source);
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
    case INSTR_SETNP:
        return setcc(instr.optype, TEST_NP, instr.source);
    case INSTR_TEST:
        return test(instr.optype, instr.source, instr.dest);
    default:
        return nop();
    }
}
