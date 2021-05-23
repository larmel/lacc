#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "compile.h"
#include "assembler.h"
#include "graphviz/dot.h"
#include "x86_64/abi.h"
#include "x86_64/assemble.h"
#include "x86_64/dwarf.h"
#include "x86_64/elf.h"
#include "x86_64/encoding.h"
#include <lacc/context.h>

#include <assert.h>
#include <limits.h>
#include <stdarg.h>

static int (*enter_context)(const struct symbol *);
static int (*emit_instruction)(struct instruction);
static int (*emit_data)(struct immediate);
static int (*flush_backend)(void);
static int (*finalize_backend)(void);

/* Current function definition being compiled. */
static struct definition *definition;

/* Values from va_list initialization. */
static struct {
    int gp_offset;
    int fp_offset;
    int overflow_arg_area_offset;
    int reg_save_area_offset;
} vararg;

/* Store incoming PARAM operations before CALL. */
static array_of(struct var) func_args;

/*
 * Use callee-saved registers %rbx, %r12, %r13, %r14 and %r15 for
 * temporary integer values.
 *
 * Use SSE registers not used for parameter passing as temporaries for
 * floating point values. These need to be saved before each function
 * call.
 */
#define TEMP_INT_REGS (sizeof(temp_int_reg) / sizeof(temp_int_reg[0]))
#define TEMP_SSE_REGS (sizeof(temp_sse_reg) / sizeof(temp_sse_reg[0]))

#define is_sse(c) (c > INSTR_XOR && c < INSTR_PXOR)

static enum reg
    temp_int_reg[] = {BX, R12, R13, R14, R15},
    temp_sse_reg[] = {XMM8, XMM9, XMM10, XMM11, XMM12, XMM13, XMM14, XMM15},
    param_int_reg[] = {DI, SI, DX, CX, R8, R9},
    param_sse_reg[] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7},
    ret_int_reg[] = {AX, DX},
    ret_sse_reg[] = {XMM0, XMM1};

/* Count number of integer/sse registers allocated for temporaries. */
static int int_regs_alloc, sse_regs_alloc;

/*
 * Keep track of used registers when evaluating expressions, not having
 * to explicitly tell which register is to be used in all rules.
 */
static int int_regs_used, sse_regs_used;

/* Number of registers pushed to x87 stack. */
static int x87_stack;

/*
 * Convert x87 register (ST0 through ST7) to stack relative position,
 * where top of stack means ST0.
 */
INTERNAL int x87_stack_pos(enum reg r)
{
    assert(r >= ST0);
    assert(r <= ST7);
    assert(x87_stack > 1);

    return (x87_stack - 1) - (r - ST0);
}

static enum reg get_x87_reg(void)
{
    enum reg st;

    st = ST0 + x87_stack;
    x87_stack++;
    assert(x87_stack <= 8);
    return st;
}

static enum reg get_sse_reg(void)
{
    assert(sse_regs_used < 8);
    return XMM0 + sse_regs_used++;
}

static enum reg get_int_reg(void)
{
    assert(int_regs_used < 2);
    return AX + int_regs_used++;
}

static void relase_regs(void)
{
    int_regs_used = 0;
    sse_regs_used = 0;
}

static void emit_jcc(enum tttn cc, struct immediate imm)
{
    struct instruction instr = {INSTR_Jcc};

    instr.optype = OPT_IMM;
    instr.cc = cc;
    instr.source.imm = imm;
    emit_instruction(instr);
}

static void emit_setcc(enum tttn cc, struct registr reg)
{
    struct instruction instr = {INSTR_SETcc};

    instr.optype = OPT_REG;
    instr.cc = cc;
    instr.source.reg = reg;
    emit_instruction(instr);
}

static void emit_rep_movs(int width)
{
    struct instruction instr = {INSTR_MOV_STR};

    instr.optype = OPT_NONE;
    instr.prefix = PREFIX_REP;
    instr.source.width = width;
    emit_instruction(instr);
}

static void emit_cxy(int width)
{
    struct instruction instr = {0};

    instr.opcode = INSTR_Cxy;
    instr.optype = OPT_NONE;
    instr.source.width = width;
    emit_instruction(instr);
}

static void emit_(enum opcode op)
{
    struct instruction instr = {0};

    instr.opcode = op;
    instr.optype = OPT_NONE;
    emit_instruction(instr);
}

static void emit_i_(enum opcode op, struct immediate imm)
{
    struct instruction instr = {0};

    instr.opcode = op;
    instr.optype = OPT_IMM;
    instr.source.imm = imm;
    emit_instruction(instr);
}

static void emit_r_(enum opcode op, struct registr reg)
{
    struct instruction instr = {0};

    instr.opcode = op;
    instr.optype = OPT_REG;
    instr.source.reg = reg;
    emit_instruction(instr);
}

static void emit_m_(enum opcode op, struct memory mem)
{
    struct instruction instr = {0};

    instr.opcode = op;
    instr.optype = OPT_MEM;
    instr.source.mem = mem;
    emit_instruction(instr);
}

static void emit_rm(enum opcode op, struct registr reg, struct memory mem)
{
    struct instruction instr = {0};

    instr.opcode = op;
    instr.optype = OPT_REG_MEM;
    instr.source.reg = reg;
    instr.dest.mem = mem;
    emit_instruction(instr);
}

static void emit_rr(enum opcode op, struct registr r1, struct registr r2)
{
    struct instruction instr = {0};

    instr.opcode = op;
    instr.optype = OPT_REG_REG;
    instr.source.reg = r1;
    instr.dest.reg = r2;
    emit_instruction(instr);
}

static void emit_mr(enum opcode op, struct memory mem, struct registr reg)
{
    struct instruction instr = {0};

    instr.opcode = op;
    instr.optype = OPT_MEM_REG;
    instr.source.mem = mem;
    instr.dest.reg = reg;
    emit_instruction(instr);
}

static void emit_im(enum opcode op, struct immediate imm, struct memory mem)
{
    struct instruction instr = {0};

    instr.opcode = op;
    instr.optype = OPT_IMM_MEM;
    instr.source.imm = imm;
    instr.dest.mem = mem;
    emit_instruction(instr);
}

static void emit_ir(enum opcode op, struct immediate imm, struct registr reg)
{
    struct instruction instr = {0};

    instr.opcode = op;
    instr.optype = OPT_IMM_REG;
    instr.source.imm = imm;
    instr.dest.reg = reg;
    emit_instruction(instr);
}

static int is_standard_register_width(int width)
{
    return width == 1
        || width == 2
        || width == 4
        || width == 8;
}

static struct registr reg(enum reg r, int w)
{
    struct registr v = {0};
    v.r = r;
    v.width = w;
    return v;
}

static int displacement_from_offset(size_t offset)
{
    if (offset > INT_MAX) {
        error("Offset %lu exceeds limit of %d\n", offset, INT_MAX);
        exit(1);
    }

    return (int) offset;
}

static int is_global_offset(const struct symbol *sym)
{
    return context.pic && sym->linkage == LINK_EXTERN;
}

static int is_register_allocated(struct var v)
{
    return v.kind == DIRECT
        && is_temporary(v.value.symbol)
        && v.value.symbol->slot != 0;
}

static enum reg allocated_register(struct var v)
{
    if (is_register_allocated(v)) {
        if (is_integer(v.value.symbol->type) || is_pointer(v.value.symbol->type)) {
            return temp_int_reg[v.value.symbol->slot - 1];
        }

        assert(is_float(v.value.symbol->type) || is_double(v.value.symbol->type));
        return temp_sse_reg[v.value.symbol->slot - 1];
    }

    return 0;
}

static struct immediate value_of(struct var var, int w)
{
    struct immediate imm = {0};

    assert(var.kind == IMMEDIATE);
    assert(!var.offset);
    assert(is_scalar(var.type));
    assert(w == 1 || w == 2 || w == 4 || w == 8);

    imm.width = w;
    imm.type = IMM_INT;
    imm.d.qword = var.value.imm.i;
    return imm;
}

static struct memory location(struct address addr, int w)
{
    struct memory loc = {0};
    loc.addr = addr;
    loc.width = w;
    return loc;
}

static struct address address_of(struct var var)
{
    struct address addr = {0};
    assert(var.kind == DIRECT || var.kind == ADDRESS);

    addr.type = ADDR_NORMAL;
    addr.displacement = displacement_from_offset(var.offset);
    switch (var.value.symbol->linkage) {
    case LINK_EXTERN:
    case LINK_INTERN:
        addr.base = IP;
        addr.sym = var.value.symbol;
        break;
    case LINK_NONE:
        addr.displacement += var.value.symbol->stack_offset;
        addr.base = BP;
        break;
    }

    ((struct symbol *) var.value.symbol)->referenced = 1;
    return addr;
}

static struct memory location_of(struct var var, int w)
{
    assert(!is_register_allocated(var));
    return location(address_of(var), w);
}

static struct address address(
    int displacement,
    enum reg base,
    enum reg index,
    unsigned scale)
{
    struct address addr = {ADDR_NORMAL};

    assert((!index && scale == 0)
        || (index && (scale == 1 || scale == 2 || scale == 4 || scale == 8)));

    addr.displacement = displacement;
    addr.base = base;
    addr.index = index;
    addr.scale = scale;
    return addr;
}

static struct address got(const struct symbol *sym)
{
    struct address addr = {0};
    assert(is_global_offset(sym));

    addr.type = ADDR_GLOBAL_OFFSET;
    addr.base = IP;
    addr.sym = sym;
    ((struct symbol *) sym)->referenced = 1;
    return addr;
}

static struct immediate addr(const struct symbol *sym)
{
    struct immediate imm = {0};

    imm.type = IMM_ADDR;
    imm.d.addr.sym = sym;
    imm.width = 8;
    if (is_global_offset(sym)) {
        if (is_function(sym->type)) {
            imm.d.addr.type = ADDR_PLT;
        } else {
            imm.d.addr.type = ADDR_GLOBAL_OFFSET;
        }
    } else if (sym->symtype != SYM_LABEL && !is_function(sym->type)) {
        imm.d.addr.base = IP;
    }

    ((struct symbol *) sym)->referenced = 1;
    return imm;
}

/*
 * Check if operand can be represented as a 32 bit constant, which is
 * the largest width allowed for many instructions.
 */
static int is_int_constant(struct var v)
{
    return v.kind == IMMEDIATE
        && (is_integer(v.type) || is_pointer(v.type))
        && !v.is_symbol
        && (size_of(v.type) < 4
            || (v.value.imm.i <= INT_MAX && v.value.imm.i >= INT_MIN));
}

static struct immediate constant(long n, int w)
{
    struct immediate imm = {0};
    assert(w == 1 || w == 2 || w == 4 || w == 8);
    imm.type = IMM_INT;
    imm.d.qword = n;
    imm.width = w;
    return imm;
}

INTERNAL enum instr_optype allocation(struct var var, union operand *op)
{
    enum reg ax;
    struct var tmp;

    if (is_register_allocated(var)) {
        op->reg.r = allocated_register(var);
        op->width = size_of(var.type);
        return OPT_REG;
    }

    switch (var.kind) {
    default: assert(0);
    case DIRECT:
        op->mem.addr = address_of(var);
        break;
    case DEREF:
        tmp = var_direct(var.value.symbol);
        assert(is_register_allocated(tmp));
        ax = allocated_register(tmp);
        op->mem.addr = address(displacement_from_offset(var.offset), ax, 0, 0);
        break;
    }

    op->width = size_of(var.type);
    return OPT_MEM;
}

/*
 * Return smallest type big enough to cover the i'th eightbyte slice of
 * aggregate type. Scalar types are returned as is.
 */
static Type slice_type(Type type, struct param_class pc, int i)
{
    size_t width;

    assert(i >= 0 && i < 4);
    assert(pc.eightbyte[i] == PC_INTEGER || pc.eightbyte[i] == PC_SSE);
    if (!is_scalar(type)) {
        width = size_of(type);
        if ((i + 1) * 8 < width) {
            type = basic_type__unsigned_long;
        } else switch (width % 8) {
        case 1:
            type = basic_type__unsigned_char;
            break;
        case 2:
            type = basic_type__unsigned_short;
            break;
        case 3:
        case 4:
            type = basic_type__unsigned_int;
            break;
        default:
            type = basic_type__unsigned_long;
            break;
        }

        if (pc.eightbyte[i] == PC_SSE) {
            width = size_of(type) % 8;
            if (width == 4) {
                type = basic_type__float;
            } else {
                assert(width == 0);
                type = basic_type__double;
            }
        }
    }

    return type;
}

static void count_register_classifications(
    struct param_class pc,
    int *intregs,
    int *sseregs)
{
    int i;
    for (i = 0; i < 4; ++i) {
        switch (pc.eightbyte[i]) {
        case PC_INTEGER:
            (*intregs)++;
            break;
        case PC_SSE:
            (*sseregs)++;
        default: break;
        }
    }
}

static int is_zero(union value val, Type type)
{
    switch (type_of(type)) {
    case T_CHAR:
    case T_SHORT:
    case T_INT:
    case T_LONG:
        return val.u == 0;
    case T_FLOAT:
        return val.f == 0.0f;
    case T_DOUBLE:
        return val.d == 0.0;
    case T_LDOUBLE:
        return get_long_double(val) == 0;
    }

    return 0;
}

/*
 * Emit instruction to load a value to specified register. Handles all
 * kinds of variables, including immediate.
 *
 * Load instruction can perform conversion, for example sign extend.
 * I.e., variable and register does not need to be of the same width.
 */
static void emit_load(
    enum opcode opcode,
    struct var source,
    struct registr dest)
{
    struct var ptr;
    enum reg ax;
    int w;

    w = size_of(source.type);
    if (opcode == INSTR_MOV) {
        w = dest.width;
    }

    assert(is_standard_register_width(w));
    switch (source.kind) {
    case IMMEDIATE:
        /*
         * There is no suitable instruction for moving an immediate
         * floating point value directly to register. Need to store it
         * in memory first, so create symbol holding constant value.
         */
        if (is_real(source.type)) {
            if (!source.is_symbol) {
                source.value.symbol = sym_create_constant(source.type, source.value.imm);
                source.is_symbol = 1;
            }
            source.kind = DIRECT;
        } else if (is_zero(source.value.imm, source.type)) {
            assert(is_integer(source.type) || is_pointer(source.type));
            emit_rr(INSTR_XOR, dest, dest);
            break;
        } else {
            assert(opcode == INSTR_MOV);
            emit_ir(INSTR_MOV, value_of(source, w), dest);
            break;
        }
    case DIRECT:
        if (is_register_allocated(source)) {
            ax = allocated_register(source);
            emit_rr(opcode, reg(ax, w), dest);
        } else if (is_global_offset(source.value.symbol)) {
            ax = dest.r < XMM0 ? dest.r : R11;
            emit_mr(INSTR_MOV,
                location(got(source.value.symbol), 8), reg(ax, 8));
            emit_mr(opcode, location(address(
                displacement_from_offset(source.offset), ax, 0, 0), w), dest);
        } else {
            emit_mr(opcode, location_of(source, w), dest);
        }
        break;
    case DEREF:
        ptr = var_direct(source.value.symbol);
        assert(is_pointer(ptr.type));
        if (is_register_allocated(ptr)) {
            ax = allocated_register(ptr);
        } else if (is_global_offset(source.value.symbol)) {
            ax = dest.r < XMM0 ? dest.r : R11;
            emit_mr(INSTR_MOV,
                location(got(source.value.symbol), 8), reg(ax, 8));
            emit_mr(INSTR_MOV,
                location(address(0, ax, 0, 0), 8),
                reg(ax, 8));
        } else {
            ax = R11;
            emit_mr(INSTR_MOV, location_of(ptr, 8), reg(ax, 8));
        }
        emit_mr(opcode,
            location(
                address(
                    displacement_from_offset(source.offset), ax, 0, 0), w),
            dest);
        break;
    case ADDRESS:
        assert(opcode == INSTR_LEA);
        assert(dest.width == 8);
        if (is_global_offset(source.value.symbol)) {
            ax = dest.r;
            emit_mr(INSTR_MOV, location(got(source.value.symbol), 8), dest);
            if (source.offset) {
                emit_mr(INSTR_LEA,
                    location(address(
                        displacement_from_offset(source.offset), ax, 0, 0), 8),
                    dest);
            }
        } else {
            emit_mr(opcode, location_of(source, 8), dest);
        }
        break;
    }
}

/*
 * Load value of type float or double to register. Valid register are
 * in the range XMM0 through XMM7.
 */
static void load_sse(struct var val, enum reg r, int w)
{
    assert(is_real(val.type));
    assert(w == 4 || w == 8);
    assert(r >= XMM0 && r <= XMM7);

    if (size_of(val.type) != w) {
        emit_load(INSTR_CVTS2S, val, reg(r, w));
    } else {
        emit_load(INSTR_MOVS, val, reg(r, w));
    }
}

/* Load field value to register, either 4 or 8 bytes. */
static void load_field(struct var v, enum reg r, int w)
{
    int bits;
    struct registr ax;

    assert(is_field(v));
    assert(v.field_width > 0 && v.field_width < 64);
    assert(v.field_offset + v.field_width <= 64);

    if (size_of(v.type) <= 4) {
        ax = reg(r, 4);
    } else {
        ax = reg(r, 8);
    }

    emit_load(INSTR_MOV, v, ax);
    bits = (ax.width * 8) - (v.field_offset + v.field_width);
    if (bits > 0) {
        emit_ir(INSTR_SHL, constant(bits, 1), ax);
    }

    bits = v.field_offset + bits;
    if (bits > 0) {
        emit_ir(is_signed(v.type) ? INSTR_SAR : INSTR_SHR,
            constant(bits, 1),
            ax);
    }

    if (is_signed(v.type) && w > ax.width) {
        emit_rr(INSTR_MOVSX, reg(r, 4), reg(r, 8));
    }
}

/*
 * Load integer variable v to register r, sign extended to fit register
 * size. Values are always sign extended to either 4 or 8 bytes on load.
 */
static void load_int(struct var v, enum reg r, int w)
{
    enum opcode opcode;

    assert(w == 4 || w == 8);
    assert(!is_real(v.type));

    if (is_field(v)) {
        load_field(v, r, w);
        return;
    }

    opcode = INSTR_MOV;
    if (v.kind == ADDRESS) {
        opcode = INSTR_LEA;
        assert(w == 8);
        assert(size_of(v.type) == w);
    } else if (v.kind == IMMEDIATE) {
        opcode = INSTR_MOV;
    } else if (size_of(v.type) < w) {
        if (is_unsigned(v.type)) {
            if (size_of(v.type) < 4) {
                opcode = INSTR_MOVZX;
            } else if (size_of(v.type) == 4 && w == 8) {
                /*
                 * Special case for unsigned extension from 32 to 64
                 * bit, as there is no instruction 'movzlq'.
                 */
                opcode = INSTR_MOV;
                w = 4;
            } else assert(0);
        } else if (is_signed(v.type)) {
            opcode = INSTR_MOVSX;
        } else assert(0);
    } else if (size_of(v.type) > w) {
        /* Truncate load from larger to smaller type. */
        assert(size_of(v.type) == 8);
        v.type = basic_type__int;
    }

    emit_load(opcode, v, reg(r, w));
}

/*
 * Load variable to register, automatically sign/width extended to
 * either 32 or 64 bit.
 */
static enum reg load(struct var v, enum reg r)
{
    int w;

    w = size_of(v.type);
    if (w < 4) {
        w = 4;
    }

    if (is_real(v.type)) {
        assert(!is_long_double(v.type));
        load_sse(v, r, w);
    } else {
        load_int(v, r, w);
    }

    return r;
}

static void load_address(struct var v, enum reg r)
{
    if (v.kind == DIRECT) {
        if (is_global_offset(v.value.symbol)) {
            emit_mr(INSTR_MOV, location(got(v.value.symbol), 8), reg(r, 8));
            if (v.offset) {
                emit_mr(INSTR_LEA,
                    location(address(
                        displacement_from_offset(v.offset), r, 0, 0), 8),
                    reg(r, 8));
            }
        } else {
            emit_mr(INSTR_LEA, location_of(v, 8), reg(r, 8));
        }
    } else {
        assert(v.kind == DEREF);
        load(var_direct(v.value.symbol), r);
        if (v.offset) {
            emit_ir(INSTR_ADD, constant(v.offset, 8), reg(r, 8));
        }
    }
}

/*
 * Keep reference to constant once generated, but reset between each
 * source file.
 */
static struct symbol *x87_unsigned_adjust_constant;

/*
 * Use long double constant with value MAX_LONG + 1 in order to generate
 * code for converting 64 bit unsigned to long double.
 */
static struct var get_x87_unsigned_adjust_constant(void)
{
    union value val;

    if (!x87_unsigned_adjust_constant) {
        val = put_long_double(18446744073709551616.L);
        x87_unsigned_adjust_constant =
            sym_create_constant(basic_type__long_double, val);
    }

    return var_direct(x87_unsigned_adjust_constant);
}

/*
 * Emit code for copying given nymber of bytes between %rsi and %rdi.
 *
 * It is not easy to beat memcpy, in particular rep movsb seems
 * surprisingly slow.
 */
static void emit_memcpy(size_t bytes)
{
    int i;
    size_t w;
    struct address source, dest;

    if (bytes <= 64) {
        for (i = 0; i < bytes; i += w) {
            source = address(i, SI, 0, 0);
            dest = address(i, DI, 0, 0);
            switch (bytes - i) {
            case 1:
            case 2:
            case 4:
            case 8:
                w = bytes - i;
                break;
            case 3:
                w = 2;
                break;
            case 5:
            case 6:
            case 7:
                w = 4;
                break;
            default:
                w = 8;
                break;
            }

            emit_mr(INSTR_MOV, location(source, w), reg(AX, w));
            emit_rm(INSTR_MOV, reg(AX, w), location(dest, w));
        }
    } else {
        emit_ir(INSTR_MOV, constant(bytes, 8), reg(DX, 8));
        emit_i_(INSTR_CALL, addr(decl_memcpy));
    }
}

/* Push value to stack, rounded up to always be 8 byte aligned. */
static void push(struct var v)
{
    int eb;

    if (is_long_double(v.type)) {
        if (v.kind == IMMEDIATE) {
            assert(!v.is_symbol);
            v.value.symbol = sym_create_constant(v.type, v.value.imm);
            v.is_symbol = 1;
            v.kind = DIRECT;
        }
        if (v.kind == DIRECT) {
            v.offset += 8;
            emit_m_(INSTR_PUSH, location_of(v, 8));
            v.offset -= 8;
            emit_m_(INSTR_PUSH, location_of(v, 8));
        } else {
            load_address(v, SI);
            emit_m_(INSTR_PUSH, location(address(8, SI, 0, 0), 8));
            emit_m_(INSTR_PUSH, location(address(0, SI, 0, 0), 8));
        }
    } else if (is_scalar(v.type)) {
        if (v.kind == IMMEDIATE && is_int_constant(v)) {
            emit_i_(INSTR_PUSH, value_of(v, 8));
        } else {
            /*
             * Not possible to push SSE registers, so load as if normal
             * integer and push that instead.
             */
            if (is_real(v.type)) {
                v.type = (is_float(v.type))
                    ? basic_type__unsigned_int
                    : basic_type__unsigned_long;
            }
            load_int(v, AX, 8);
            emit_r_(INSTR_PUSH, reg(AX, 8));
        }
    } else {
        eb = EIGHTBYTES(v.type);
        emit_ir(INSTR_SUB, constant(eb * 8, 8), reg(SP, 8));
        emit_ir(INSTR_MOV, constant(eb, 4), reg(CX, 4));
        emit_rr(INSTR_MOV, reg(SP, 8), reg(DI, 8));
        load_address(v, SI);
        emit_rep_movs(8);
    }
}

/*
 * Load value to top of x87 floating point stack, ST(0). Convert from
 * arbitrary source type.
 */
static enum reg load_x87(struct var v)
{
    size_t w;
    enum reg ax, st;
    const struct symbol *label;

    st = get_x87_reg();
    if (v.kind == IMMEDIATE) {
        assert(!v.is_symbol);
        v.value.symbol = sym_create_constant(v.type, v.value.imm);
        v.is_symbol = 1;
        v.kind = DIRECT;
    }

    w = size_of(v.type);
    if (is_real(v.type)) {
        if (v.kind == DIRECT
            && !is_register_allocated(v)
            && !is_global_offset(v.value.symbol))
        {
            emit_m_(INSTR_FLD, location_of(v, w));
        } else {
            push(v);
            emit_m_(INSTR_FLD, location(address(0, SP, 0, 0), w));
            emit_ir(INSTR_ADD, constant(EIGHTBYTES(v.type) * 8, 8), reg(SP, 8));
        }
    } else if (is_unsigned(v.type) || w == 1) {
        ax = get_int_reg();
        load_int(v, ax, 8);
        emit_r_(INSTR_PUSH, reg(ax, 8));
        emit_m_(INSTR_FILD, location(address(0, SP, 0, 0), 8));
        emit_ir(INSTR_ADD, constant(8, 8), reg(SP, 8));
        if (w == 8) {
            label = create_label(definition);
            emit_rr(INSTR_TEST, reg(ax, 8), reg(ax, 8));
            emit_jcc(CC_NS, addr(label));
            v = get_x87_unsigned_adjust_constant();
            load_x87(v);
            emit_r_(INSTR_FADDP, reg(st, 16));
            x87_stack--;
            enter_context(label);
        }
        assert(int_regs_used == 1);
        relase_regs();
    } else {
        assert(is_signed(v.type));
        assert(w != 1);
        if (v.kind == DIRECT && !is_global_offset(v.value.symbol)) {
            emit_m_(INSTR_FILD, location_of(v, w));
        } else {
            push(v);
            emit_m_(INSTR_FILD, location(address(0, SP, 0, 0), w));
            emit_ir(INSTR_ADD, constant(w, 8), reg(SP, 8));
        }
    }

    return st;
}

static enum reg load_float_as_integer(
    struct var val,
    Type type)
{
    enum reg ax, cx, xmm0, xmm1;
    struct symbol *convert, *next;
    union value limit;
    int ws, wd;

    assert(is_real(val.type));
    assert(is_integer(type));

    wd = size_of(type);
    ax = get_int_reg();
    if (is_signed(type)) {
        wd = (wd < 4) ? 4 : wd;
        assert(wd == 4 || wd == 8);
        emit_load(INSTR_CVTTS2SI, val, reg(ax, wd));
    } else if (wd < 4) { /* <= 4?? */
        emit_load(INSTR_CVTTS2SI, val, reg(ax, 4));
    } else if (wd < 8) {
        emit_load(INSTR_CVTTS2SI, val, reg(ax, 8));
    } else {
        cx = get_int_reg();
        xmm0 = get_sse_reg();
        xmm1 = get_sse_reg();
        ws = size_of(val.type);
        assert(ws == 4 || ws == 8);
        convert = create_label(definition);
        next = create_label(definition);
        load_sse(val, xmm0, ws);
        if (is_float(val.type)) {
            limit.f = (float) LONG_MAX;
            load_sse(var_numeric(basic_type__float, limit), xmm1, ws);
        } else {
            limit.d = (double) LONG_MAX;
            load_sse(var_numeric(basic_type__double, limit), xmm1, ws);
        }
        emit_rr(INSTR_UCOMIS, reg(xmm1, ws), reg(xmm0, ws));
        emit_jcc(CC_AE, addr(convert));
        /* Value is representable as signed long. */
        emit_load(INSTR_CVTTS2SI, val, reg(ax, 8));
        emit_i_(INSTR_JMP, addr(next));
        enter_context(convert);
        /* Trickery to convert value not within signed long. */
        emit_rr(INSTR_SUBS, reg(xmm1, ws), reg(xmm0, ws));
        emit_rr(INSTR_CVTTS2SI, reg(xmm0, ws), reg(ax, 8));
        emit_ir(INSTR_MOV, constant(LONG_MAX + 1ul, 8), reg(cx, 8));
        emit_rr(INSTR_XOR, reg(cx, 8), reg(ax, 8));
        enter_context(next);
    }

    return ax;
}

static enum reg load_integer_as_float(struct var val, Type type)
{
    struct symbol *label, *next;
    enum reg xmm, ax, cx;
    int w;

    assert(is_integer(val.type));
    assert(is_float(type) || is_double(type));

    w = size_of(type);
    xmm = get_sse_reg();
    if (is_signed(val.type)) {
        if (size_of(val.type) < 4 || is_field(val)) {
            ax = get_int_reg();
            load_int(val, ax, 4);
            emit_rr(INSTR_CVTSI2S, reg(ax, 4), reg(xmm, w));
        } else {
            emit_load(INSTR_CVTSI2S, val, reg(xmm, w));
        }
    } else {
        ax = get_int_reg();
        if (size_of(val.type) < 4) {
            load_int(val, ax, 4);
            emit_rr(INSTR_CVTSI2S, reg(ax, 4), reg(xmm, w));
        } else {
            cx = get_int_reg();
            load_int(val, ax, 8);
            label = create_label(definition);
            next = create_label(definition);
            /*
             * Check if unsigned integer is small enough to be
             * interpreted as signed.
             */
            if (size_of(val.type) == 4) {
                emit_rr(INSTR_MOV, reg(ax, 4), reg(ax, 4));
            }
            emit_rr(INSTR_TEST, reg(ax, 8), reg(ax, 8));
            emit_jcc(CC_S, addr(label));
            emit_rr(INSTR_CVTSI2S, reg(ax, 8), reg(xmm, w));
            emit_i_(INSTR_JMP, addr(next));
            enter_context(label);
            /*
             * Convert large unsigned integer by adding up two halves.
             */
            emit_rr(INSTR_MOV, reg(ax, 8), reg(cx, 8));
            emit_ir(INSTR_SHR, constant(1, 1), reg(cx, 8));
            emit_ir(INSTR_AND, constant(1, 4), reg(ax, 4));
            emit_rr(INSTR_OR, reg(cx, 8), reg(ax, 8));
            emit_rr(INSTR_CVTSI2S, reg(ax, 8), reg(xmm, w));
            emit_rr(INSTR_ADDS, reg(xmm, w), reg(xmm, w));
            enter_context(next);
        }
    }

    return xmm;
}

static enum reg load_float_from_long_double(
    struct var val,
    Type type)
{
    size_t w;
    enum reg xmm;

    assert(is_long_double(val.type));
    assert(is_float(type) || is_double(type));
    w = size_of(type);
    xmm = get_sse_reg();
    load_x87(val);
    emit_m_(INSTR_FSTP, location(address(-8, SP, 0, 0), w));
    emit_mr(INSTR_MOVS,
        location(address(-8, SP, 0, 0), w), reg(xmm, w));

    assert(x87_stack == 1);
    x87_stack = 0;
    return xmm;
}

/*
 * Load value of type long double converted to type another integer or
 * floating type.
 *
 * How exactly this works is a mystery. We mimic what GCC and clang
 * outputs, and hope for the best.
 */
static enum reg load_integer_from_long_double(
    struct var val,
    Type type)
{
    int w;
    enum reg ax;

    assert(is_long_double(val.type));
    assert(is_integer(type));
    assert(!is_long_double(type));
    if (size_of(type) < 2) {
        type = basic_type__short;
    } else if (size_of(type) == 4 && is_unsigned(type)) {
        type = basic_type__long;
    }

    w = size_of(type);
    load_x87(val);
    ax = get_int_reg();

    /* Store and modify control word using magic. */
    emit_m_(INSTR_FNSTCW, location(address(-16, SP, 0, 0), 2));
    emit_mr(INSTR_MOVZX,
        location(address(-16, SP, 0, 0), 2), reg(ax, 4));
    emit_ir(INSTR_OR, constant(12 << 8, 4), reg(ax, 4));
    emit_rm(INSTR_MOV,
        reg(ax, 2), location(address(-14, SP, 0, 0), 2));

    /* Load control word, and store integer. */
    emit_m_(INSTR_FLDCW, location(address(-14, SP, 0, 0), 2));
    emit_m_(INSTR_FISTP, location(address(-8, SP, 0, 0), w));
    assert(x87_stack == 1);
    x87_stack = 0;

    /* Restore the old control word and load result to register. */
    emit_m_(INSTR_FLDCW, location(address(-16, SP, 0, 0), 2));
    emit_mr(INSTR_MOV,
        location(address(-8, SP, 0, 0), w), reg(ax, w));
    return ax;
}

/*
 * Read value while doing type conversion (if necessary), returning a
 * register holding the value. Type is promoted to be at least 32 bit
 * wide.
 */
static enum reg load_cast(struct var val, Type type)
{
    enum reg r;

    if (is_long_double(type)) {
        r = load_x87(val);
    } else if (is_long_double(val.type)) {
        if (is_real(type)) {
            r = load_float_from_long_double(val, type);
        } else {
            r = load_integer_from_long_double(val, type);
        }
    } else {
        if (size_of(type) < 4) {
            type = is_integer(type)
                ? basic_type__int
                : basic_type__unsigned_int;
        }
        assert(size_of(type) == 4 || size_of(type) == 8);
        if (is_real(val.type)) {
            assert(!is_long_double(val.type));
            if (is_real(type)) {
                r = get_sse_reg();
                load_sse(val, r, size_of(type));
            } else {
                r = load_float_as_integer(val, type);
            }
        } else if (is_real(type)) {
            r = load_integer_as_float(val, type);
        } else {
            r = get_int_reg();
            load_int(val, r, size_of(type));
        }
    }

    return r;
}

static void store_x87(struct var v)
{
    enum reg r1, r2;

    assert(x87_stack > 0);
    x87_stack--;

    assert(size_of(v.type) == 16);
    if (v.kind == DEREF) {
        r1 = get_int_reg();
        r2 = get_int_reg();
        load_address(v, r2);
        v.type = basic_type__long;
        emit_ir(INSTR_SUB, constant(16, 8), reg(SP, 8));
        emit_m_(INSTR_FSTP, location(address(0, SP, 0, 0), 16));
        emit_r_(INSTR_POP, reg(r1, 8));
        emit_rm(INSTR_MOV,
            reg(r1, 8), location(address(0, r2, 0, 0), 8));
        emit_r_(INSTR_POP, reg(r1, 8));
        emit_rm(INSTR_MOV,
            reg(r1, 8), location(address(8, r2, 0, 0), 8));
    } else {
        assert(v.kind == DIRECT);
        emit_m_(INSTR_FSTP, location_of(v, 16));
    }
}

static void bitwise_imm_reg(
    enum opcode opcode,
    struct immediate imm,
    struct registr target)
{
    assert(imm.type == IMM_INT);
    assert(target.r != R11);
    if (imm.width == 8 && (imm.d.qword > INT_MAX || imm.d.qword < INT_MIN)) {
        emit_ir(INSTR_MOV, imm, reg(R11, 8));
        emit_rr(opcode, reg(R11, 8), target);
    } else {
        emit_ir(opcode, imm, target);
    }
}

/*
 * Emit store of either register or immediate operand.
 *
 * Immediate values might need to be loaded to a register before being
 * stored.
 */
static void store_op(
    enum instr_optype optype,
    union operand op,
    struct var target)
{
    size_t w;
    long mask;
    enum reg ax;
    enum opcode opc;
    struct var field;
    struct memory mem;

    assert(optype == OPT_IMM || optype == OPT_REG);
    if (is_long_double(target.type)) {
        assert(optype == OPT_REG);
        store_x87(target);
        return;
    }

    w = size_of(target.type);
    opc = INSTR_MOV;
    if (is_real(target.type)) {
        opc = INSTR_MOVS;
        assert(optype == OPT_REG);
    } else if (is_field(target)) {
        assert(target.field_width > 0 && target.field_width < 64);
        field = target;
        field.field_width = 0;
        field.field_offset = 0;
        load(field, CX);
        mask = ((1L << target.field_width) - 1) << target.field_offset;
        bitwise_imm_reg(INSTR_AND, constant(~mask, w), reg(CX, w));
        if (optype == OPT_IMM) {
            assert(op.imm.type == IMM_INT);
            mask = (op.imm.d.qword << target.field_offset) & mask;
            if (mask != 0) {
                bitwise_imm_reg(INSTR_OR, constant(mask, w), reg(CX, w));
            }
            optype = OPT_REG;
            op.reg = reg(CX, w);
        } else {
            if (target.field_offset) {
                emit_ir(INSTR_SHL,
                    constant(target.field_offset, 1), op.reg);
            }
            assert(op.width == w);
            bitwise_imm_reg(INSTR_AND, constant(mask, w), op.reg);
            emit_rr(INSTR_OR, reg(CX, w), op.reg);
        }
    }

    switch (target.kind) {
    case DIRECT:
        assert(!is_array(target.type));
        if (optype == OPT_IMM) {
            if ((ax = allocated_register(target)) != 0) {
                emit_ir(opc, op.imm, reg(ax, w));
            } else {
                if (is_global_offset(target.value.symbol)) {
                    emit_mr(INSTR_MOV,
                        location(got(target.value.symbol), 8), reg(R11, 8));
                    mem = location(address(
                        displacement_from_offset(target.offset), R11, 0, 0), w);
                } else {
                    mem = location_of(target, w);
                }

                emit_im(opc, op.imm, mem);
            }
        } else {
            if ((ax = allocated_register(target)) != 0) {
                emit_rr(opc, op.reg, reg(ax, w));
            } else if (is_global_offset(target.value.symbol)) {
                emit_mr(INSTR_MOV,
                    location(got(target.value.symbol), 8), reg(R11, 8));
                emit_rm(opc, op.reg,
                    location(address(
                        displacement_from_offset(target.offset), R11, 0, 0), w));
            } else {
                emit_rm(opc, op.reg, location_of(target, w));
            }
        }
        break;
    default:
        assert(target.kind == DEREF);
        if (!target.is_symbol) {
            target.kind = IMMEDIATE;
            load_int(target, R11, 8);
        } else {
            assert(is_pointer(target.value.symbol->type));
            load_int(var_direct(target.value.symbol), R11, 8);
        }
        mem = location(address(
            displacement_from_offset(target.offset), R11, 0, 0), w);
        if (optype == OPT_IMM) {
            emit_im(opc, op.imm, mem);
        } else {
            emit_rm(opc, op.reg, mem);
        }
        break;
    }
}

static void store(enum reg r, struct var v)
{
    union operand operand = {0};
    operand.reg = reg(r, size_of(v.type));
    store_op(OPT_REG, operand, v);
}

/*
 * Determine if given parameter classification can fit in registers
 * available.
 *
 * Parameters of type long double are always passed by memory. Only
 * integer and SSE classifications apply.
 */
static int alloc_register_params(
    struct param_class pc,
    int *next_integer_reg,
    int *next_sse_reg)
{
    int ints, sses;

    if (pc.eightbyte[0] == PC_INTEGER || pc.eightbyte[0] == PC_SSE) {
        ints = *next_integer_reg;
        sses = *next_sse_reg;
        count_register_classifications(pc, &ints, &sses);
        if (ints <= MAX_INTEGER_ARGS && sses <= MAX_SSE_ARGS) {
            *next_integer_reg = ints;
            *next_sse_reg = sses;
            return 1;
        }
    }

    return 0;
}

static void move_to_from_registers(
    struct var var,
    struct param_class pc,
    enum reg *intregs,
    enum reg *sseregs,
    int toggle_load)
{
    int i, n;
    enum reg r;
    Type type;

    if (pc.eightbyte[0] == PC_X87) {
        assert(pc.eightbyte[1] == PC_X87UP);
        assert(pc.eightbyte[2] == PC_NO_CLASS);
        assert(toggle_load);
        var.type = basic_type__long_double;
        load_x87(var);
    } else {
        type = var.type;
        n = EIGHTBYTES(type);
        for (i = 0; i < n; ++i) {
            if (pc.eightbyte[i] == PC_INTEGER) {
                r = *intregs++;
            } else {
                r = *sseregs++;
            }
            var.type = slice_type(type, pc, i);
            if (toggle_load) {
                load(var, r);
            } else {
                store(r, var);
            }
            var.offset += 8;
        }
    }   
}

/*
 * Move already classified eightbytes of variable to registers. Used
 * for parameter loading and return values.
 */
static void load_object_to_registers(
    struct var var,
    struct param_class pc,
    enum reg *intregs,
    enum reg *sseregs)
{
    move_to_from_registers(var, pc, intregs, sseregs, 1);
}

/*
 * Write to variable from already classified eightbytes stored in
 * registers.
 */
static void store_object_from_registers(
    struct var var,
    struct param_class pc,
    enum reg *intregs,
    enum reg *sseregs)
{
    move_to_from_registers(var, pc, intregs, sseregs, 0);
}

/*
 * Push arguments from previous IR_PARAM statements, which are buffered
 * in static array. Assign parameter class to arguments; using registers
 * %rdi, %rsi, %rdx, %rcx, %r8 and %r9 for class INTEGER, and %xmm0
 * through %xmm7 for SSE. Arguments of class MEMORY, or those which do
 * not fit in the available registers, are passed on stack.
 *
 * Return number of bytes of stack space used. This must be added back
 * to %rsp after result is read.
 */
static int push_function_arguments(Type type, struct param_class respc)
{
    int i, n;
    int next_integer_reg = 0,
        next_sse_reg = 0,
        mem_used = 0,
        register_args = 0;
    struct param_class pc;
    struct var arg;
    struct {
        struct param_class pc;
        int i;
    } argpc[MAX_REGISTER_ARGS];

    assert(is_function(type));
    if (respc.eightbyte[0] == PC_MEMORY) {
        next_integer_reg = 1;
    }

    /*
     * Classify parameters, handing out register slots sequentially for
     * as long as there are enough available.
     */
    for (i = 0; i < array_len(&func_args); ++i) {
        arg = array_get(&func_args, i);
        pc = classify(arg.type);
        if (alloc_register_params(pc, &next_integer_reg, &next_sse_reg)) {
            argpc[register_args].pc = pc;
            argpc[register_args].i = i;
            register_args++;
        } else {
            mem_used += 8*EIGHTBYTES(arg.type);
        }
    }

    /*
     * Make sure stack is aligned to 16 byte after pushing all memory
     * arguments. Insert padding if necessary.
     */
    if (mem_used % 16) {
        mem_used += 8;
        assert((mem_used % 16) == 0);
        emit_ir(INSTR_SUB, constant(8, 8), reg(SP, 8));
    }

    /*
     * Pass arguments on stack from right to left. Do this before
     * populating registers, because %rdi, %rsi etc will be used to do
     * the pushing. Reverse traversal in register class list to filter
     * out non-memory arguments.
     */
    for (i = array_len(&func_args) - 1, n = register_args - 1; i >= 0; --i) {
        if (n >= 0 && argpc[n].i == i) {
            n--;
        } else {
            arg = array_get(&func_args, i);
            push(arg);
        }
    }

    /*
     * Rewind register counters before pushing arguments. When return
     * value is MEMORY, pass a pointer in first register.
     */
    next_sse_reg = 0;
    next_integer_reg = (respc.eightbyte[0] == PC_MEMORY);

    /*
     * Pass arguments that fit in in registers from left to right,
     * partitioning aggregate types into eightbyte slices.
     */
    for (i = 0; i < register_args; ++i) {
        pc = argpc[i].pc;
        arg = array_get(&func_args, argpc[i].i);
        load_object_to_registers(arg, pc,
            param_int_reg + next_integer_reg,
            param_sse_reg + next_sse_reg);
        count_register_classifications(pc, &next_integer_reg, &next_sse_reg);
    }

    /*
     * For variable argument lists, %al contains the number of vector
     * registers used.
     */
    if (is_vararg(type)) {
        assert(next_sse_reg >= 0 && next_sse_reg <= MAX_SSE_ARGS);
        emit_ir(INSTR_MOV, constant(next_sse_reg, 4), reg(AX, 4));
    }

    array_empty(&func_args);
    return mem_used;
}

/*
 * Assign stack location to locals, writing sym->stack_offset.
 *
 * Round up to nearest eightbyte, making all variables aligned.
 */
static int allocate_locals(
    struct definition *def,
    int reg_offset,
    int stack_offset)
{
    int i;
    struct symbol *sym;

    for (i = 0; i < array_len(&def->locals); ++i) {
        sym = array_get(&def->locals, i);
        assert(!sym->stack_offset);
        assert(sym->symtype == SYM_DEFINITION);
        if (sym->linkage == LINK_NONE && sym->slot == 0 && !is_vla(sym->type)) {
            stack_offset -= EIGHTBYTES(sym->type) * 8;
            sym->stack_offset = stack_offset - reg_offset;
        }
    }

    return stack_offset;
}

INTERNAL enum reg get_clobbered_register(String clobber);

static int explicit_reg_constraint(String constraint, enum reg *reg)
{
    const char *raw;

    raw = str_raw(constraint);
    if (*raw == '=' || *raw == '+') {
        raw++;
    }

    switch (*raw) {
    case 'a': *reg = AX; break;
    case 'b': *reg = BX; break;
    case 'c': *reg = CX; break;
    case 'd': *reg = DX; break;
    case 'D': *reg = DI; break;
    case 'S': *reg = SI; break;
    default: return 0;
    }

    return 1;
}

static void allocate_asmblock_registers(
    struct asm_statement *st,
    int *int_regs,
    int *sse_regs)
{
    int i;
    String str;
    enum reg r;
    struct symbol *sym;
    struct asm_operand *op;
    char clobbered[XMM15 - AX + 1] = {0};

    static String
        memory = SHORT_STRING_INIT("memory"),
        cc = SHORT_STRING_INIT("cc");

    *int_regs = 0;
    *sse_regs = 0;
    for (i = 0; i < array_len(&st->clobbers); ++i) {
        str = array_get(&st->clobbers, i);
        if (str_eq(memory, str) || str_eq(cc, str)) {
            continue;
        }

        r = get_clobbered_register(str);
        clobbered[r - AX] = 1;
    }

    /* Do not allocate BX if used in constraint. */
    assert(temp_int_reg[0] == BX);
    for (i = 0; i < array_len(&st->operands); ++i) {
        op = &array_get(&st->operands, i);
        if (explicit_reg_constraint(op->constraint, &r) && r == BX) {
            *int_regs = 1;
        }
    }

    for (i = 0; i < array_len(&st->operands); ++i) {
        op = &array_get(&st->operands, i);
        assert(op->variable.kind == DIRECT || op->variable.kind == DEREF);
        sym = (struct symbol *) op->variable.value.symbol;
        str = op->constraint;
        if (sym->slot || sym->memory || explicit_reg_constraint(str, &r)) {
            continue;
        }

        if ((str_has_chr(str, 'r') && !str_has_chr(str, 'm'))
            || op->variable.kind == DEREF)
        {
            if (is_integer(op->variable.type) || is_pointer(op->variable.type)) {
                do {
                    (*int_regs)++;
                } while (*int_regs <= TEMP_INT_REGS
                    && clobbered[temp_int_reg[*int_regs - 1] - 1]);
                if (*int_regs > TEMP_INT_REGS) {
                    error("Insufficient registers to honor __asm__ constraint.");
                    exit(1);
                }
                sym->slot = *int_regs;
            } else {
                assert(is_float(op->variable.type));
                do {
                    (*sse_regs)++;
                } while (*sse_regs <= TEMP_SSE_REGS
                    && clobbered[temp_sse_reg[*sse_regs - 1] - 1]);
                if (*sse_regs > TEMP_SSE_REGS) {
                    error("Insufficient registers to honor __asm__ constraint.");
                    exit(1);
                }
                sym->slot = *sse_regs;
            }
        } else if (!str_has_chr(str, 'r') && str_has_chr(str, 'm')) {
            sym->memory = 1;
        }
    }
}

/*
 * Assign a subset of local variables to temporary registers, populating
 * sym->slot and sym->memory.
 *
 * Functions with __asm__ blocks have only their register operands
 * allocated, and will fail to compile if there are not enough registers
 * available.
 */
static void allocate_registers(struct definition *def)
{
    int i, ir, sr;
    struct symbol *sym;
    struct asm_statement *st;

    int_regs_alloc = 0;
    sse_regs_alloc = 0;
    if (array_len(&def->asm_statements)) {
        for (i = 0; i < array_len(&def->asm_statements); ++i) {
            st = &array_get(&def->asm_statements, i);
            allocate_asmblock_registers(st, &ir, &sr);
            if (ir > int_regs_alloc) int_regs_alloc = ir;
            if (sr > sse_regs_alloc) sse_regs_alloc = sr;
        }
    } else {
        for (i = 0; i < array_len(&def->locals); ++i) {
            sym = array_get(&def->locals, i);
            if (!is_temporary(sym) || sym->slot)
                continue;

            assert(sym->linkage == LINK_NONE);
            if (is_integer(sym->type) || is_pointer(sym->type)) {
                if (int_regs_alloc < TEMP_INT_REGS) {
                    sym->slot = ++int_regs_alloc;
                }
            } else if (is_float(sym->type) || is_double(sym->type)) {
                if (sse_regs_alloc < TEMP_SSE_REGS) {
                    sym->slot = ++sse_regs_alloc;
                }
            }
        }
    }
}

/*
 * Offset from %rbp where address of return value is stored, in case of
 * function with class PC_MEMORY. The return address is passed in the
 * first integer register on enter, and needs to be preserved to write
 * the result before exit. The value is stored in %rax on return.
 *
 * The offset depends on number of registers used for temporaries, and
 * is set up on enter().
 */
static int return_address_offset;

/*
 * Emit code for entering a function.
 *
 * The stack frame contains, in order:
 *
 *  1) Registers allocated for temporaries that are callee saved, being
 *     %rbx, %r12 through %r15.
 *  2) Address of return value if result is PC_MEMORY. Part of 3) if
 *     function is vararg.
 *  3) Register save area for vararg functions, holding all values that
 *     could have been passed in registers.
 *  4) Parameters passed in registers.
 *  5) Local variables.
 *  6) Variable length arrays (not allocated here).
 *
 */
static void enter(struct definition *def)
{
    int i, n,
        register_args = 0,  /* Arguments passed in registers. */
        next_integer_reg = 0,
        next_sse_reg = 0,
        mem_offset = 16,    /* Offset of PC_MEMORY parameters. */
        reg_offset = 0,     /* Offset of %rsp to save temp registers. */
        stack_offset = 0;   /* Offset of %rsp for local variables. */
    struct var ref;
    struct symbol *sym;
    struct param_class res, arg;
    struct {
        struct param_class pc;
        int i;
    } argpc[MAX_REGISTER_ARGS];
    Type type;

    type = def->symbol->type;
    assert(is_function(type));

    /* Figure out how many registers are used for temporaries. */
    allocate_registers(def);
    reg_offset = int_regs_alloc * 8;

    /*
     * Address of return value is passed as first integer argument. If
     * return value is MEMORY, store the address at stack offset -8.
     */
    res = classify(type_next(type));
    if (res.eightbyte[0] == PC_MEMORY) {
        stack_offset = -8;
        next_integer_reg = 1;
        return_address_offset = -8 - reg_offset;
        if (is_vararg(type)) {
            return_address_offset = -176 - reg_offset;
        }
    }

    /*
     * For functions with variable argument list, reserve a fixed area
     * at the beginning of the stack fram for register values. In total
     * there are 8 bytes for each of the 6 integer registers, and 16
     * bytes for each of the 8 SSE registers, for a total of 176 bytes.
     * If return type is MEMORY, the return address is automatically
     * included in register spill area.
     */
    if (is_vararg(type)) {
        stack_offset = -176;
    }

    /*
     * Assign storage to parameters. Guarantee that parameters are 8-
     * byte aligned also for those passed by register, which makes it
     * easier to store in local frame after entering function.
     */
    for (i = 0; i < array_len(&def->params); ++i) {
        sym = array_get(&def->params, i);
        arg = classify(sym->type);
        n = EIGHTBYTES(sym->type);
        assert(!sym->stack_offset);
        assert(sym->linkage == LINK_NONE);
        if (alloc_register_params(arg, &next_integer_reg, &next_sse_reg)) {
            argpc[register_args].pc = arg;
            argpc[register_args].i = i;
            register_args++;
            stack_offset -= n * 8;
            sym->stack_offset = stack_offset - reg_offset;
        } else {
            sym->stack_offset = mem_offset;
            mem_offset += n * 8;
        }
    }

    stack_offset = allocate_locals(def, reg_offset, stack_offset);

    /* Store callee-saved registers to be used for local variables. */
    for (i = 0; i < int_regs_alloc; ++i) {
        emit_r_(INSTR_PUSH, reg(temp_int_reg[i], 8));
    }

    /*
     * Make invariant to have %rsp aligned to 0x10, which is mandatory
     * according to the ABI for function calls. On entry, (%rsp + 8) is
     * a multiple of 16, and after pushing %rbp we have alignment at
     * this point. Alignment must also be considered when calling other
     * functions, to push memory divisible by 16.
     */
    if (stack_offset - reg_offset < 0) {
        i = (reg_offset - stack_offset) % 16;
        stack_offset -= 16 - i;
    }

    /* Allocate space in the call frame to hold local variables. */
    if (stack_offset < 0) {
        emit_ir(INSTR_SUB, constant(-stack_offset, 8), reg(SP, 8));
        if (res.eightbyte[0] == PC_MEMORY) {
            emit_rm(INSTR_MOV,
                reg(param_int_reg[0], 8),
                location(address(return_address_offset, BP, 0, 0), 8));
        }
    }

    /*
     * Store all potential parameters to register save area. This
     * includes parameters that are known to be passed as registers,
     * that will anyway be stored to another stack location. It is
     * desireable to skip touching floating point unit if possible,
     * %al holds the number of registers passed.
     */
    if (is_vararg(type)) {
        sym = create_label(definition);
        vararg.gp_offset = 8*next_integer_reg;
        vararg.fp_offset = 8*MAX_INTEGER_ARGS + 16*next_sse_reg;
        vararg.overflow_arg_area_offset = mem_offset;
        vararg.reg_save_area_offset = 0;
        emit_rr(INSTR_TEST, reg(AX, 1), reg(AX, 1));
        emit_jcc(CC_E, addr(sym));
        for (i = 0; i < MAX_SSE_ARGS; ++i) {
            vararg.reg_save_area_offset -= 16;
            emit_rm(INSTR_MOVAP,
                reg(XMM0 + (7 - i), 4),
                location(address(vararg.reg_save_area_offset, BP, 0, 0), 4));
        }

        enter_context(sym);
        for (i = 0; i < MAX_INTEGER_ARGS; ++i) {
            vararg.reg_save_area_offset -= 8;
            emit_rm(INSTR_MOV,
                reg(param_int_reg[5 - i], 8),
                location(address(vararg.reg_save_area_offset, BP, 0, 0), 8));
        }
    }

    /* Move arguments from register to stack. */
    next_integer_reg = (res.eightbyte[0] == PC_MEMORY);
    next_sse_reg = 0;
    for (i = 0; i < register_args; ++i) {
        arg = argpc[i].pc;
        sym = array_get(&def->params, argpc[i].i);
        ref = var_direct(sym);
        store_object_from_registers(ref, arg,
            param_int_reg + next_integer_reg,
            param_sse_reg + next_sse_reg);
        count_register_classifications(arg, &next_integer_reg, &next_sse_reg);
    }
}

/*
 * Initialize variable references to each field of va_list object.
 *
 *     typedef struct {
 *         unsigned int gp_offset;
 *         unsigned int fp_offset;
 *         void *overflow_arg_area;
 *         void *reg_save_area;
 *     } va_list[1];
 *
 */
static void deconstruct_va_arg(
    struct var args,
    struct var *gp_offset,
    struct var *fp_offset,
    struct var *overflow_arg_area,
    struct var *reg_save_area)
{
    *gp_offset = args;
    *fp_offset = args;
    *overflow_arg_area = args;
    *reg_save_area = args;

    gp_offset->type = basic_type__unsigned_int;
    fp_offset->type = basic_type__unsigned_int;
    overflow_arg_area->type = basic_type__unsigned_long;
    reg_save_area->type = basic_type__unsigned_long;

    fp_offset->offset += 4;
    overflow_arg_area->offset += 8;
    reg_save_area->offset += 16;
}

/*
 * Execute call to va_start, initializing the provided va_list object.
 * Values are taken from static context set during enter(). Address of
 * va_args object is evaluated from expression.
 *
 *  gp_offset:          Offset from reg_save_area to first int register.
 *  fp_offset:          Offset from reg_save_area to first SSE register.
 *  overflow_arg_area:  Address of first stack argument.
 *  reg_save_area:      Address of register save area.
 *
 */
static void compile__builtin_va_start(struct var args)
{
    struct var gp_offset, fp_offset, overflow_arg_area, reg_save_area;

    deconstruct_va_arg(
        args, &gp_offset, &fp_offset, &overflow_arg_area, &reg_save_area);

    if (args.kind == DEREF) {
        emit_ir(INSTR_MOV, constant(vararg.gp_offset, 4), reg(AX, 4));
        store(AX, gp_offset);
        emit_ir(INSTR_MOV, constant(vararg.fp_offset, 4), reg(AX, 4));
        store(AX, fp_offset);
    } else {
        assert(args.kind == DIRECT);
        emit_im(INSTR_MOV,
            constant(vararg.gp_offset, 4),
            location_of(gp_offset, 4));
        emit_im(INSTR_MOV,
            constant(vararg.fp_offset, 4),
            location_of(fp_offset, 4));
    }

    emit_mr(INSTR_LEA,
        location(address(vararg.overflow_arg_area_offset, BP, 0, 0), 8),
        reg(AX, 8));
    store(AX, overflow_arg_area);

    emit_mr(INSTR_LEA,
        location(address(vararg.reg_save_area_offset, BP, 0, 0), 8),
        reg(AX, 8));
    store(AX, reg_save_area);
}

/*
 * Generate code for fetching a parameter calling res = va_arg(args, T).
 * Approach is described in section 3.5.7 of the System V ABI.
 */
static void compile__builtin_va_arg(struct var res, struct var args)
{
    const struct symbol *stack, *done = NULL;
    enum reg ax;
    struct var
        slice,
        gp_offset,
        fp_offset,
        overflow_arg_area,
        reg_save_area;
    struct param_class pc;
    int i, n, w,
        num_gp = 0,
        num_fp = 0,
        integer_regs_loaded = 0,
        sse_regs_loaded = 0;

    /*
     * Type of args is pointer to first element of array. Rewrite to
     * reference element directly.
     */
    assert(is_pointer(args.type));
    assert(!args.offset);
    if (args.kind == ADDRESS) {
        args.kind = DIRECT;
    } else {
        assert(res.kind == DIRECT);
        args.kind = DEREF;
    }

    deconstruct_va_arg(
        args, &gp_offset, &fp_offset, &overflow_arg_area, &reg_save_area);

    /*
     * Integer or SSE parameters are read from registers, if there are
     * enough of them left. Otherwise read from overflow area.
     */
    pc = classify(res.type);
    if (pc.eightbyte[0] == PC_INTEGER || pc.eightbyte[0] == PC_SSE) {
        stack = create_label(definition);
        done = create_label(definition);
        slice = res;
        n = EIGHTBYTES(res.type);
        count_register_classifications(pc, &num_gp, &num_fp);

        /*
         * Check whether there are enough registers left for the
         * argument to be passed in, comparing gp_offset and fp_offset
         * with max values.
         *
         * Max fp_offset is 6*8 + 8*16, pointing past also the first
         * integer registers. In ABI it sais 16*16, but that has to
         * be wrong. Only 8 registers are used for arguments.
         */
        if (num_gp) {
            load(gp_offset, CX);
            emit_ir(INSTR_CMP,
                constant(MAX_INTEGER_ARGS*8 - 8*num_gp, 4), reg(CX, 4));
            emit_jcc(CC_A, addr(stack));
        }
        if (num_fp) {
            load(fp_offset, DX);
            emit_ir(INSTR_CMP,
                constant(MAX_INTEGER_ARGS*8 + MAX_SSE_ARGS*16 - 16*num_fp, 4),
                reg(DX, 4));
            emit_jcc(CC_A, addr(stack));
        }

        /*
         * Keep reg_save_area in register for the remainder, a pointer
         * to stack where registers are stored. This value does not
         * change.
         */
        load(reg_save_area, SI);

        /*
         * Load argument, one eightbyte at a time. Similar to routines
         * moving object to and from register, but here we are getting
         * the values from spill area.
         *
         * Advanced addressing, load (%rsi + 8*i + (%rcx * 1))  into
         * %rax. Base of registers are stored in %rsi, first pending
         * register is at offset %rcx, and i counts number of registers
         * done. In GNU assembly it is {i*8}(%rsi, %rcx, 1).
         *
         * Floating arguments to vararg is always double.
         */
        while ((i = integer_regs_loaded + sse_regs_loaded) < n) {
            switch (pc.eightbyte[i]) {
            case PC_INTEGER:
                slice.type = slice_type(res.type, pc, i);
                i = integer_regs_loaded++;
                w = size_of(slice.type);
                emit_mr(INSTR_MOV,
                    location(address(i*8, SI, CX, 1), w), reg(AX, w));
                store(AX, slice);
                break;
            case PC_SSE:
                i = sse_regs_loaded++;
                slice.type = basic_type__double;
                emit_mr(INSTR_MOVS,
                    location(address(i*16, SI, DX, 1), 8), reg(XMM0, 8));
                store(XMM0, slice);
                break;
            default: assert(0);
            }

            slice.offset += size_of(slice.type);
        }

        /*
         * Store updated offsets pointing to next integer and SSE
         * register.
         */
        if (num_gp) {
            if (gp_offset.kind == DIRECT) {
                emit_im(INSTR_ADD,
                    constant(8*num_gp, 4), location_of(gp_offset, 4));
            } else {
                load(gp_offset, AX);
                emit_ir(INSTR_ADD, constant(8*num_gp, 4), reg(AX, 4));
                store(AX, gp_offset);
            }
        }
        if (num_fp) {
            if (fp_offset.kind == DIRECT) {
                emit_im(INSTR_ADD,
                    constant(16*num_fp, 4), location_of(fp_offset, 4));
            } else {
                load(fp_offset, AX);
                emit_ir(INSTR_ADD, constant(16*num_fp, 4), reg(AX, 4));
                store(AX, fp_offset);
            }
        }

        emit_i_(INSTR_JMP, addr(done));
        enter_context(stack);
    }

    /*
     * Parameters that are passed on stack will be read from
     * overflow_arg_area. This is also the fallback when arguments
     * do not fit in remaining registers.
     */
    load(overflow_arg_area, SI);
    w = size_of(res.type);
    if (is_standard_register_width(w) && res.kind == DIRECT) {
        if ((ax = allocated_register(res)) != 0) {
            emit_mr(INSTR_MOV, location(address(0, SI, 0, 0), w), reg(ax, w));
        } else {
            emit_mr(INSTR_MOV, location(address(0, SI, 0, 0), w), reg(AX, w));
            emit_rm(INSTR_MOV, reg(AX, w), location_of(res, w));
        }
    } else {
        load_address(res, DI);
        emit_memcpy(w);
    }

    /*
     * Move overflow_arg_area pointer to position of next memory
     * argument, aligning to 8 byte.
     */
    w = EIGHTBYTES(res.type);
    if (overflow_arg_area.kind == DIRECT) {
        emit_im(INSTR_ADD, constant(w*8, 8), location_of(overflow_arg_area, 8));
    } else {
        load(overflow_arg_area, AX);
        emit_ir(INSTR_ADD, constant(w*8, 8), reg(AX, 8));
        store(AX, overflow_arg_area);
    }

    if (pc.eightbyte[0] == PC_INTEGER || pc.eightbyte[0] == PC_SSE) {
        assert(done);
        enter_context(done);
    }
}

static void store_caller_saved_registers(void)
{
    int i;

    for (i = 0; i < sse_regs_alloc; ++i) {
        emit_ir(INSTR_SUB, constant(16, 8), reg(SP, 8));
        emit_rm(INSTR_MOVS,
            reg(temp_sse_reg[i], 8),
            location(address(0, SP, 0, 0), 8));
    }
}

static void load_caller_saved_registers(void)
{
    int i;

    for (i = sse_regs_alloc - 1; i >= 0; --i) {
        emit_mr(INSTR_MOVS,
            location(address(0, SP, 0, 0), 8),
            reg(temp_sse_reg[i], 8));
        emit_ir(INSTR_ADD, constant(16, 8), reg(SP, 8));
    }
}

/*
 * Emit function call, optionally with assignment of the result back to
 * a variable. Return register containing the result, if applicable.
 *
 * Results of class PC_MEMORY has already been written by callee.
 */
static enum reg compile_call(struct var target, struct var ptr)
{
    int mem_used;
    Type func, ret;
    struct param_class pc;
    assert(is_pointer(ptr.type));
    assert(is_function(type_next(ptr.type)));

    func = type_next(ptr.type);
    ret = type_next(func);
    pc = classify(ret);

    store_caller_saved_registers();
    mem_used = push_function_arguments(func, pc);
    if (pc.eightbyte[0] == PC_MEMORY) {
        assert(!is_void(target.type));
        load_address(target, param_int_reg[0]);
    }

    if (ptr.kind == ADDRESS) {
        assert(!ptr.offset);
        emit_i_(INSTR_CALL, addr(ptr.value.symbol));
    } else {
        load(ptr, R11);
        emit_r_(INSTR_CALL, reg(R11, 8));
    }

    if (mem_used) {
        emit_ir(INSTR_ADD, constant(mem_used, 8), reg(SP, 8));
    }

    load_caller_saved_registers();
    switch (pc.eightbyte[0]) {
    case PC_X87:
        assert(x87_stack == 0);
        x87_stack = 1;
        if (!is_void(target.type)) {
            store_x87(target);
            assert(x87_stack == 0);
        }
        return ST0;
    case PC_INTEGER:
    case PC_SSE:
        if (!is_void(target.type)) {
            store_object_from_registers(target, pc, ret_int_reg, ret_sse_reg);
        } else {
            /* Can be multi-eightbyte if standalone expression. */
            return pc.eightbyte[0] == PC_INTEGER
                ? ret_int_reg[0] : ret_sse_reg[0];
        }
    default:
        return AX;
    }
}

static enum tttn compile_compare(
    enum optype op,
    struct var l,
    struct var r)
{
    size_t w;
    enum tttn cc;
    enum reg ax, cx;
    enum reg xmm0, xmm1;

    switch (op) {
    default: assert(0);
    case IR_OP_EQ:
        cc = CC_E;
        break;
    case IR_OP_NE:
        cc = CC_NE;
        break;
    case IR_OP_GE:
        cc = is_unsigned(l.type) || is_real(l.type)
            ? CC_AE
            : CC_GE;
        break;
    case IR_OP_GT:
        cc = is_unsigned(l.type) || is_real(l.type)
            ? CC_A
            : CC_G;
        break;
    }

    w = size_of(l.type);
    if (is_real(l.type)) {
        xmm0 = load_cast(l, l.type);
        xmm1 = load_cast(r, r.type);
        if (is_long_double(l.type)) {
            if (op == IR_OP_GE || op == IR_OP_GT) {
                emit_r_(INSTR_FXCH, reg(xmm0, 16));
            }
            emit_r_(INSTR_FUCOMIP, reg(xmm0, 16));
            emit_r_(INSTR_FSTP, reg(xmm1, 16));
            assert(x87_stack == 2);
            x87_stack = 0;
        } else {
            assert(is_float(l.type) || is_double(l.type));
            emit_rr(INSTR_UCOMIS, reg(xmm1, w), reg(xmm0, w));
        }
    } else {
        assert(w == size_of(r.type));
        if (l.kind == IMMEDIATE && w < 8) {
            if (r.kind == DIRECT
                && !is_global_offset(r.value.symbol)
                && !is_field(r))
            {
                if ((ax = allocated_register(r)) != 0) {
                    emit_ir(INSTR_CMP, value_of(l, w), reg(ax, w));
                } else {
                    emit_im(INSTR_CMP, value_of(l, w), location_of(r, w));
                }
            } else {
                ax = load_cast(r, r.type);
                emit_ir(INSTR_CMP, value_of(l, w), reg(ax, w));
            }
            switch (cc) {
            case CC_AE:
                cc = CC_NA;
                break;
            case CC_GE:
                cc = CC_NG;
                break;
            case CC_A:
                cc = CC_NAE;
                break;
            case CC_G:
                cc = CC_NGE;
                break;
            default: break;
            }
        } else if (r.kind == IMMEDIATE && w < 8) {
            if (l.kind == DIRECT
                && !is_global_offset(l.value.symbol)
                && !is_field(l))
            {
                if ((ax = allocated_register(l)) != 0) {
                    emit_ir(INSTR_CMP, value_of(r, w), reg(ax, w));
                } else {
                    emit_im(INSTR_CMP, value_of(r, w),
                        location_of(l, w));
                }
            } else {
                ax = load_cast(l, l.type);
                emit_ir(INSTR_CMP, value_of(r, w), reg(ax, w));
            }
        } else {
            if (l.kind == DIRECT
                && !is_global_offset(l.value.symbol)
                && !is_field(l))
            {
                if ((ax = allocated_register(l)) != 0) {
                    ax = load_cast(l, l.type);
                    cx = load_cast(r, r.type);
                    emit_rr(INSTR_CMP, reg(cx, w), reg(ax, w));
                } else {
                    ax = load_cast(r, r.type);
                    emit_rm(INSTR_CMP, reg(ax, w), location_of(l, w));
                }
            } else if (r.kind == DIRECT
                && !is_global_offset(r.value.symbol)
                && !is_field(r))
            {
                if ((ax = allocated_register(r)) != 0) {
                    ax = load_cast(l, l.type);
                    cx = load_cast(r, r.type);
                    emit_rr(INSTR_CMP, reg(cx, w), reg(ax, w));
                } else {
                    ax = load_cast(l, l.type);
                    emit_mr(INSTR_CMP, location_of(r, w), reg(ax, w));
                }
            } else {
                ax = load_cast(l, l.type);
                cx = load_cast(r, r.type);
                emit_rr(INSTR_CMP, reg(cx, w), reg(ax, w));
            }
        }
    }

    return cc;
}

/*
 * Compare variables for equality as expression operands.
 *
 * Do not consider type, as this can be changed during evaluation, for
 * example addition of pointer types being treated as long.
 */
static int operand_equal(struct var a, struct var b)
{
    return a.is_symbol
        && b.is_symbol
        && a.value.symbol == b.value.symbol
        && a.kind == b.kind
        && a.field_width == b.field_width
        && a.field_offset == b.field_offset
        && a.offset == b.offset;
}

static enum reg compile_add(
    struct var target,
    Type type,
    struct var l,
    struct var r)
{
    size_t w;
    enum reg ax, cx;

    w = size_of(type);
    if (is_long_double(type)) {
        ax = load_cast(l, type);
        load_cast(r, type);
        emit_r_(INSTR_FADDP, reg(ax, w));
        assert(x87_stack == 2);
        x87_stack--;
    } else if (is_real(type)) {
        ax = load_cast(l, type);
        cx = load_cast(r, type);
        emit_rr(INSTR_ADDS, reg(cx, w), reg(ax, w));
    } else {
        if (!is_void(target.type)
            && target.kind == DIRECT
            && !is_global_offset(target.value.symbol)
            && !is_field(target))
        {
            ax = AX;
            if (operand_equal(target, r)) {
                if (is_int_constant(l)) {
                    if ((cx = allocated_register(r)) != 0) {
                        emit_rr(INSTR_ADD, reg(ax, w), reg(cx, w));
                        ax = cx;
                    } else {
                        emit_im(INSTR_ADD,
                            value_of(l, w), location_of(target, w));
                    }
                } else {
                    ax = load_cast(l, type);
                    if ((cx = allocated_register(r)) != 0) {
                        emit_rr(INSTR_ADD, reg(ax, w), reg(cx, w));
                        ax = cx;
                    } else {
                        emit_rm(INSTR_ADD, reg(ax, w), location_of(target, w));
                    }
                }
                return ax;
            } else if (operand_equal(target, l)) {
                if (is_int_constant(r)) {
                    if ((cx = allocated_register(l)) != 0) {
                        emit_rr(INSTR_ADD, reg(ax, w), reg(cx, w));
                        ax = cx;
                    } else {
                        emit_im(INSTR_ADD,
                            value_of(r, w), location_of(target, w));
                    }
                } else {
                    ax = load_cast(r, type);
                    if ((cx = allocated_register(l)) != 0) {
                        emit_rr(INSTR_ADD, reg(ax, w), reg(cx, w));
                        ax = cx;
                    } else {
                        emit_rm(INSTR_ADD, reg(ax, w), location_of(target, w));
                    }
                }
                return ax;
            }
        }
        if (is_int_constant(l)) {
            ax = load_cast(r, type);
            emit_ir(INSTR_ADD, value_of(l, w), reg(ax, w));
        } else if (is_int_constant(r)) {
            ax = load_cast(l, type);
            emit_ir(INSTR_ADD, value_of(r, w), reg(ax, w));
        } else if (l.kind == DIRECT
            && !is_global_offset(l.value.symbol)
            && !is_field(l))
        {
            ax = load_cast(r, type);
            if ((cx = allocated_register(l)) != 0) {
                emit_rr(INSTR_ADD, reg(cx, w), reg(ax, w));
            } else {
                emit_mr(INSTR_ADD, location_of(l, w), reg(ax, w));
            }
        } else if (r.kind == DIRECT
            && !is_global_offset(r.value.symbol)
            && !is_field(r))
        {
            ax = load_cast(l, type);
            if ((cx = allocated_register(r)) != 0) {
                emit_rr(INSTR_ADD, reg(cx, w), reg(ax, w));
            } else {
                emit_mr(INSTR_ADD, location_of(r, w), reg(ax, w));
            }
        } else {
            ax = load_cast(l, type);
            cx = load_cast(r, type);
            emit_rr(INSTR_ADD, reg(cx, w), reg(ax, w));
        }
    }

    if (!is_void(target.type)) {
        store(ax, target);
    }

    return ax;
}

static enum reg compile_sub(
    struct var target,
    Type type,
    struct var l,
    struct var r)
{
    size_t w;
    enum opcode opc;
    enum reg ax, cx;

    w = size_of(type);
    if (is_long_double(type)) {
        ax = load_cast(l, type);
        load_cast(r, type);
        emit_r_(INSTR_FSUBRP, reg(ax, w));
        assert(x87_stack == 2);
        x87_stack--;
        if (!is_void(target.type)) {
            store_x87(target);
        }
    } else {
        ax = load_cast(l, type);
        cx = load_cast(r, type);
        opc = (is_float(type) || is_double(type)) ? INSTR_SUBS : INSTR_SUB;
        emit_rr(opc, reg(cx, w), reg(ax, w));
        if (!is_void(target.type)) {
            store(ax, target);
        }
    }

    return ax;
}

static enum reg compile_not(
    struct var target,
    struct var l)
{
    enum reg ax;

    ax = load(l, AX);
    emit_r_(INSTR_NOT, reg(AX, size_of(l.type)));
    if (!is_void(target.type)) {
        store(ax, target);
    }

    return ax;
}

static enum reg compile_neg(
    struct var target,
    struct var l)
{
    static struct symbol *c32, *c64;

    enum reg xmm0, xmm1;
    struct var val;
    union value num = {0};

    if (is_float(l.type)) {
        if (!c32) {
            num.u = 1u << 31;
            c32 = sym_create_constant(basic_type__float, num);
        }
        val = var_direct(c32);
    } else {
        assert(is_double(l.type));
        if (!c64) {
            num.u = 1ul << 63;
            c64 = sym_create_constant(basic_type__double, num);
        }
        val = var_direct(c64);
    }

    assert(size_of(l.type) == size_of(val.type));
    xmm0 = load_cast(val, val.type);
    xmm1 = load_cast(l, l.type);
    emit_rr(INSTR_PXOR, reg(xmm1, 8), reg(xmm0, 8));
    if (!is_void(target.type)) {
        store(xmm0, target);
    }

    return xmm0;
}

static enum reg compile_mul(
    struct var target,
    Type type,
    struct var l,
    struct var r)
{
    size_t w;
    enum reg ax, cx;

    w = size_of(type);
    if (is_real(type)) {
        ax = load_cast(l, type);
        cx = load_cast(r, type);
        if (is_long_double(type)) {
            emit_r_(INSTR_FMULP, reg(ax, w));
            assert(x87_stack == 2);
            x87_stack--;
            if (!is_void(target.type)) {
                store_x87(target);
            }
        } else {
            emit_rr(INSTR_MULS, reg(cx, w), reg(ax, w));
            if (!is_void(target.type)) {
                store(ax, target);
            }
        }
    } else {
        ax = load_cast(r, r.type);
        assert(ax == AX);
        if (l.kind == DIRECT
            && !is_register_allocated(l)
            && !is_global_offset(l.value.symbol))
        {
            emit_m_(INSTR_MUL, location_of(l, w));
        } else {
            cx = load_cast(l, l.type);
            emit_r_(INSTR_MUL, reg(cx, w));
        }
        if (!is_void(target.type)) {
            store(ax, target);
        }
    }

    return ax;
}

static enum reg compile_div(
    struct var target,
    Type type,
    struct var l,
    struct var r)
{
    int w;
    enum opcode opc;
    enum reg ax, cx;

    if (is_real(type)) {
        w = size_of(l.type);
        ax = load_cast(l, type);
        cx = load_cast(r, type);
        if (is_long_double(type)) {
            emit_r_(INSTR_FDIVRP, reg(ax, w));
            assert(x87_stack == 2);
            x87_stack--;
            if (!is_void(target.type)) {
                store_x87(target);
            }
        } else {
            emit_rr(INSTR_DIVS, reg(cx, w), reg(ax, w));
            if (!is_void(target.type)) {
                store(ax, target);
            }
        }
    } else {
        ax = load_cast(l, l.type);
        assert(ax == AX);
        if (is_signed(l.type)) {
            w = size_of(l.type);
            assert(w == 8 || w == 4);
            emit_cxy(w);
        } else {
            emit_rr(INSTR_XOR, reg(DX, 8), reg(DX, 8));
        }
        opc = is_signed(type) ? INSTR_IDIV : INSTR_DIV;
        if (r.kind == DIRECT
            && !is_global_offset(r.value.symbol)
            && !is_register_allocated(r)
            && !is_field(r))
        {
            emit_m_(opc, location_of(r, size_of(r.type)));
        } else {
            cx = load_cast(r, r.type);
            emit_r_(opc, reg(cx, size_of(r.type)));
        }
        if (!is_void(target.type)) {
            store(ax, target);
        }
    }

    return ax;
}

/* Divident is %rdx:%rax, zero extend if type is signed. */
static enum reg compile_mod(
    struct var target,
    Type type,
    struct var l,
    struct var r)
{
    enum opcode opc;
    enum reg ax;
    int w;
    assert(!is_real(type));

    ax = load_cast(l, l.type);
    assert(ax == AX);
    if (is_signed(l.type)) {
        w = size_of(l.type);
        assert(w == 8 || w == 4);
        emit_cxy(w);
    } else {
        emit_rr(INSTR_XOR, reg(DX, 8), reg(DX, 8));
    }

    opc = is_signed(type) ? INSTR_IDIV : INSTR_DIV;
    if (r.kind == DIRECT
        && !is_register_allocated(r)
        && !is_global_offset(r.value.symbol)
        && !is_field(r))
    {
        emit_m_(opc, location_of(r, size_of(r.type)));
    } else {
        ax = load_cast(r, r.type);
        emit_r_(opc, reg(ax, size_of(r.type)));
    }

    if (!is_void(target.type)) {
        store(DX, target);
    }

    return DX;
}

static enum reg compile_bitwise(
    struct var target,
    enum opcode opcode,
    struct var l,
    struct var r)
{
    size_t w;
    enum reg ax;

    w = size_of(l.type);
    assert(size_of(r.type) == w);
    if (r.kind == IMMEDIATE && w < 8) {
        ax = load(l, AX);
        emit_ir(opcode, value_of(r, w), reg(ax, w));
    } else if (l.kind == IMMEDIATE && w < 8) {
        ax = load(r, AX);
        emit_ir(opcode, value_of(l, w), reg(ax, w));
    } else {
        ax = load(l, AX);
        load(r, CX);
        emit_rr(opcode, reg(CX, w), reg(ax, w));
    }

    if (!is_void(target.type)) {
        store(ax, target);
    }

    return ax;
}

/*
 * Shift instruction encoding is either by immediate, or implicit %cl
 * register. Encode as if something other than %cl could be chosen.
 * 
 * Behavior is undefined if shift is greater than integer width, so
 * don't care about overflow or sign.
 */
static enum reg compile_shl(
    struct var target,
    struct var l,
    struct var r)
{
    load(l, AX);
    load(r, CX);
    emit_rr(INSTR_SHL, reg(CX, 1), reg(AX, size_of(l.type)));
    if (!is_void(target.type)) {
        store(AX, target);
    }

    return AX;
}

static enum reg compile_shr(
    struct var target,
    struct var l,
    struct var r)
{
    load(l, AX);
    load(r, CX);
    if (is_unsigned(l.type)) {
        emit_rr(INSTR_SHR, reg(CX, 1), reg(AX, size_of(l.type)));
    } else {
        emit_rr(INSTR_SAR, reg(CX, 1), reg(AX, size_of(l.type)));
    }

    if (!is_void(target.type)) {
        store(AX, target);
    }

    return AX;
}

/*
 * Store result of comparing two values of given type. Instruction used
 * for compare is used to determine special handling for floating point
 * equality.
 *
 * This is called after a comparison expression is evaluated, and the
 * result is assigned to a variable. In other cases, a jump can be
 * emitted without ever computing the integer 0 or 1 result.
 */
static enum reg set_compare_value(Type type, enum tttn cc)
{
    emit_setcc(cc, reg(AX, 1));
    if (is_real(type)) {
        if (cc == CC_E) {
            emit_setcc(CC_NP, reg(CX, 1));
            emit_rr(INSTR_AND, reg(CX, 1), reg(AX, 1));
        } else if (cc == CC_NE) {
            emit_setcc(CC_P, reg(CX, 1));
            emit_rr(INSTR_OR, reg(CX, 1), reg(AX, 1));
        }
        emit_ir(INSTR_AND, constant(1, 1), reg(AX, 1));
    }

    emit_rr(INSTR_MOVZX, reg(AX, 1), reg(AX, 4));
    return AX;
}

/*
 * Write object from address returned by evaluating an expression. Used
 * for assignment of objects that do not fit in a single register.
 *
 * Handle special case of char [] = string literal. This will only occur
 * as part of initializer, at block scope. External definitions are
 * handled before this. At no other point should array types be seen in
 * assembly backend.
 */
static void store_copy_object(struct var var, struct var target)
{
    size_t b;

    b = size_of(var.type);
    if (is_array(var.type)) {
        assert(target.kind == DIRECT);
        assert(var.is_symbol);
        assert(var.value.symbol->symtype == SYM_LITERAL);
        assert(type_equal(target.type, var.type));
        emit_mr(INSTR_LEA, location_of(var, 8), reg(SI, 8));
    } else {
        load_address(var, SI);
    }

    load_address(target, DI);
    emit_memcpy(b);
}

static enum reg compile_cast(
    struct var target,
    Type type,
    struct var l)
{
    size_t w;
    enum reg ax;
    union operand op;
    struct param_class pc;

    w = size_of(type);
    if (!is_standard_register_width(w) && w < 8) {
        /* Do not bother masking adjacent bits read. */
        switch (w) {
        default: assert(0);
        case 3:
            l.type = basic_type__unsigned_int;
            ax = load_cast(l, l.type);
            break;
        case 5:
        case 6:
        case 7:
            l.type = basic_type__unsigned_long;
            ax = load_cast(l, l.type);
            break;
        }
        if (!is_void(target.type)) {
            /* All variables have at least 8 byte slots. */
            target.type = l.type;
            store(ax, target);
        }
        return ax;
    }

    pc = classify(type);
    switch (pc.eightbyte[0]) {
    default: assert(0);
    case PC_X87:
        if (is_struct_or_union(l.type)) {
            l.type = basic_type__long_double;
        }
        ax = load_x87(l);
        if (!is_void(target.type)) {
            target.type = basic_type__long_double;
            store(ax, target);
            assert(x87_stack == 0);
        }
        break;
    case PC_INTEGER:
    case PC_SSE:
        if (pc.eightbyte[1] == PC_NO_CLASS) {
            if (is_struct_or_union(type)) {
                type = slice_type(type, pc, 0);
                l.type = type;
            }
            if (!is_void(target.type)) {
                target.type = type;
                if (type_equal(l.type, type)) {
                    if (is_int_constant(l)) {
                        op.imm = value_of(l, w);
                        store_op(OPT_IMM, op, target);
                        ax = AX;
                        break;
                    } else if ((ax = allocated_register(l)) != 0) {
                        op.reg = reg(ax, w);
                        store_op(OPT_REG, op, target);
                        break;
                    }
                }
                ax = load_cast(l, type);
                store(ax, target);
            } else {
                ax = load_cast(l, type);
            }
            break;
        }
    case PC_MEMORY:
        assert(!is_void(target.type));
        store_copy_object(l, target);
        ax = AX;
        break;
    }

    return ax;
}

static enum reg compile_assign(struct var target, struct expression expr)
{
    enum reg ax;
    enum tttn cc;

    switch (expr.op) {
    default: assert(0);
    case IR_OP_CAST:
        ax = compile_cast(target, expr.type, expr.l);
        break;
    case IR_OP_CALL:
        ax = compile_call(target, expr.l);
        break;
    case IR_OP_VA_ARG:
        compile__builtin_va_arg(target, expr.l);
        ax = AX;
        break;
    case IR_OP_NOT:
        ax = compile_not(target, expr.l);
        break;
    case IR_OP_NEG:
        ax = compile_neg(target, expr.l);
        break;
    case IR_OP_ADD:
        ax = compile_add(target, expr.type, expr.l, expr.r);
        break;
    case IR_OP_SUB:
        ax = compile_sub(target, expr.type, expr.l, expr.r);
        break;
    case IR_OP_MUL:
        ax = compile_mul(target, expr.type, expr.l, expr.r);
        break;
    case IR_OP_DIV:
        ax = compile_div(target, expr.type, expr.l, expr.r);
        break;
    case IR_OP_MOD:
        ax = compile_mod(target, expr.type, expr.l, expr.r);
        break;
    case IR_OP_AND:
        ax = compile_bitwise(target, INSTR_AND, expr.l, expr.r);
        break;
    case IR_OP_OR:
        ax = compile_bitwise(target, INSTR_OR, expr.l, expr.r);
        break;
    case IR_OP_XOR:
        ax = compile_bitwise(target, INSTR_XOR, expr.l, expr.r);
        break;
    case IR_OP_SHL:
        ax = compile_shl(target, expr.l, expr.r);
        break;
    case IR_OP_SHR:
        ax = compile_shr(target, expr.l, expr.r);
        break;
    case IR_OP_EQ:
    case IR_OP_NE:
    case IR_OP_GE:
    case IR_OP_GT:
        cc = compile_compare(expr.op, expr.l, expr.r);
        ax = set_compare_value(expr.l.type, cc);
        if (!is_void(target.type)) {
            store(ax, target);
        }
        break;
    }

    return ax;
}

static enum reg compile_expression(struct expression expr)
{
    struct var target = {0};
    assert(is_void(target.type));
    assert(expr.op != IR_OP_VA_ARG);
    return compile_assign(target, expr);
}

/*
 * Allocate a variable length array, storing the address of the first
 * element to sym->vla_address.
 */
static void compile_vla_alloc(
    const struct symbol *sym,
    struct expression size)
{
    enum reg ax;
    assert(is_vla(sym->type));

    /* Subtract aligned variable length from %rsp. */
    ax = compile_expression(size);
    emit_rr(INSTR_SUB, reg(ax, 8), reg(SP, 8));
    emit_ir(INSTR_MOV, constant(-16, 8), reg(R11, 8));
    emit_rr(INSTR_AND, reg(R11, 8), reg(SP, 8));

    /* Assign current stack location to VLA symbol. */
    store(SP, var_direct(sym->value.vla_address));
}

static void compile__asm(struct asm_statement st)
{
    int i;
    enum reg r;
    struct asm_operand op;

    /* Put all variables in register according to constraint. */
    for (i = 0; i < array_len(&st.operands); ++i) {
        op = array_get(&st.operands, i);
        if (!str_has_chr(op.constraint, '=')
            && explicit_reg_constraint(op.constraint, &r))
        {
            load(op.variable, r);
        }
    }

    assemble_inline(st, emit_instruction);

    /* Store variables from register constraint. */
    for (i = 0; i < array_len(&st.operands); ++i) {
        op = array_get(&st.operands, i);
        if ((str_has_chr(op.constraint, '=') || str_has_chr(op.constraint, '+'))
            && explicit_reg_constraint(op.constraint, &r))
        {
            store(r, op.variable);
        }
    }
}

static void compile_statement(struct statement stmt)
{
    switch (stmt.st) {
    case IR_PARAM:
        assert(is_identity(stmt.expr));
        array_push_back(&func_args, stmt.expr.l);
        break;
    case IR_VA_START:
        assert(is_identity(stmt.expr));
        compile__builtin_va_start(stmt.expr.l);
        break;
    case IR_EXPR:
        stmt.t.type = basic_type__void;
    case IR_ASSIGN:
        compile_assign(stmt.t, stmt.expr);
        break;
    case IR_VLA_ALLOC:
        assert(stmt.t.kind == DIRECT);
        assert(stmt.t.is_symbol);
        compile_vla_alloc(stmt.t.value.symbol, stmt.expr);
        break;
    case IR_ASM:
        compile__asm(array_get(&definition->asm_statements, stmt.asm_index));
        break;
    }

    relase_regs();
    assert(x87_stack == 0);
}

/*
 * Return value from function, placing it in register(s) or writing it
 * to stack, based on parameter class.
 *
 * Load return address from magic stack offset and copy result. Return
 * address is stored in -8(%rbp), unless the function takes variable
 * argument lists, in which case it is read from register spill area.
 *
 * The ABI specifies that the address of a returned object should be
 * placed in %rax.
 */
static void compile_return(Type func, struct expression expr)
{
    size_t w;
    enum reg ax;
    Type type;
    const struct symbol *label;
    struct param_class pc;

    type = type_next(func);
    pc = classify(type);
    w = size_of(type);

    switch (pc.eightbyte[0]) {
    default: assert(0);
    case PC_INTEGER:
        if (EIGHTBYTES(type) == 1) {
            ax = compile_expression(expr);
            if (ax != ret_int_reg[0]) {
                assert(is_standard_register_width(w));
                emit_rr(INSTR_MOV,
                    reg(ax, w), reg(ret_int_reg[0], w));
            }
        } else {
            assert(is_identity(expr));
            load_object_to_registers(expr.l, pc, ret_int_reg, ret_sse_reg);
        }
        break;
    case PC_SSE:
        if (EIGHTBYTES(type) == 1) {
            ax = compile_expression(expr);
            if (ax != ret_sse_reg[0]) {
                assert(w == 8 || w == 4);
                emit_rr(INSTR_MOV,
                    reg(ax, w), reg(ret_sse_reg[0], w));
            }
        } else {
            assert(is_identity(expr));
            load_object_to_registers(expr.l, pc, ret_int_reg, ret_sse_reg);
        }
        break;
    case PC_X87:
        assert(pc.eightbyte[1] == PC_X87UP);
        compile_expression(expr);
        assert(x87_stack == 1);
        x87_stack = 0; /* Reset before next function. */
        break;
    case PC_MEMORY:
        assert(is_identity(expr));
        label = create_label(definition);
        load_address(expr.l, SI);
        emit_mr(INSTR_MOV,
            location(address(return_address_offset, BP, 0, 0), 8), reg(DI, 8));
        emit_rr(INSTR_CMP, reg(DI, 8), reg(SI, 8));
        emit_jcc(CC_E, addr(label));
        emit_memcpy(w);
        emit_mr(INSTR_MOV,
            location(address(return_address_offset, BP, 0, 0), 8), reg(AX, 8));
        enter_context(label);
        break;
    }

    assert(x87_stack == 0);
}

/*
 * Emit code for all statements in a block, jump to children based on
 * compare result, or return value in case of no children.
 *
 * Most of the complexity deals with interpreting the last block->expr
 * object, branchhing to the correct next block. All scalar expressions
 * are allowed.
 */
static void compile_block(struct block *block, Type type)
{
    int i, w;
    enum reg ax;
    enum reg xmm0, xmm1;
    enum tttn cc;
    struct statement st;
    struct immediate br0, br1;

    assert(is_function(type));
    if (block->color == BLACK)
        return;

    block->color = BLACK;
    enter_context(block->label);
    for (i = 0; i < array_len(&block->code); ++i) {
        st = array_get(&block->code, i);
        compile_statement(st);
    }

    if (!block->jump[0] && !block->jump[1]) {
        if (block->has_return_value) {
            assert(is_object(block->expr.type));
            assert(type_equal_unqualified(block->expr.type, type_next(type)));
            compile_return(type, block->expr);
            relase_regs();
            assert(x87_stack == 0);
        }
        if (int_regs_alloc) {
            emit_mr(INSTR_LEA,
                location(address(-int_regs_alloc * 8, BP, 0, 0), 8),
                reg(SP, 8));
            for (i = int_regs_alloc; i > 0; --i) {
                emit_r_(INSTR_POP, reg(temp_int_reg[i - 1], 8));
            }
        }
        emit_(INSTR_LEAVE);
        emit_(INSTR_RET);
    } else if (!block->jump[1]) {
        if (block->jump[0]->color == BLACK) {
            emit_i_(INSTR_JMP, addr(block->jump[0]->label));
        } else {
            compile_block(block->jump[0], type);
        }
    } else {
        assert(block->jump[0]);
        assert(block->jump[1]);
        assert(is_scalar(block->expr.type));
        br0 = addr(block->jump[0]->label);
        br1 = addr(block->jump[1]->label);
        if (is_comparison(block->expr)) {
            cc = compile_compare(block->expr.op, block->expr.l, block->expr.r);
            switch (cc) {
            default: assert(0);
            case CC_E:
                if (is_real(block->expr.l.type)) {
                    emit_jcc(CC_NE, br0);
                    emit_jcc(CC_P, br0);
                    emit_i_(INSTR_JMP, br1);
                } else {
                    emit_jcc(CC_NE, br0);
                }
                break;
            case CC_NE:
                if (is_real(block->expr.l.type)) {
                    emit_jcc(CC_NE, br1);
                    emit_jcc(CC_P, br1);
                    emit_i_(INSTR_JMP, br0);
                } else {
                    emit_jcc(CC_E, br0);
                }
                break;
            case CC_G:
                emit_jcc(CC_NG, br0);
                break;
            case CC_NG:
                emit_jcc(CC_G, br0);
                break;
            case CC_A:
                emit_jcc(CC_NA, br0);
                break;
            case CC_NA:
                emit_jcc(CC_A, br0);
                break;
            case CC_GE:
                emit_jcc(CC_NGE, br0);
                break;
            case CC_NGE:
                emit_jcc(CC_GE, br0);
                break;
            case CC_AE:
                emit_jcc(CC_NAE, br0);
                break;
            case CC_NAE:
                emit_jcc(CC_AE, br0);
                break;
            }
        } else {
            ax = compile_expression(block->expr);
            w = size_of(block->expr.type);
            if (is_real(block->expr.type)) {
                if (is_long_double(block->expr.type)) {
                    assert(x87_stack == 1);
                    emit_(INSTR_FLDZ);
                    x87_stack++;
                    emit_r_(INSTR_FUCOMIP, reg(ax, 16));
                    emit_r_(INSTR_FSTP, reg(ax, 16));
                    x87_stack = 0;
                } else {
                    assert(w == 4 || w == 8);
                    xmm0 = ax;
                    xmm1 = (xmm0 == XMM0) ? XMM1 : XMM0;
                    emit_rr(INSTR_PXOR, reg(xmm1, 8), reg(xmm1, 8));
                    emit_rr(INSTR_UCOMIS, reg(xmm0, w), reg(xmm1, w));
                }

                emit_jcc(CC_NE, br1);
                emit_jcc(CC_P, br1);
                emit_i_(INSTR_JMP, br0);
            } else {
                assert(w == 1 || w == 2 || w == 4 || w == 8);
                emit_ir(INSTR_CMP, constant(0, w), reg(ax, w));
                emit_jcc(CC_E, br0);
            }
        }

        relase_regs();
        if (block->jump[1]->color == BLACK) {
            emit_i_(INSTR_JMP, br1);
        } else {
            compile_block(block->jump[1], type);
        }

        compile_block(block->jump[0], type);
    }
}

static void compile_data_assign(struct var target, struct var val)
{
    struct immediate imm = {0};
    assert(target.kind == DIRECT);
    assert(!target.field_offset);

    if (is_field(target)) {
        assert(target.field_width % 8 == 0);
        assert(val.kind == IMMEDIATE);
        assert(is_integer(val.type));
        imm.width = target.field_width / 8;
        imm.d.qword = val.value.imm.i;
    } else {
        imm.width = size_of(target.type);
        switch (val.kind) {
        case IMMEDIATE:
            assert(!is_array(target.type));
            assert(type_equal(target.type, val.type));
            assert(!val.is_symbol || val.value.symbol->symtype == SYM_CONSTANT);
            imm.type = IMM_INT;
            if (is_long_double(val.type)) {
                union {
                    long double val;
                    long arr[2];
                } cast = {0};
                imm.width = 8;
                cast.val = get_long_double(val.value.imm);
                imm.d.qword = cast.arr[0];
                emit_data(imm);
                imm.d.qword = cast.arr[1] & 0xFFFF;
            } else {
                imm.d.qword = val.value.imm.i;
            }
            break;
        case DIRECT:
            assert(val.value.symbol->symtype == SYM_LITERAL);
            imm.type = IMM_STRING;
            imm.d.string = val.value.symbol->value.string;
            break;
        default:
            assert(val.kind == ADDRESS);
            assert(val.value.symbol->linkage != LINK_NONE);
            imm.type = IMM_ADDR;
            imm.d.addr = address_of(val);
            break;
        }
    }

    emit_data(imm);
}

static void compile_data(struct definition *def)
{
    int i;
    struct statement st;

    enter_context(def->symbol);
    for (i = 0; i < array_len(&def->body->code); ++i) {
        st = array_get(&def->body->code, i);
        assert(st.st == IR_ASSIGN);
        assert(st.t.kind == DIRECT);
        assert(st.t.value.symbol == def->symbol);
        assert(is_identity(st.expr));
        compile_data_assign(st.t, st.expr.l);
    }
}

static void compile_function(struct definition *def)
{
    assert(is_function(def->symbol->type));
    enter_context(def->symbol);
    emit_r_(INSTR_PUSH, reg(BP, 8));
    emit_rr(INSTR_MOV, reg(SP, 8), reg(BP, 8));

    /* Make sure parameters and local variables are placed on stack. */
    enter(def);

    /* Recursively assemble body. */
    compile_block(def->body, def->symbol->type);
}

INTERNAL void set_compile_target(FILE *stream, const char *file)
{
    switch (context.target) {
    default: assert(0);
    case TARGET_IR_DOT:
        dot_init(stream);
        break;
    case TARGET_x86_64_ASM:
        asm_init(stream, file);
        enter_context = asm_symbol;
        emit_instruction = asm_text;
        emit_data = asm_data;
        flush_backend = asm_flush;
        break;
    case TARGET_x86_64_OBJ:
    case TARGET_x86_64_EXE:
        elf_init(stream, file);
        enter_context = elf_symbol;
        emit_instruction = elf_text;
        emit_data = elf_data;
        flush_backend = elf_flush;
        finalize_backend = elf_finalize;
        break;
    }
}

INTERNAL int compile(struct definition *def)
{
    assert(decl_memcpy);
    assert(def->symbol);
    assert(x87_stack == 0);

    definition = def;
    switch (context.target) {
    case TARGET_IR_DOT:
        dotgen(def);
    case TARGET_PREPROCESS:
        break;
    case TARGET_x86_64_ASM:
    case TARGET_x86_64_OBJ:
    case TARGET_x86_64_EXE:
        if (is_function(def->symbol->type)) {
            compile_function(def);
            if (context.target != TARGET_x86_64_ASM) {
                elf_flush_text_displacements();
            }
        } else {
            compile_data(def);
        }
        break;
    }

    assert(x87_stack == 0);
    return 0;
}

INTERNAL int declare(const struct symbol *sym)
{
    switch (context.target) {
    case TARGET_x86_64_ASM:
    case TARGET_x86_64_OBJ:
    case TARGET_x86_64_EXE:
        return enter_context(sym);
    default:
        return 0;
    }
}

INTERNAL void flush(void)
{
    array_empty(&func_args);
    if (flush_backend) {
        flush_backend();
    }

    x87_unsigned_adjust_constant = NULL;
}

INTERNAL void finalize(void)
{
    array_clear(&func_args);
    if (finalize_backend) {
        finalize_backend();
    }
}
