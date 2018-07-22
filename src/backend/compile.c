#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "compile.h"
#include "graphviz/dot.h"
#include "x86_64/abi.h"
#include "x86_64/assemble.h"
#include "x86_64/elf.h"
#include "x86_64/instr.h"
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
 */
#define TEMP_INT_REGS (sizeof(temp_int_reg) / sizeof(temp_int_reg[0]))

static enum reg
    temp_int_reg[] = {BX, R12, R13, R14, R15},
    param_int_reg[] = {DI, SI, DX, CX, R8, R9},
    param_sse_reg[] = {XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7},
    ret_int_reg[] = {AX, DX},
    ret_sse_reg[] = {XMM0, XMM1};

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
int x87_stack_pos(enum reg r)
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

static void compile_block(struct block *block, Type type, int stack);

static void emit(enum opcode opcode, enum instr_optype optype, ...)
{
    va_list args;
    struct instruction instr = {0};

    instr.opcode = opcode;
    instr.optype = optype;
    va_start(args, optype);
    switch (optype) {
    case OPT_IMM:
        instr.source.imm = va_arg(args, struct immediate);
        break;
    case OPT_REG:
        instr.source.reg = va_arg(args, struct registr);
        break;
    case OPT_MEM:
        instr.source.mem = va_arg(args, struct memory);
        break;
    case OPT_REG_REG:
        instr.source.reg = va_arg(args, struct registr);
        instr.dest.reg = va_arg(args, struct registr);
        break;
    case OPT_REG_MEM:
        instr.source.reg = va_arg(args, struct registr);
        instr.dest.mem = va_arg(args, struct memory);
        break;
    case OPT_MEM_REG:
        instr.source.mem = va_arg(args, struct memory);
        instr.dest.reg = va_arg(args, struct registr);
        break;
    case OPT_IMM_REG:
        instr.source.imm = va_arg(args, struct immediate);
        instr.dest.reg = va_arg(args, struct registr);
        break;
    case OPT_IMM_MEM:
        instr.source.imm = va_arg(args, struct immediate);
        instr.dest.mem = va_arg(args, struct memory);
        break;
    case OPT_NONE:
        break;
    }

    va_end(args);
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
    v.w = w;
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
        && is_temporary(v.symbol)
        && v.symbol->slot != 0;
}

static enum reg allocated_register(struct var v)
{
    if (is_register_allocated(v)) {
        assert(is_integer(v.symbol->type) || is_pointer(v.symbol->type));
        return temp_int_reg[v.symbol->slot - 1];
    }

    return 0;
}

static struct immediate value_of(struct var var, int w)
{
    struct immediate imm = {0};

    assert(var.kind == IMMEDIATE);
    assert(!is_string(var));
    assert(!var.offset);
    assert(is_scalar(var.type));
    assert(w == 1 || w == 2 || w == 4 || w == 8);

    imm.w = w;
    imm.type = IMM_INT;
    imm.d.qword = var.imm.i;
    return imm;
}

static struct memory location(struct address addr, int w)
{
    struct memory loc = {0};
    loc.addr = addr;
    loc.w = w;
    return loc;
}

static struct address address_of(struct var var)
{
    struct address addr = {0};
    assert(var.kind != DEREF);

    addr.disp = displacement_from_offset(var.offset);
    if (var.kind == IMMEDIATE) {
        assert(is_string(var));
        addr.base = IP;
        addr.sym = var.symbol;
    } else {
        assert(var.kind == DIRECT || var.kind == ADDRESS);
        switch (var.symbol->linkage) {
        case LINK_EXTERN:
            assert(!context.pic);
        case LINK_INTERN:
            addr.base = IP;
            addr.disp = displacement_from_offset(var.offset);
            addr.sym = var.symbol;
            break;
        case LINK_NONE:
            addr.disp += var.symbol->stack_offset;
            addr.base = BP;
            break;
        }
    }

    return addr;
}

static struct memory location_of(struct var var, int w)
{
    assert(!is_register_allocated(var));
    return location(address_of(var), w);
}

static struct address address(int disp, enum reg base, enum reg off, int mult)
{
    struct address addr = {0};
    addr.disp = disp;
    addr.base = base;
    addr.offset = off;
    addr.mult = mult;
    return addr;
}

static struct address got(const struct symbol *sym)
{
    struct address addr = {0};
    assert(is_global_offset(sym));

    addr.type = ADDR_GLOBAL_OFFSET;
    addr.base = IP;
    addr.sym = sym;
    return addr;
}

static struct immediate addr(const struct symbol *sym)
{
    struct immediate imm = {IMM_ADDR};
    imm.d.addr.sym = sym;
    if (is_global_offset(sym)) {
        if (is_function(sym->type)) {
            imm.d.addr.type = ADDR_PLT;
        } else {
            imm.d.addr.type = ADDR_GLOBAL_OFFSET;
        }
    } else if (sym->symtype != SYM_LABEL && !is_function(sym->type)) {
        imm.d.addr.base = IP;
    }

    return imm;
}

static struct immediate constant(long n, int w)
{
    struct immediate imm = {0};
    assert(w == 1 || w == 2 || w == 4 || w == 8);
    imm.type = IMM_INT;
    imm.d.qword = n;
    imm.w = w;
    return imm;
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
        return val.ld == 0;
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
    size_t w;

    w = size_of(source.type);
    assert(is_standard_register_width(w));
    switch (source.kind) {
    case IMMEDIATE:
        /*
         * There is no suitable instruction for moving an immediate
         * floating point value directly to register. Need to store it
         * in memory first, so create symbol holding constant value.
         */
        if (is_real(source.type)) {
            if (!source.symbol) {
                source.symbol = sym_create_constant(source.type, source.imm);
            }
            source.kind = DIRECT;
        } else if (is_zero(source.imm, source.type)) {
            assert(!is_long_double(source.type));
            emit(is_real(source.type) ? INSTR_PXOR : INSTR_XOR,
                OPT_REG_REG,
                dest,
                dest);
            break;
        } else if (is_string(source)) {
            assert(dest.w == 8);
            assert(opcode == INSTR_MOV);
            emit(INSTR_LEA, OPT_MEM_REG, address_of(source), dest);
            break;
        } else {
            assert(opcode == INSTR_MOV);
            emit(opcode, OPT_IMM_REG, value_of(source, dest.w), dest);
            break;
        }
    case DIRECT:
        if (is_register_allocated(source)) {
            ax = allocated_register(source);
            if (opcode == INSTR_MOV) {
                w = dest.w;
            }
            emit(opcode, OPT_REG_REG, reg(ax, w), dest);
        } else if (is_global_offset(source.symbol)) {
            ax = dest.r < XMM0 ? dest.r : R11;
            emit(INSTR_MOV, OPT_MEM_REG,
                location(got(source.symbol), 8), reg(ax, 8));
            emit(opcode, OPT_MEM_REG, location(address(
                displacement_from_offset(source.offset), ax, 0, 0), w), dest);
        } else {
            emit(opcode, OPT_MEM_REG, location_of(source, w), dest);
        }
        break;
    case DEREF:
        ptr = var_direct(source.symbol);
        if (is_string(ptr)) {
            ptr.offset = source.offset;
            emit(opcode, OPT_MEM_REG, location_of(ptr, w), dest);
        } else {
            assert(is_pointer(ptr.type));
            if (is_register_allocated(ptr)) {
                ax = allocated_register(ptr);
            } else if (is_global_offset(source.symbol)) {
                ax = dest.r < XMM0 ? dest.r : R11;
                emit(INSTR_MOV, OPT_MEM_REG,
                    location(got(source.symbol), 8), reg(ax, 8));
                emit(INSTR_MOV, OPT_MEM_REG,
                    location(address(0, ax, 0, 0), w),
                    reg(ax, 8));
            } else {
                ax = R11;
                emit(INSTR_MOV, OPT_MEM_REG, location_of(ptr, 8), reg(ax, 8));
            }
            emit(opcode,
                OPT_MEM_REG,
                location(
                    address(
                        displacement_from_offset(source.offset), ax, 0, 0), w),
                dest);
        }
        break;
    case ADDRESS:
        assert(opcode == INSTR_LEA);
        assert(dest.w == 8);
        if (is_global_offset(source.symbol)) {
            ax = dest.r;
            emit(INSTR_MOV, OPT_MEM_REG, location(got(source.symbol), 8), dest);
            if (source.offset) {
                emit(INSTR_LEA, OPT_MEM_REG,
                    location(address(
                        displacement_from_offset(source.offset), ax, 0, 0), 8),
                    dest);
            }
        } else {
            emit(opcode, OPT_MEM_REG, location_of(source, 8), dest);
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
    enum opcode opcode;

    assert(is_real(val.type));
    assert(w == 4 || w == 8);
    assert(r >= XMM0 && r <= XMM7);

    opcode =
        (size_of(val.type) == 4 && w == 8) ? INSTR_CVTSS2SD :
        (size_of(val.type) == 8 && w == 4) ? INSTR_CVTSD2SS :
        (w == 4) ? INSTR_MOVSS : INSTR_MOVSD;

    emit_load(opcode, val, reg(r, w));
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
    bits = (ax.w * 8) - (v.field_offset + v.field_width);
    if (bits > 0) {
        emit(INSTR_SHL, OPT_IMM_REG, constant(bits, 1), ax);
    }

    bits = v.field_offset + bits;
    if (bits > 0) {
        emit(is_signed(v.type) ? INSTR_SAR : INSTR_SHR,
            OPT_IMM_REG,
            constant(bits, 1),
            ax);
    }

    if (is_signed(v.type) && w > ax.w) {
        emit(INSTR_MOVSX, OPT_REG_REG, reg(r, 4), reg(r, 8));
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
    } else {
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
                }
            } else if (is_signed(v.type)) {
                opcode = INSTR_MOVSX;
            }
        }
        emit_load(opcode, v, reg(r, w));
    }
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
        if (is_global_offset(v.symbol)) {
            emit(INSTR_MOV, OPT_MEM_REG, location(got(v.symbol), 8), reg(r, 8));
            emit(INSTR_LEA, OPT_MEM_REG,
                location(address(
                    displacement_from_offset(v.offset), r, 0, 0), 8),
                reg(r, 8));
        } else {
            emit(INSTR_LEA, OPT_MEM_REG, location_of(v, 8), reg(r, 8));
        }
    } else {
        assert(v.kind == DEREF);
        load(var_direct(v.symbol), r);
        if (v.offset) {
            emit(INSTR_ADD, OPT_IMM_REG, constant(v.offset, 8), reg(r, 8));
        }
    }
}

/*
 * Use long double constant with value MAX_LONG + 1 in order to generate
 * code for converting 64 bit unsigned to long double.
 */
static struct var x87_unsigned_adjust_constant(void)
{
    static const struct symbol *sym;
    union value val = {0};

    if (!sym) {
        val.ld = 18446744073709551616.L;
        sym = sym_create_constant(basic_type__long_double, val);
    }

    return var_direct(sym);
}

/* Push value to stack, rounded up to always be 8 byte aligned. */
static void push(struct var v)
{
    int eb;

    if (is_long_double(v.type)) {
        if (v.kind == IMMEDIATE) {
            v.symbol = sym_create_constant(v.type, v.imm);
            v.kind = DIRECT;
        }
        if (v.kind == DIRECT) {
            v.offset += 8;
            emit(INSTR_PUSH, OPT_MEM, location_of(v, 8));
            v.offset -= 8;
            emit(INSTR_PUSH, OPT_MEM, location_of(v, 8));
        } else {
            load_address(v, SI);
            emit(INSTR_PUSH, OPT_MEM, location(address(8, SI, 0, 0), 8));
            emit(INSTR_PUSH, OPT_MEM, location(address(0, SI, 0, 0), 8));
        }
    } else if (is_scalar(v.type)) {
        if (v.kind == IMMEDIATE && size_of(v.type) < 8) {
            emit(INSTR_PUSH, OPT_IMM, value_of(v, 8));
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
            emit(INSTR_PUSH, OPT_REG, reg(AX, 8));
        }
    } else {
        eb = EIGHTBYTES(v.type);
        emit(INSTR_SUB, OPT_IMM_REG, constant(eb * 8, 8), reg(SP, 8));
        emit(INSTR_MOV, OPT_IMM_REG, constant(eb, 4), reg(CX, 4));
        emit(INSTR_MOV, OPT_REG_REG, reg(SP, 8), reg(DI, 8));
        load_address(v, SI);
        emit(INSTR_REP_MOVSQ, OPT_NONE);
    }
}

/*
 * Load value to top of x87 floating point stack, ST(0). Convert from
 * arbitrary source type.
 */
static enum reg load_x87(struct var v)
{
    enum reg ax, st;
    const struct symbol *label;

    st = get_x87_reg();
    if (v.kind == IMMEDIATE) {
        v.symbol = sym_create_constant(v.type, v.imm);
        v.kind = DIRECT;
    }

    if (is_real(v.type)) {
        if (v.kind == DIRECT && !is_global_offset(v.symbol)) {
            emit(INSTR_FLD, OPT_MEM, location_of(v, size_of(v.type)));
        } else {
            push(v);
            emit(INSTR_FLD, OPT_MEM,
                location(address(0, SP, 0, 0), size_of(v.type)));
            emit(INSTR_ADD, OPT_IMM_REG,
                constant(EIGHTBYTES(v.type) * 8, 8), reg(SP, 8));
        }
    } else if (is_unsigned(v.type) || size_of(v.type) == 1) {
        ax = get_int_reg();
        load_int(v, ax, 8);
        emit(INSTR_PUSH, OPT_REG, reg(ax, 8));
        emit(INSTR_FILD, OPT_MEM, location(address(0, SP, 0, 0), 8));
        emit(INSTR_ADD, OPT_IMM_REG, constant(8, 8), reg(SP, 8));
        if (size_of(v.type) == 8) {
            label = create_label(definition);
            emit(INSTR_TEST, OPT_REG_REG, reg(ax, 8), reg(ax, 8));
            emit(INSTR_JNS, OPT_IMM, addr(label));
            v = x87_unsigned_adjust_constant();
            load_x87(v);
            emit(INSTR_FADDP, OPT_REG, reg(st, 16));
            x87_stack--;
            enter_context(label);
        }
        assert(int_regs_used == 1);
        relase_regs();
    } else {
        assert(is_signed(v.type));
        assert(size_of(v.type) != 1);
        if (v.kind == DIRECT && !is_global_offset(v.symbol)) {
            emit(INSTR_FILD, OPT_MEM, location_of(v, size_of(v.type)));
        } else {
            push(v);
            emit(INSTR_FILD, OPT_MEM,
                location(address(0, SP, 0, 0), size_of(v.type)));
            emit(INSTR_ADD, OPT_IMM_REG,
                constant(size_of(v.type), 8), reg(SP, 8));
        }
    }

    return st;
}

static enum reg load_float_as_integer(
    struct var val,
    Type type)
{
    enum opcode opcode;
    enum reg ax, cx, xmm0, xmm1;
    struct symbol *convert, *next;
    union value limit;
    int width;

    assert(is_real(val.type));
    assert(is_integer(type));

    ax = get_int_reg();
    width = size_of(type);
    opcode = is_float(val.type) ? INSTR_CVTTSS2SI : INSTR_CVTTSD2SI;
    if (is_signed(type)) {
        width = (width < 4) ? 4 : width;
        assert(width == 4 || width == 8);
        emit_load(opcode, val, reg(ax, width));
    } else if (width < 4) {
        emit_load(opcode, val, reg(ax, 4));
    } else if (width < 8) {
        emit_load(opcode, val, reg(ax, 8));
    } else {
        cx = get_int_reg();
        xmm0 = get_sse_reg();
        xmm1 = get_sse_reg();
        width = size_of(val.type);
        convert = create_label(definition);
        next = create_label(definition);
        load_sse(val, xmm0, width);
        if (is_float(val.type)) {
            limit.f = (float) LONG_MAX;
            load_sse(var_numeric(basic_type__float, limit), xmm1, width);
        } else {
            limit.d = (double) LONG_MAX;
            load_sse(var_numeric(basic_type__double, limit), xmm1, width);
        }
        if (is_float(val.type)) {
            emit(INSTR_UCOMISS, OPT_REG_REG, reg(xmm1, 4), reg(xmm0, 4));
        } else {
            emit(INSTR_UCOMISD, OPT_REG_REG, reg(xmm1, 8), reg(xmm0, 8));
        }
        emit(INSTR_JAE, OPT_IMM, addr(convert));
        /* Value is representable as signed long. */
        emit_load(opcode, val, reg(ax, 8));
        emit(INSTR_JMP, OPT_IMM, addr(next));
        enter_context(convert);
        /* Trickery to convert value not within signed long. */
        if (is_float(val.type)) {
            emit(INSTR_SUBSS, OPT_REG_REG, reg(xmm1, 4), reg(xmm0, 4));
        } else {
            emit(INSTR_SUBSD, OPT_REG_REG, reg(xmm1, 8), reg(xmm0, 8));
        }
        emit(opcode, OPT_REG_REG, reg(xmm0, width), reg(ax, 8));
        emit(INSTR_MOV, OPT_IMM_REG, constant(LONG_MAX + 1ul, 8), reg(cx, 8));
        emit(INSTR_XOR, OPT_REG_REG, reg(cx, 8), reg(ax, 8));
        enter_context(next);
    }

    return ax;
}

static enum reg load_integer_as_float(
    struct var val,
    Type type)
{
    struct symbol *label, *next;
    enum reg xmm, ax, cx;
    enum opcode opcode;

    assert(is_integer(val.type));
    assert(is_real(type));

    xmm = get_sse_reg();
    opcode = (size_of(type) == 8) ? INSTR_CVTSI2SD : INSTR_CVTSI2SS;
    if (is_signed(val.type)) {
        if (size_of(val.type) < 4 || is_field(val)) {
            ax = get_int_reg();
            load_int(val, ax, 4);
            emit(opcode, OPT_REG_REG, reg(ax, 4), reg(xmm, size_of(type)));
        } else {
            emit_load(opcode, val, reg(xmm, size_of(type)));
        }
    } else {
        ax = get_int_reg();
        if (size_of(val.type) < 4) {
            load_int(val, ax, 4);
            emit(opcode, OPT_REG_REG, reg(ax, 4), reg(xmm, size_of(type)));
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
                emit(INSTR_MOV, OPT_REG_REG, reg(ax, 4), reg(ax, 4));
            }
            emit(INSTR_TEST, OPT_REG_REG, reg(ax, 8), reg(ax, 8));
            emit(INSTR_JS, OPT_IMM, addr(label));
            emit(opcode, OPT_REG_REG, reg(ax, 8), reg(xmm, size_of(type)));
            emit(INSTR_JMP, OPT_IMM, addr(next));
            enter_context(label);
            /*
             * Convert large unsigned integer by adding up two halves.
             */
            emit(INSTR_MOV, OPT_REG_REG, reg(ax, 8), reg(cx, 8));
            emit(INSTR_SHR, OPT_IMM_REG, constant(1, 1), reg(cx, 8));
            emit(INSTR_AND, OPT_IMM_REG, constant(1, 4), reg(ax, 4));
            emit(INSTR_OR, OPT_REG_REG, reg(cx, 8), reg(ax, 8));
            emit(opcode, OPT_REG_REG, reg(ax, 8), reg(xmm, size_of(type)));
            if (is_float(type)) {
                emit(INSTR_ADDSS, OPT_REG_REG, reg(xmm, 4), reg(xmm, 4));
            } else {
                emit(INSTR_ADDSD, OPT_REG_REG, reg(xmm, 8), reg(xmm, 8));
            }
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
    emit(INSTR_FSTP, OPT_MEM, location(address(-8, SP, 0, 0), w));
    emit(is_float(type) ? INSTR_MOVSS : INSTR_MOVSD, OPT_MEM_REG,
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
    emit(INSTR_FNSTCW, OPT_MEM, location(address(-16, SP, 0, 0), 2));
    emit(INSTR_MOVZX, OPT_MEM_REG,
        location(address(-16, SP, 0, 0), 2), reg(ax, 4));
    emit(INSTR_OR, OPT_IMM_REG, constant(12 << 8, 4), reg(ax, 4));
    emit(INSTR_MOV, OPT_REG_MEM,
        reg(ax, 2), location(address(-14, SP, 0, 0), 2));

    /* Load control word, and store integer. */
    emit(INSTR_FLDCW, OPT_MEM, location(address(-14, SP, 0, 0), 2));
    emit(INSTR_FISTP, OPT_MEM, location(address(-8, SP, 0, 0), w));
    assert(x87_stack == 1);
    x87_stack = 0;

    /* Restore the old control word and load result to register. */
    emit(INSTR_FLDCW, OPT_MEM, location(address(-16, SP, 0, 0), 2));
    emit(INSTR_MOV, OPT_MEM_REG,
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
        emit(INSTR_SUB, OPT_IMM_REG, constant(16, 8), reg(SP, 8));
        emit(INSTR_FSTP, OPT_MEM, location(address(0, SP, 0, 0), 16));
        emit(INSTR_POP, OPT_REG, reg(r1, 8));
        emit(INSTR_MOV, OPT_REG_MEM,
            reg(r1, 8), location(address(0, r2, 0, 0), 8));
        emit(INSTR_POP, OPT_REG, reg(r1, 8));
        emit(INSTR_MOV, OPT_REG_MEM,
            reg(r1, 8), location(address(8, r2, 0, 0), 8));
    } else {
        assert(v.kind == DIRECT);
        emit(INSTR_FSTP, OPT_MEM, location_of(v, 16));
    }
}

static void bitwise_imm_reg(
    enum opcode opcode,
    struct immediate imm,
    struct registr target)
{
    assert(imm.type == IMM_INT);
    assert(target.r != R11);
    if (imm.w == 8) {
        if (imm.d.qword <= INT_MAX && imm.d.qword >= INT_MIN) {
            imm.w = 4;
            imm.d.dword = (int) imm.d.qword;
            emit(opcode, OPT_IMM_REG, imm, target);
        } else {
            emit(INSTR_MOVAPS, OPT_IMM_REG, imm, reg(R11, 8));
            emit(opcode, OPT_REG_REG, reg(R11, 8), target);
        }
    } else {
        emit(opcode, OPT_IMM_REG, imm, target);
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

    assert(optype == OPT_IMM || optype == OPT_REG);
    if (is_long_double(target.type)) {
        assert(optype == OPT_REG);
        store_x87(target);
        return;
    }

    w = size_of(target.type);
    opc = INSTR_MOV;
    if (is_real(target.type)) {
        opc = is_float(target.type) ? INSTR_MOVSS : INSTR_MOVSD;
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
                emit(INSTR_SHL, OPT_IMM_REG,
                    constant(target.field_offset, w), op.reg);
            }
            bitwise_imm_reg(INSTR_AND, constant(mask, w), op.reg);
            emit(INSTR_OR, OPT_REG_REG, reg(CX, w), op.reg);
        }
    }

    switch (target.kind) {
    case DIRECT:
        assert(!is_array(target.type));
        if (optype == OPT_IMM) {
            if ((ax = allocated_register(target)) != 0) {
                emit(opc, OPT_IMM_REG, op.imm, reg(ax, w));
            } else if (is_global_offset(target.symbol)) {
                emit(INSTR_MOV, OPT_MEM_REG,
                    location(got(target.symbol), 8), reg(R11, 8));
                emit(opc, OPT_IMM_MEM, op.imm,
                    location(address(
                        displacement_from_offset(target.offset), R11, 0, 0), w));
            } else {
                emit(opc, OPT_IMM_MEM, op.imm, location_of(target, w));
            }
        } else {
            if ((ax = allocated_register(target)) != 0) {
                emit(opc, OPT_REG_REG, op.reg, reg(ax, w));
            } else if (is_global_offset(target.symbol)) {
                emit(INSTR_MOV, OPT_MEM_REG,
                    location(got(target.symbol), 8), reg(R11, 8));
                emit(opc, OPT_REG_MEM, op.reg,
                    location(address(
                        displacement_from_offset(target.offset), R11, 0, 0), w));
            } else {
                emit(opc, OPT_REG_MEM, op.reg, location_of(target, w));
            }
        }
        break;
    default:
        assert(target.kind == DEREF);
        if (!target.symbol) {
            target.kind = IMMEDIATE;
            load_int(target, R11, 8);
        } else {
            assert(is_pointer(target.symbol->type));
            load_int(var_direct(target.symbol), R11, 8);
        }
        if (optype == OPT_IMM) {
            emit(opc, OPT_IMM_MEM, op.imm,
                location(address(
                    displacement_from_offset(target.offset), R11, 0, 0), w));
        } else {
            emit(opc, OPT_REG_MEM, op.reg,
                location(address(
                    displacement_from_offset(target.offset), R11, 0, 0), w));
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
        emit(INSTR_SUB, OPT_IMM_REG, constant(8, 8), reg(SP, 8));
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
        emit(INSTR_MOV, OPT_IMM_REG, constant(next_sse_reg, 4), reg(AX, 4));
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

/*
 * Assign a subset of local variables to temporary registers, populating
 * sym->slot.
 *
 * Return number of registers allocated.
 */
static int allocate_registers(struct definition *def)
{
    int i, regs;
    struct symbol *sym;

    regs = 0;
    for (i = 0; i < array_len(&def->locals); ++i) {
        sym = array_get(&def->locals, i);
        if (is_temporary(sym)
            && sym->slot == 0
            && is_arithmetic(sym->type)
            && (is_integer(sym->type) || is_pointer(sym->type))
            && regs < TEMP_INT_REGS)
        {
            assert(sym->linkage == LINK_NONE);
            sym->slot = ++regs;
        }
    }

    return regs;
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
 * Return number of temporary registers pushed to stack, which must be
 * restored on return.
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
static int enter(struct definition *def)
{
    int i, n, regs,
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
    regs = allocate_registers(def);
    reg_offset = regs * 8;

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
    for (i = 0; i < regs; ++i)
        emit(INSTR_PUSH, OPT_REG, reg(temp_int_reg[i], 8));

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
        emit(INSTR_SUB, OPT_IMM_REG, constant(-stack_offset, 8), reg(SP, 8));
        if (res.eightbyte[0] == PC_MEMORY) {
            emit(INSTR_MOV, OPT_REG_MEM,
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
        emit(INSTR_TEST, OPT_REG_REG, reg(AX, 1), reg(AX, 1));
        emit(INSTR_JE, OPT_IMM, addr(sym));
        for (i = 0; i < MAX_SSE_ARGS; ++i) {
            vararg.reg_save_area_offset -= 16;
            emit(INSTR_MOVAPS, OPT_REG_MEM,
                reg(XMM0 + (7 - i), 8),
                location(address(vararg.reg_save_area_offset, BP, 0, 0), 8));
        }

        enter_context(sym);
        for (i = 0; i < MAX_INTEGER_ARGS; ++i) {
            vararg.reg_save_area_offset -= 8;
            emit(INSTR_MOV, OPT_REG_MEM,
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

    return regs;
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
        emit(INSTR_MOV, OPT_IMM_REG, constant(vararg.gp_offset, 4), reg(AX, 4));
        store(AX, gp_offset);
        emit(INSTR_MOV, OPT_IMM_REG, constant(vararg.fp_offset, 4), reg(AX, 4));
        store(AX, fp_offset);
    } else {
        assert(args.kind == DIRECT);
        emit(INSTR_MOV, OPT_IMM_MEM,
            constant(vararg.gp_offset, 4),
            location_of(gp_offset, 4));
        emit(INSTR_MOV, OPT_IMM_MEM,
            constant(vararg.fp_offset, 4),
            location_of(fp_offset, 4));
    }

    emit(INSTR_LEA, OPT_MEM_REG,
        location(address(vararg.overflow_arg_area_offset, BP, 0, 0), 8),
        reg(AX, 8));
    store(AX, overflow_arg_area);

    emit(INSTR_LEA, OPT_MEM_REG,
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
            emit(INSTR_CMP, OPT_IMM_REG,
                constant(MAX_INTEGER_ARGS*8 - 8*num_gp, 4), reg(CX, 4));
            emit(INSTR_JA, OPT_IMM, addr(stack));
        }
        if (num_fp) {
            load(fp_offset, DX);
            emit(INSTR_CMP, OPT_IMM_REG,
                constant(MAX_INTEGER_ARGS*8 + MAX_SSE_ARGS*16 - 16*num_fp, 4),
                reg(DX, 4));
            emit(INSTR_JA, OPT_IMM, addr(stack));
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
                emit(INSTR_MOV, OPT_MEM_REG,
                    location(address(i*8, SI, CX, 1), w), reg(AX, w));
                store(AX, slice);
                break;
            case PC_SSE:
                i = sse_regs_loaded++;
                slice.type = basic_type__double;
                emit(INSTR_MOVSD, OPT_MEM_REG,
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
                emit(INSTR_ADD, OPT_IMM_MEM,
                    constant(8*num_gp, 4), location_of(gp_offset, 4));
            } else {
                load(gp_offset, AX);
                emit(INSTR_ADD, OPT_IMM_REG,
                    constant(8*num_gp, 4), reg(AX, 4));
                store(AX, gp_offset);
            }
        }
        if (num_fp) {
            if (fp_offset.kind == DIRECT) {
                emit(INSTR_ADD, OPT_IMM_MEM,
                    constant(16*num_fp, 4), location_of(fp_offset, 4));
            } else {
                load(fp_offset, AX);
                emit(INSTR_ADD, OPT_IMM_REG,
                    constant(16*num_fp, 4), reg(AX, 4));
                store(AX, fp_offset);
            }
        }

        emit(INSTR_JMP, OPT_IMM, addr(done));
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
            emit(INSTR_MOV, OPT_MEM_REG,
                location(address(0, SI, 0, 0), w), reg(ax, w));
        } else {
            emit(INSTR_MOV, OPT_MEM_REG,
                location(address(0, SI, 0, 0), w), reg(AX, w));
            emit(INSTR_MOV, OPT_REG_MEM, reg(AX, w), location_of(res, w));
        }
    } else {
        load_address(res, DI);
        emit(INSTR_MOV, OPT_IMM_REG, constant(w, 4), reg(DX, 4));
        emit(INSTR_CALL, OPT_IMM, addr(decl_memcpy));
    }

    /*
     * Move overflow_arg_area pointer to position of next memory
     * argument, aligning to 8 byte.
     */
    w = EIGHTBYTES(res.type);
    if (overflow_arg_area.kind == DIRECT) {
        emit(INSTR_ADD, OPT_IMM_MEM,
            constant(w*8, 8), location_of(overflow_arg_area, 8));
    } else {
        load(overflow_arg_area, AX);
        emit(INSTR_ADD, OPT_IMM_REG, constant(w*8, 8), reg(AX, 8));
        store(AX, overflow_arg_area);
    }

    if (pc.eightbyte[0] == PC_INTEGER || pc.eightbyte[0] == PC_SSE) {
        assert(done);
        enter_context(done);
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
    mem_used = push_function_arguments(func, pc);
    if (pc.eightbyte[0] == PC_MEMORY) {
        assert(!is_void(target.type));
        load_address(target, param_int_reg[0]);
    }

    if (ptr.kind == ADDRESS) {
        assert(!ptr.offset);
        emit(INSTR_CALL, OPT_IMM, addr(ptr.symbol));
    } else {
        assert(ptr.kind != IMMEDIATE);
        load(ptr, R11);
        emit(INSTR_CALL, OPT_REG, reg(R11, 8));
    }

    if (mem_used) {
        emit(INSTR_ADD, OPT_IMM_REG, constant(mem_used, 8), reg(SP, 8));
    }

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

static enum opcode compile_compare(
    enum optype op,
    struct var l,
    struct var r)
{
    size_t w;
    enum opcode cmp;
    enum reg ax, cx;
    enum reg xmm0, xmm1;

    switch (op) {
    default: assert(0);
    case IR_OP_EQ:
        cmp = INSTR_SETE;
        break;
    case IR_OP_NE:
        cmp = INSTR_SETNE;
        break;
    case IR_OP_GE:
        cmp = is_unsigned(l.type) || is_real(l.type)
            ? INSTR_SETAE
            : INSTR_SETGE;
        break;
    case IR_OP_GT:
        cmp = is_unsigned(l.type) || is_real(l.type)
            ? INSTR_SETA
            : INSTR_SETG;
        break;
    }

    if (is_real(l.type)) {
        xmm0 = load_cast(l, l.type);
        xmm1 = load_cast(r, r.type);
        if (is_float(l.type)) {
            emit(INSTR_UCOMISS, OPT_REG_REG, reg(xmm1, 4), reg(xmm0, 4));
        } else if (is_double(l.type)) {
            emit(INSTR_UCOMISD, OPT_REG_REG, reg(xmm1, 8), reg(xmm0, 8));
        } else {
            assert(is_long_double(l.type));
            if (op == IR_OP_GE || op == IR_OP_GT) {
                emit(INSTR_FXCH, OPT_REG, reg(xmm0, 16));
            }
            emit(INSTR_FUCOMIP, OPT_REG, reg(xmm0, 16));
            emit(INSTR_FSTP, OPT_REG, reg(xmm1, 16));
            assert(x87_stack == 2);
            x87_stack = 0;
        }
    } else {
        w = size_of(l.type);
        assert(w == size_of(r.type));
        if (is_constant(l) && w < 8) {
            if (r.kind == DIRECT
                && !is_global_offset(r.symbol)
                && !is_field(r))
            {
                if ((ax = allocated_register(r)) != 0) {
                    emit(INSTR_CMP, OPT_IMM_REG, value_of(l, w), reg(ax, w));
                } else {
                    emit(INSTR_CMP, OPT_IMM_MEM, value_of(l, w),
                        location_of(r, w));
                }
            } else {
                ax = load_cast(r, r.type);
                emit(INSTR_CMP, OPT_IMM_REG, value_of(l, w), reg(ax, w));
            }
            switch (cmp) {
            case INSTR_SETAE:
                cmp = INSTR_SETNA;
                break;
            case INSTR_SETGE:
                cmp = INSTR_SETNG;
                break;
            case INSTR_SETA:
                cmp = INSTR_SETNAE;
                break;
            case INSTR_SETG:
                cmp = INSTR_SETNGE;
                break;
            default: break;
            }
        } else if (is_constant(r) && w < 8) {
            if (l.kind == DIRECT
                && !is_global_offset(l.symbol)
                && !is_field(l))
            {
                if ((ax = allocated_register(l)) != 0) {
                    emit(INSTR_CMP, OPT_IMM_REG, value_of(r, w), reg(ax, w));
                } else {
                    emit(INSTR_CMP, OPT_IMM_MEM, value_of(r, w),
                        location_of(l, w));
                }
            } else {
                ax = load_cast(l, l.type);
                emit(INSTR_CMP, OPT_IMM_REG, value_of(r, w), reg(ax, w));
            }
        } else {
            if (l.kind == DIRECT
                && !is_global_offset(l.symbol)
                && !is_field(l))
            {
                if ((ax = allocated_register(l)) != 0) {
                    ax = load_cast(l, l.type);
                    cx = load_cast(r, r.type);
                    emit(INSTR_CMP, OPT_REG_REG, reg(cx, w), reg(ax, w));
                } else {
                    ax = load_cast(r, r.type);
                    emit(INSTR_CMP, OPT_REG_MEM, reg(ax, w), location_of(l, w));
                }
            } else if (r.kind == DIRECT
                && !is_global_offset(r.symbol)
                && !is_field(r))
            {
                if ((ax = allocated_register(r)) != 0) {
                    ax = load_cast(l, l.type);
                    cx = load_cast(r, r.type);
                    emit(INSTR_CMP, OPT_REG_REG, reg(cx, w), reg(ax, w));
                } else {
                    ax = load_cast(l, l.type);
                    emit(INSTR_CMP, OPT_MEM_REG, location_of(r, w), reg(ax, w));
                }
            } else {
                ax = load_cast(l, l.type);
                cx = load_cast(r, r.type);
                emit(INSTR_CMP, OPT_REG_REG, reg(cx, w), reg(ax, w));
            }
        }
    }

    return cmp;
}

/*
 * Compare variables for equality as expression operands.
 *
 * Do not consider type, as this can be changed during evaluation, for
 * example addition of pointer types being treated as long.
 */
static int operand_equal(struct var a, struct var b)
{
    return a.symbol == b.symbol
        && a.symbol != NULL
        && a.kind == b.kind
        && a.field_width == b.field_width
        && a.field_offset == b.field_offset
        && a.offset == b.offset;
}

static int is_int_constant(struct var v)
{
    return v.kind == IMMEDIATE
        && (is_integer(v.type) || is_pointer(v.type))
        && !v.symbol
        && (size_of(v.type) < 8
            || (v.imm.i <= INT_MAX && v.imm.i >= INT_MIN));
}

static enum reg compile_add(
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
        emit(INSTR_FADDP, OPT_REG, reg(ax, w));
        assert(x87_stack == 2);
        x87_stack--;
    } else if (is_real(type)) {
        ax = load_cast(l, type);
        cx = load_cast(r, type);
        opc = is_float(type) ? INSTR_ADDSS : INSTR_ADDSD;
        emit(opc, OPT_REG_REG, reg(cx, w), reg(ax, w));
    } else {
        if (!is_void(target.type)
            && target.kind == DIRECT
            && !is_global_offset(target.symbol)
            && !is_field(target))
        {
            ax = AX;
            if (operand_equal(target, r)) {
                if (is_int_constant(l)) {
                    if ((cx = allocated_register(r)) != 0) {
                        emit(INSTR_ADD, OPT_REG_REG, reg(ax, w), reg(cx, w));
                        ax = cx;
                    } else {
                        emit(INSTR_ADD, OPT_IMM_MEM,
                            value_of(l, w), location_of(target, w));
                    }
                } else {
                    ax = load_cast(l, type);
                    if ((cx = allocated_register(r)) != 0) {
                        emit(INSTR_ADD, OPT_REG_REG, reg(ax, w), reg(cx, w));
                        ax = cx;
                    } else {
                        emit(INSTR_ADD, OPT_REG_MEM,
                            reg(ax, w), location_of(target, w));
                    }
                }
                return ax;
            } else if (operand_equal(target, l)) {
                if (is_int_constant(r)) {
                    if ((cx = allocated_register(l)) != 0) {
                        emit(INSTR_ADD, OPT_REG_REG, reg(ax, w), reg(cx, w));
                        ax = cx;
                    } else {
                        emit(INSTR_ADD, OPT_IMM_MEM,
                            value_of(r, w), location_of(target, w));
                    }
                } else {
                    ax = load_cast(r, type);
                    if ((cx = allocated_register(l)) != 0) {
                        emit(INSTR_ADD, OPT_REG_REG, reg(ax, w), reg(cx, w));
                        ax = cx;
                    } else {
                        emit(INSTR_ADD, OPT_REG_MEM,
                            reg(ax, w), location_of(target, w));
                    }
                }
                return ax;
            }
        }
        if (is_int_constant(l)) {
            ax = load_cast(r, type);
            emit(INSTR_ADD, OPT_IMM_REG, value_of(l, w), reg(ax, w));
        } else if (is_int_constant(r)) {
            ax = load_cast(l, type);
            emit(INSTR_ADD, OPT_IMM_REG, value_of(r, w), reg(ax, w));
        } else if (l.kind == DIRECT
            && !is_global_offset(l.symbol)
            && !is_field(l))
        {
            ax = load_cast(r, type);
            if ((cx = allocated_register(l)) != 0) {
                emit(INSTR_ADD, OPT_REG_REG, reg(cx, w), reg(ax, w));
            } else {
                emit(INSTR_ADD, OPT_MEM_REG, location_of(l, w), reg(ax, w));
            }
        } else if (r.kind == DIRECT
            && !is_global_offset(r.symbol)
            && !is_field(r))
        {
            ax = load_cast(l, type);
            if ((cx = allocated_register(r)) != 0) {
                emit(INSTR_ADD, OPT_REG_REG, reg(cx, w), reg(ax, w));
            } else {
                emit(INSTR_ADD, OPT_MEM_REG, location_of(r, w), reg(ax, w));
            }
        } else {
            ax = load_cast(l, type);
            cx = load_cast(r, type);
            emit(INSTR_ADD, OPT_REG_REG, reg(cx, w), reg(ax, w));
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
        emit(INSTR_FSUBRP, OPT_REG, reg(ax, w));
        assert(x87_stack == 2);
        x87_stack--;
        if (!is_void(target.type)) {
            store_x87(target);
        }
    } else {
        ax = load_cast(l, type);
        cx = load_cast(r, type);
        opc = is_float(type) ? INSTR_SUBSS
            : is_double(type) ? INSTR_SUBSD
            : INSTR_SUB;
        emit(opc, OPT_REG_REG, reg(cx, w), reg(ax, w));
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
    emit(INSTR_NOT, OPT_REG, reg(AX, size_of(l.type)));
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
    size_t w;

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

    w = size_of(l.type);
    assert(w == size_of(val.type));
    xmm0 = load_cast(val, val.type);
    xmm1 = load_cast(l, l.type);
    emit(INSTR_PXOR, OPT_REG_REG, reg(xmm1, w), reg(xmm0, w));
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
    enum opcode opc;
    enum reg ax, cx;

    w = size_of(type);
    if (is_real(type)) {
        ax = load_cast(l, type);
        cx = load_cast(r, type);
        if (is_long_double(type)) {
            emit(INSTR_FMULP, OPT_REG, reg(ax, w));
            assert(x87_stack == 2);
            x87_stack--;
            if (!is_void(target.type)) {
                store_x87(target);
            }
        } else {
            opc = is_float(type) ? INSTR_MULSS : INSTR_MULSD;
            emit(opc, OPT_REG_REG, reg(cx, w), reg(ax, w));
            if (!is_void(target.type)) {
                store(ax, target);
            }
        }
    } else {
        ax = load_cast(r, r.type);
        assert(ax == AX);
        if (l.kind == DIRECT
            && !is_register_allocated(l)
            && !is_global_offset(l.symbol))
        {
            emit(INSTR_MUL, OPT_MEM, location_of(l, w));
        } else {
            cx = load_cast(l, l.type);
            emit(INSTR_MUL, OPT_REG, reg(cx, w));
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
    size_t w;
    enum opcode opc;
    enum reg ax, cx;

    if (is_real(type)) {
        w = size_of(l.type);
        ax = load_cast(l, type);
        cx = load_cast(r, type);
        if (is_long_double(type)) {
            emit(INSTR_FDIVRP, OPT_REG, reg(ax, w));
            assert(x87_stack == 2);
            x87_stack--;
            if (!is_void(target.type)) {
                store_x87(target);
            }
        } else {
            opc = is_float(type) ? INSTR_DIVSS : INSTR_DIVSD;
            emit(opc, OPT_REG_REG, reg(cx, w), reg(ax, w));
            if (!is_void(target.type)) {
                store(ax, target);
            }
        }
    } else {
        ax = load_cast(l, l.type);
        assert(ax == AX);
        if (is_signed(l.type)) {
            if (size_of(l.type) == 8) {
                emit(INSTR_CQO, OPT_NONE);
            } else {
                assert(size_of(l.type) == 4);
                emit(INSTR_CDQ, OPT_NONE);
            }
        } else {
            emit(INSTR_XOR, OPT_REG_REG, reg(DX, 8), reg(DX, 8));
        }
        opc = is_signed(type) ? INSTR_IDIV : INSTR_DIV;
        if (r.kind == DIRECT
            && !is_global_offset(r.symbol)
            && !is_register_allocated(r)
            && !is_field(r))
        {
            emit(opc, OPT_MEM, location_of(r, size_of(r.type)));
        } else {
            cx = load_cast(r, r.type);
            emit(opc, OPT_REG, reg(cx, size_of(r.type)));
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
    assert(!is_real(type));

    ax = load_cast(l, l.type);
    assert(ax == AX);
    if (is_signed(l.type)) {
        if (size_of(l.type) == 8) {
            emit(INSTR_CQO, OPT_NONE);
        } else {
            assert(size_of(l.type) == 4);
            emit(INSTR_CDQ, OPT_NONE);
        }
    } else {
        emit(INSTR_XOR, OPT_REG_REG, reg(DX, 8), reg(DX, 8));
    }

    opc = is_signed(type) ? INSTR_IDIV : INSTR_DIV;
    if (r.kind == DIRECT
        && !is_register_allocated(r)
        && !is_global_offset(r.symbol)
        && !is_field(r))
    {
        emit(opc, OPT_MEM, location_of(r, size_of(r.type)));
    } else {
        ax = load_cast(r, r.type);
        emit(opc, OPT_REG, reg(ax, size_of(r.type)));
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
    if (is_constant(r) && w < 8) {
        ax = load(l, AX);
        emit(opcode, OPT_IMM_REG, value_of(r, w), reg(ax, w));
    } else if (is_constant(l) && w < 8) {
        ax = load(r, AX);
        emit(opcode, OPT_IMM_REG, value_of(l, w), reg(ax, w));
    } else {
        ax = load(l, AX);
        load(r, CX);
        emit(opcode, OPT_REG_REG, reg(CX, w), reg(ax, w));
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
    emit(INSTR_SHL, OPT_REG_REG, reg(CX, 1), reg(AX, size_of(l.type)));
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
        emit(INSTR_SHR, OPT_REG_REG, reg(CX, 1), reg(AX, size_of(l.type)));
    } else {
        emit(INSTR_SAR, OPT_REG_REG, reg(CX, 1), reg(AX, size_of(l.type)));
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
static enum reg set_compare_value(Type type, enum opcode op)
{
    emit(op, OPT_REG, reg(AX, 1));
    if (is_real(type)) {
        if (op == INSTR_SETE) {
            emit(INSTR_SETNP, OPT_REG, reg(CX, 1));
            emit(INSTR_AND, OPT_REG_REG, reg(CX, 1), reg(AX, 1));
        } else if (op == INSTR_SETNE) {
            emit(INSTR_SETP, OPT_REG, reg(CX, 1));
            emit(INSTR_OR, OPT_REG_REG, reg(CX, 1), reg(AX, 1));
        }
        emit(INSTR_AND, OPT_IMM_REG, constant(1, 1), reg(AX, 1));
    }

    emit(INSTR_MOVZX, OPT_REG_REG, reg(AX, 1), reg(AX, 4));
    return AX;
}

/*
 * Write object from address returned by evaluating an expression. Used
 * for assignment of objects that do not fit in a single register.
 *
 * Handle special case of char [] = string literal. This will only occur
 * as part of initializer, at block scope. External definitions are
 * handled before this. At no other point should array types be seen in
 * assembly backend. We handle these assignments with memcpy, other
 * compilers load the string into register as ascii numbers.
 */
static void store_copy_object(struct var var, struct var target)
{
    if (is_array(var.type)) {
        assert(target.kind == DIRECT);
        assert(is_string(var));
        assert(type_equal(target.type, var.type));
        emit(INSTR_LEA, OPT_MEM_REG, location_of(var, 8), reg(SI, 8));
    } else {
        load_address(var, SI);
    }

    load_address(target, DI);
    emit(INSTR_MOV, OPT_IMM_REG, constant(size_of(target.type), 4), reg(DX, 4));
    emit(INSTR_CALL, OPT_IMM, addr(decl_memcpy));
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
        if (EIGHTBYTES(type) == 1) {
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
    enum opcode op;

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
        op = compile_compare(expr.op, expr.l, expr.r);
        ax = set_compare_value(expr.l.type, op);
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
    emit(INSTR_SUB, OPT_REG_REG, reg(ax, 8), reg(SP, 8));
    emit(INSTR_MOV, OPT_IMM_REG, constant(-16, 8), reg(R11, 8));
    emit(INSTR_AND, OPT_REG_REG, reg(R11, 8), reg(SP, 8));

    /* Assign current stack location to VLA symbol. */
    store(SP, var_direct(sym->value.vla_address));
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
        assert(stmt.t.symbol);
        compile_vla_alloc(stmt.t.symbol, stmt.expr);
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
                emit(INSTR_MOV, OPT_REG_REG,
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
                emit(INSTR_MOV, OPT_REG_REG,
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
        emit(INSTR_MOV, OPT_MEM_REG,
            location(address(return_address_offset, BP, 0, 0), 8), reg(DI, 8));
        emit(INSTR_CMP, OPT_REG_REG, reg(DI, 8), reg(SI, 8));
        emit(INSTR_JE, OPT_IMM, addr(label));
        emit(INSTR_MOV, OPT_IMM_REG, constant(w, 8), reg(DX, 8));
        emit(INSTR_CALL, OPT_IMM, addr(decl_memcpy));
        emit(INSTR_MOV, OPT_MEM_REG,
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
static void compile_block(struct block *block, Type type, int regs)
{
    int i;
    enum reg ax;
    enum reg xmm0, xmm1;
    enum opcode cmp;
    struct statement st;

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
            assert(type_equal(block->expr.type, type_next(type)));
            compile_return(type, block->expr);
            relase_regs();
            assert(x87_stack == 0);
        }
        if (regs) {
            emit(INSTR_LEA, OPT_MEM_REG,
                location(address(-regs * 8, BP, 0, 0), 8),
                reg(SP, 8));
            for (i = regs; i > 0; --i) {
                emit(INSTR_POP, OPT_REG, reg(temp_int_reg[i - 1], 8));
            }
        }
        emit(INSTR_LEAVE, OPT_NONE);
        emit(INSTR_RET, OPT_NONE);
    } else if (!block->jump[1]) {
        if (block->jump[0]->color == BLACK) {
            emit(INSTR_JMP, OPT_IMM, addr(block->jump[0]->label));
        } else {
            compile_block(block->jump[0], type, regs);
        }
    } else {
        assert(block->jump[0]);
        assert(block->jump[1]);
        assert(is_scalar(block->expr.type));
        if (is_comparison(block->expr)) {
            cmp = compile_compare(block->expr.op, block->expr.l, block->expr.r);
            switch (cmp) {
            default: assert(0);
            case INSTR_SETE:
                if (is_real(block->expr.l.type)) {
                    emit(INSTR_JNE, OPT_IMM, addr(block->jump[0]->label));
                    emit(INSTR_JP, OPT_IMM, addr(block->jump[0]->label));
                    emit(INSTR_JMP, OPT_IMM, addr(block->jump[1]->label));
                } else {
                    emit(INSTR_JNE, OPT_IMM, addr(block->jump[0]->label));
                }
                break;
            case INSTR_SETNE:
                if (is_real(block->expr.l.type)) {
                    emit(INSTR_JNE, OPT_IMM, addr(block->jump[1]->label));
                    emit(INSTR_JP, OPT_IMM, addr(block->jump[1]->label));
                    emit(INSTR_JMP, OPT_IMM, addr(block->jump[0]->label));
                } else {
                    emit(INSTR_JE, OPT_IMM, addr(block->jump[0]->label));
                }
                break;
            case INSTR_SETG:
                emit(INSTR_JNG, OPT_IMM, addr(block->jump[0]->label));
                break;
            case INSTR_SETNG:
                emit(INSTR_JG, OPT_IMM, addr(block->jump[0]->label));
                break;
            case INSTR_SETA:
                emit(INSTR_JNA, OPT_IMM, addr(block->jump[0]->label));
                break;
            case INSTR_SETNA:
                emit(INSTR_JA, OPT_IMM, addr(block->jump[0]->label));
                break;
            case INSTR_SETGE:
                emit(INSTR_JNGE, OPT_IMM, addr(block->jump[0]->label));
                break;
            case INSTR_SETNGE:
                emit(INSTR_JGE, OPT_IMM, addr(block->jump[0]->label));
                break;
            case INSTR_SETAE:
                emit(INSTR_JNAE, OPT_IMM, addr(block->jump[0]->label));
                break;
            case INSTR_SETNAE:
                emit(INSTR_JAE, OPT_IMM, addr(block->jump[0]->label));
                break;
            }
        } else {
            ax = compile_expression(block->expr);
            if (is_real(block->expr.type)) {
                xmm0 = ax;
                xmm1 = (xmm0 == XMM0) ? XMM1 : XMM0;
                emit(INSTR_PXOR, OPT_REG_REG, reg(xmm1, 8), reg(xmm1, 8));
                if (is_float(block->expr.type)) {
                    emit(INSTR_UCOMISS, OPT_REG_REG, reg(xmm0, 4), reg(xmm1, 4));
                } else {
                    emit(INSTR_UCOMISD, OPT_REG_REG, reg(xmm0, 8), reg(xmm1, 8));
                }
                emit(INSTR_JNE, OPT_IMM, addr(block->jump[1]->label));
                emit(INSTR_JP, OPT_IMM, addr(block->jump[1]->label));
                emit(INSTR_JMP, OPT_IMM, addr(block->jump[0]->label));
            } else {
                i = size_of(block->expr.type);
                assert(i == 1 || i == 2 || i == 4 || i == 8);
                emit(INSTR_CMP, OPT_IMM_REG, constant(0, i), reg(ax, i));
                emit(INSTR_JE, OPT_IMM, addr(block->jump[0]->label));
            }
        }

        relase_regs();
        if (block->jump[1]->color == BLACK) {
            emit(INSTR_JMP, OPT_IMM, addr(block->jump[1]->label));
        } else {
            compile_block(block->jump[1], type, regs);
        }

        compile_block(block->jump[0], type, regs);
    }
}

static void compile_data_assign(struct var target, struct var val)
{
    struct immediate imm = {0};
    assert(target.kind == DIRECT);

    imm.w = size_of(target.type);
    if (val.kind == IMMEDIATE) {
        switch (type_of(target.type)) {
        case T_POINTER:
            if (is_string(val)) {
                imm.type = IMM_ADDR;
                imm.d.addr.base = IP;
                imm.d.addr.sym = val.symbol;
                imm.d.addr.disp = displacement_from_offset(val.offset);
                break;
            }
        case T_BOOL:
        case T_CHAR:
        case T_SHORT:
        case T_INT:
        case T_LONG:
        case T_FLOAT:
        case T_DOUBLE:
        case T_LDOUBLE:
            assert(type_equal(target.type, val.type));
            imm.type = IMM_INT;
            if (is_long_double(val.type)) {
                union {
                    long double val;
                    long arr[2];
                } cast = {0};
                imm.w = 8;
                cast.val = val.imm.ld;
                imm.d.qword = cast.arr[0];
                emit_data(imm);
                imm.d.qword = cast.arr[1] & 0xFFFF;
            } else {
                imm.d.qword = val.imm.i;
            }
            break;
        case T_ARRAY:
            if (is_string(val)) {
                imm.type = IMM_STRING;
                imm.d.string = val.symbol->value.string;
                break;
            }
        default: assert(0);
        }
    } else {
        assert(val.kind == ADDRESS);
        assert(val.symbol->linkage != LINK_NONE);
        imm.type = IMM_ADDR;
        imm.d.addr.base = IP;
        imm.d.addr.sym = val.symbol;
        imm.d.addr.disp = displacement_from_offset(val.offset);
    }

    emit_data(imm);
}

static void zero_fill_data(size_t bytes)
{
    static struct immediate
        zero_byte = {IMM_INT, 1},
        zero_int = {IMM_INT, 4},
        zero_quad = {IMM_INT, 8};

    while (bytes >= size_of(basic_type__long)) {
        emit_data(zero_quad);
        bytes -= size_of(basic_type__long);
    }

    while (bytes >= size_of(basic_type__int)) {
        emit_data(zero_int);
        bytes -= size_of(basic_type__int);
    }

    while (bytes > 0) {
        bytes -= 1;
        emit_data(zero_byte);
    }
}

static void compile_data(struct definition *def)
{
    int i;
    long mask;
    union value value;
    struct statement st, fl;
    size_t backing_field_size;
    size_t
        total_size = size_of(def->symbol->type),
        initialized = 0;

    enter_context(def->symbol);
    for (i = 0; i < array_len(&def->body->code); ++i) {
        st = array_get(&def->body->code, i);
        if (st.t.offset > initialized) {
            zero_fill_data(st.t.offset - initialized);
        }

        initialized = st.t.offset;
        if (is_field(st.t)) {
            backing_field_size = size_of(st.t.type);
            value.i = 0;
            fl = st;
            do {
                assert(fl.st == IR_ASSIGN);
                assert(fl.t.kind == DIRECT);
                assert(fl.t.symbol == def->symbol);
                assert(is_identity(fl.expr));
                assert(is_integer(fl.expr.type));
                assert(fl.expr.l.kind == IMMEDIATE);
                mask = ((1l << fl.t.field_width) - 1);
                value.i |= (fl.expr.l.imm.i & mask) << fl.t.field_offset;
                if (size_of(fl.t.type) > backing_field_size) {
                    backing_field_size = size_of(fl.t.type);
                }
                i += 1;
                if (i == array_len(&def->body->code))
                    break;
                fl = array_get(&def->body->code, i);
            } while (is_field(fl.t) && fl.t.offset == initialized);
            i -= 1;
            switch (backing_field_size) {
            default: assert(0);
            case 1:
                st.t.type = basic_type__char;
                break;
            case 2:
                st.t.type = basic_type__short;
                break;
            case 4:
                st.t.type = basic_type__int;
                break;
            case 8:
                st.t.type = basic_type__long;
                break;
            }
            st.t.field_offset = 0;
            st.t.field_width = 0;
            compile_data_assign(st.t, var_numeric(st.t.type, value));
        } else {
            assert(st.st == IR_ASSIGN);
            assert(st.t.kind == DIRECT);
            assert(st.t.symbol == def->symbol);
            assert(is_identity(st.expr));
            compile_data_assign(st.t, st.expr.l);
        }

        initialized = st.t.offset + size_of(st.t.type);
    }

    assert(total_size >= initialized);
    zero_fill_data(total_size - initialized);
}

static void compile_function(struct definition *def)
{
    int regs;

    assert(is_function(def->symbol->type));
    enter_context(def->symbol);
    emit(INSTR_PUSH, OPT_REG, reg(BP, 8));
    emit(INSTR_MOV, OPT_REG_REG, reg(SP, 8), reg(BP, 8));

    /* Make sure parameters and local variables are placed on stack. */
    regs = enter(def);

    /* Recursively assemble body. */
    compile_block(def->body, def->symbol->type, regs);
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
}

INTERNAL void finalize(void)
{
    array_clear(&func_args);
    if (finalize_backend) {
        finalize_backend();
    }
}
