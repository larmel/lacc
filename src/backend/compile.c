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

static FILE *output_stream;

static int (*enter_context)(const struct symbol *);
static int (*emit_instruction)(struct instruction);
static int (*emit_data)(struct immediate);
static int (*flush_backend)(void);

/* Values from va_list initialization. */
static struct {
    int gp_offset;
    int fp_offset;
    int overflow_arg_area_offset;
    int reg_save_area_offset;
} vararg;

/* Registers used for parameter passing and return value. */
static enum reg param_int_reg[] = {DI, SI, DX, CX, R8, R9};
static enum reg param_sse_reg[] = {XMM0, XMM1, XMM2, XMM3,
                                   XMM4, XMM5, XMM6, XMM7};
static enum reg ret_int_reg[] = {AX, DX};
static enum reg ret_sse_reg[] = {XMM0, XMM1};

/* Store incoming PARAM operations before CALL. */
static array_of(struct var) func_args;

/*
 * Keep track of used registers when evaluating expressions, not having
 * to explicitly tell which register is to be used in all rules.
 */
static unsigned int_regs_used, sse_regs_used;

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

static void compile_block(struct block *block, const struct typetree *type);

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

static int is_string(struct var val)
{
    return
        val.kind == IMMEDIATE && val.symbol &&
        val.symbol->symtype == SYM_STRING_VALUE;
}

static struct immediate value_of(struct var var, int w)
{
    struct immediate imm = {0};
    assert(var.kind == IMMEDIATE);

    imm.w = w;
    if (is_string(var)) {
        imm.type = IMM_ADDR;
        imm.d.addr.sym = var.symbol;
        imm.d.addr.disp = var.offset;
    } else {
        assert(!var.offset);
        assert(is_scalar(var.type));
        assert(w == 1 || w == 2 || w == 4 || w == 8);
        imm.type = IMM_INT;
        imm.d.qword = var.imm.i;
    }

    return imm;
}

static struct memory location(struct address addr, int w)
{
    struct memory loc = {{0}};
    loc.addr = addr;
    loc.w = w;
    return loc;
}

static struct address address_of(struct var var)
{
    struct address addr = {0};
    assert(var.kind != DEREF);

    if (var.kind == IMMEDIATE) {
        assert(is_string(var));
        addr.sym = var.symbol;
        addr.disp = var.offset;
    } else if (var.symbol->linkage != LINK_NONE) {
        addr.base = IP;
        addr.disp = var.offset;
        addr.sym = var.symbol;
    } else {
        addr.disp = var.symbol->stack_offset + var.offset;
        addr.base = BP;
    }

    return addr;
}

static struct memory location_of(struct var var, int w)
{
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

static struct immediate addr(const struct symbol *sym)
{
    struct immediate imm = {IMM_ADDR};
    imm.d.addr.sym = sym;
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
        case PC_NO_CLASS:
            break;
        default: assert(0);
        }
    }
}

/*
 * Emit instruction to load a value to specified register. Handles all
 * kinds of variables, including immediate. Load instruction can perform
 * conversion, for example sign extend. I.e. variable and register does
 * not need to be of the same width.
 */
static void emit_load(
    enum opcode opcode,
    struct var source,
    struct registr dest)
{
    struct var ptr;
    const int w = size_of(source.type);
    assert(is_standard_register_width(w));

    switch (source.kind) {
    case IMMEDIATE:
        /*
         * There is no suitable instruction for moving an immediate
         * floating point value directly to register. Need to store it
         * in memory first, so create symbol holding constant value.
         */
        if (is_real(source.type)) {
            source.symbol = sym_create_constant(source.type, source.imm);
            source.kind = DIRECT;
        } else {
            assert(opcode == INSTR_MOV);
            emit(opcode, OPT_IMM_REG, value_of(source, dest.w), dest);
            break;
        }
    case DIRECT:
        emit(opcode, OPT_MEM_REG, location_of(source, w), dest);
        break;
    case DEREF:
        ptr = var_direct(source.symbol);
        if (is_string(ptr)) {
            ptr.offset = source.offset;
            emit(opcode, OPT_MEM_REG, location_of(ptr, w), dest);
        } else {
            assert(is_pointer(ptr.type));
            emit(INSTR_MOV, OPT_MEM_REG, location_of(ptr, 8), reg(R11, 8));
            emit(opcode,
                OPT_MEM_REG,
                location(address(source.offset, R11, 0, 0), w),
                dest);
        }
        break;
    case ADDRESS:
        assert(opcode == INSTR_LEA);
        emit(opcode, OPT_MEM_REG, location_of(source, 8), dest);
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

/*
 * Load integer variable v to register r, sign extended to fit register
 * size. Values are always sign extended to either 4 or 8 bytes on load.
 */
static void load_int(struct var v, enum reg r, int w)
{
    enum opcode opcode;

    assert(w == 4 || w == 8);
    assert(!is_real(v.type));

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

/*
 * Load variable to register, automatically sign/width extended to
 * either 32 or 64 bit.
 */
static enum reg load(struct var v, enum reg r)
{
    const int w = size_of(v.type);
    if (is_real(v.type)) {
        load_sse(v, r, w);
    } else {
        load_int(v, r, (w < 4) ? 4 : w);
    }

    return r;
}

static enum reg load_float_as_integer(
    struct var val,
    const struct typetree *type)
{
    enum opcode opcode;
    enum reg ax, cx, xmm0, xmm1;
    struct symbol *convert, *next;
    struct number limit;
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
        convert = sym_create_label();
        next = sym_create_label();
        if (is_float(val.type)) {
            limit.type = &basic_type__float;
            limit.val.f = (float) LONG_MAX;
        } else {
            limit.type = &basic_type__double;
            limit.val.d = (double) LONG_MAX;
        }
        load_sse(val, xmm0, width);
        load_sse(var_numeric(limit), xmm1, width);
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
    const struct typetree *type)
{
    struct symbol *label, *next;
    enum reg xmm, ax, cx;
    enum opcode opcode;

    assert(is_integer(val.type));
    assert(is_real(type));

    xmm = get_sse_reg();
    opcode = (size_of(type) == 8) ? INSTR_CVTSI2SD : INSTR_CVTSI2SS;
    if (is_signed(val.type)) {
        if (size_of(val.type) < 4) {
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
            label = sym_create_label();
            next = sym_create_label();
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

/*
 * Read value while doing type conversion (if necessary), returning a
 * register holding the value. Type is promoted to be at least 32 bit
 * wide.
 */
static enum reg load_cast(struct var val, const struct typetree *type)
{
    unsigned int mask;
    enum reg r;

    if (size_of(type) < 4) {
        type = is_integer(type)
            ? &basic_type__int
            : &basic_type__unsigned_int;
    }

    assert(size_of(type) == 4 || size_of(type) == 8);
    if (is_real(val.type)) {
        if (is_real(type)) {
            r = get_sse_reg();
            load_sse(val, r, size_of(type));
        } else {
            r = load_float_as_integer(val, type);
        }
    } else {
        if (is_real(type)) {
            assert(!is_field(val));
            r = load_integer_as_float(val, type);
        } else {
            r = get_int_reg();
            load_int(val, r, size_of(type));
            if (is_field(val)) {
                assert(size_of(val.type) == size_of(&basic_type__int));
                assert(val.width > 0 && val.width < 32);
                mask = (0xFFFFFFFFu >> (32 - val.width));
                emit(INSTR_AND, OPT_IMM_REG, constant(mask, 4), reg(r, 4));
            }
        }
    }

    return r;
}

static void load_address(struct var v, enum reg r)
{
    if (v.kind == DIRECT) {
        emit(INSTR_LEA, OPT_MEM_REG, location_of(v, 8), reg(r, 8));
    } else {
        assert(v.kind == DEREF);
        load(var_direct(v.symbol), r);
        if (v.offset) {
            emit(INSTR_ADD, OPT_IMM_REG, constant(v.offset, 8), reg(r, 8));
        }
    }
}

static void store(enum reg r, struct var v)
{
    const int w = size_of(v.type);
    unsigned int mask;
    enum opcode op = INSTR_MOV;

    if (is_real(v.type)) {
        op = (is_float(v.type)) ? INSTR_MOVSS : INSTR_MOVSD;
    } else if (is_field(v)) {
        assert(w == size_of(&basic_type__int));
        assert(v.width > 0 && v.width < 32);
        mask = (0xFFFFFFFFu >> (32 - v.width));
        emit(INSTR_AND, OPT_IMM_REG, constant(mask, w), reg(r, w));
    }

    if (v.kind == DIRECT) {
        assert(!is_array(v.type));
        emit(op, OPT_REG_MEM, reg(r, w), location_of(v, w));
    } else {
        assert(v.kind == DEREF);
        if (!v.symbol) {
            v.kind = IMMEDIATE;
            load_int(v, R11, 8);
        } else {
            assert(is_pointer(&v.symbol->type));
            load_int(var_direct(v.symbol), R11, size_of(&v.symbol->type));
        }
        emit(op, OPT_REG_MEM,
            reg(r, w), location(address(v.offset, R11, 0, 0), w));
    }
}

/* Push value to stack, rounded up to always be 8 byte aligned. */
static void push(struct var v)
{
    int eb;

    if (is_scalar(v.type)) {
        if (v.kind == IMMEDIATE && size_of(v.type) < 8) {
            emit(INSTR_PUSH, OPT_IMM, value_of(v, 8));
        } else {
            /*
             * Not possible to push SSE registers, so load as if normal
             * integer and push that instead.
             */
            if (is_real(v.type)) {
                v.type = (is_float(v.type))
                    ? &basic_type__unsigned_int
                    : &basic_type__unsigned_long;
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
 * Determine if given parameter classification can fit in registers
 * available.
 */
static int alloc_register_params(
    struct param_class pc,
    int *next_integer_reg,
    int *next_sse_reg)
{
    int ints, sses;

    if (pc.eightbyte[0] != PC_MEMORY) {
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
    const struct typetree *type;

    type = var.type;
    n = EIGHTBYTES(type);
    for (i = 0; i < n; ++i) {
        switch (pc.eightbyte[i]) {
        case PC_INTEGER:
            r = *intregs++;
            if (!is_scalar(type)) {
                var.type = (i < n - 1) ?
                    &basic_type__unsigned_long :
                    BASIC_TYPE_UNSIGNED(size_of(type) % 8);
            }
            break;
        case PC_SSE:
            r = *sseregs++;
            if (size_of(type) % 8 == 4) {
                var.type = &basic_type__float;
            } else {
                assert(size_of(type) % 8 == 0);
                var.type = &basic_type__double;
            }
            break;
        default: assert(0);
        }

        if (toggle_load) {
            load(var, r);
        } else {
            store(r, var);
        }

        var.offset += 8;
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
static int push_function_arguments(
    const struct typetree *type,
    struct param_class respc)
{
    int i, n;
    int next_integer_reg = 0, next_sse_reg = 0, mem_used = 0, register_args = 0;
    struct param_class pc;
    struct var arg;
    struct {
        struct param_class pc;
        int i;
    } argpc[MAX_REGISTER_ARGS];

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

/* Compile a function call. */
static void call(struct var func, int mem_used)
{
    if (func.kind == ADDRESS) {
        assert(!func.offset);
        emit(INSTR_CALL, OPT_IMM, addr(func.symbol));
    } else {
        assert(func.kind != IMMEDIATE);
        load(func, R11);
        emit(INSTR_CALL, OPT_REG, reg(R11, 8));
    }

    if (mem_used) {
        emit(INSTR_ADD, OPT_IMM_REG, constant(mem_used, 8), reg(SP, 8));
    }
}

/*
 * Generic function call, with assignment of the result back to a
 * variable. This is used for functions that return non-scalar values,
 * which have to live in an IR_ASSIGN statement.
 *
 * Results of class PC_MEMORY has already been written by callee.
 */
static void function_call(struct var res, struct var func)
{
    int n;
    struct param_class pc;
    assert(is_pointer(func.type));
    assert(is_function(func.type->next));

    pc = classify(func.type->next->next);
    n = push_function_arguments(func.type->next, pc);
    if (pc.eightbyte[0] == PC_MEMORY) {
        load_address(res, param_int_reg[0]);
    }

    call(func, n);
    if (pc.eightbyte[0] != PC_MEMORY) {
        store_object_from_registers(res, pc, ret_int_reg, ret_sse_reg);
    }
}

/*
 * Compile function call returning a single scalar value, or void. This
 * can be used on standalone struct expression nodes, for example in
 * branch conditions.
 */
static enum reg scalar_function_call(struct var func)
{
    struct param_class pc;
    assert(is_pointer(func.type) && is_function(func.type->next));

    pc = classify(func.type->next->next);
    assert(pc.eightbyte[0] != PC_MEMORY);
    assert(EIGHTBYTES(func.type->next->next) == 1);

    call(func, push_function_arguments(func.type->next, pc));
    return pc.eightbyte[0] == PC_SSE ? ret_sse_reg[0] : ret_int_reg[0];
}

static void enter(struct definition *def)
{
    int i, n,
        register_args = 0,  /* Arguments passed in registers. */
        next_integer_reg = 0,
        next_sse_reg = 0,
        mem_offset = 16,    /* Offset of PC_MEMORY parameters. */
        stack_offset = 0;   /* Offset of %rsp to for local variables. */
    struct var ref;
    struct symbol *sym;
    struct param_class res, arg;
    struct {
        struct param_class pc;
        int i;
    } argpc[MAX_REGISTER_ARGS];
    const struct typetree *type;

    type = &def->symbol->type;
    assert(is_function(type));

    /*
     * Address of return value is passed as first integer argument. If
     * return value is MEMORY, store the address at stack offset -8.
     */
    res = classify(type->next);
    if (res.eightbyte[0] == PC_MEMORY) {
        stack_offset = -8;
        next_integer_reg = 1;
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
        arg = classify(&sym->type);
        n = EIGHTBYTES(&sym->type);
        assert(!sym->stack_offset);
        assert(sym->linkage == LINK_NONE);
        if (alloc_register_params(arg, &next_integer_reg, &next_sse_reg)) {
            argpc[register_args].pc = arg;
            argpc[register_args].i = i;
            register_args++;
            stack_offset -= n * 8;
            sym->stack_offset = stack_offset;
        } else {
            sym->stack_offset = mem_offset;
            mem_offset += n * 8;
        }
    }

    /*
     * Assign storage to locals. Round up to nearest eightbyte, making
     * all variables aligned.
     */
    for (i = 0; i < array_len(&def->locals); ++i) {
        sym = array_get(&def->locals, i);
        assert(!sym->stack_offset);
        if (sym->linkage == LINK_NONE) {
            stack_offset -= EIGHTBYTES(&sym->type) * 8;
            sym->stack_offset = stack_offset;
        }
    }

    /*
     * Make invariant to have %rsp aligned to 0x10, which is mandatory
     * according to the ABI for function calls. On entry, (%rsp + 8) is
     * a multiple of 16, and after pushing %rbp we have alignment at
     * this point. Alignment must also be considered when calling other
     * functions, to push memory divisible by 16.
     */
    if (stack_offset < 0) {
        i = (-stack_offset) % 16;
        stack_offset -= 16 - i;
    }

    /*
     * Allocate space in the call frame to hold local variables. Store
     * return address to well known location -8(%rbp).
     */
    if (stack_offset < 0) {
        emit(INSTR_SUB, OPT_IMM_REG, constant(-stack_offset, 8), reg(SP, 8));
        if (res.eightbyte[0] == PC_MEMORY) {
            emit(INSTR_MOV, OPT_REG_MEM,
                reg(param_int_reg[0], 8), location(address(-8, BP, 0, 0), 8));
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
        sym = sym_create_label();
        vararg.gp_offset = 8*next_integer_reg;
        vararg.fp_offset = 8*MAX_INTEGER_ARGS + 16*next_sse_reg;
        vararg.overflow_arg_area_offset = mem_offset;
        vararg.reg_save_area_offset = 0;
        emit(INSTR_TEST, OPT_REG_REG, reg(AX, 1), reg(AX, 1));
        emit(INSTR_JZ, OPT_IMM, addr(sym));
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
}

/*
 * Execute call to va_start, initializing the provided va_list object.
 * Values are taken from static context set during enter(). Address of
 * va_args object is evaluated from expression.
 */
static void compile__builtin_va_start(struct var args)
{
    assert(args.kind == DIRECT);

    emit(INSTR_MOV, OPT_IMM_MEM,
        constant(vararg.gp_offset, 4),
        location_of(args, 4));
    args.offset += 4;
    emit(INSTR_MOV, OPT_IMM_MEM,
        constant(vararg.fp_offset, 4),
        location_of(args, 4));
    args.offset += 4;
    emit(INSTR_LEA, OPT_MEM_REG,
        location(address(vararg.overflow_arg_area_offset, BP, 0, 0), 8),
        reg(AX, 8));
    emit(INSTR_MOV, OPT_REG_MEM,
        reg(AX, 8),
        location_of(args, 8));
    args.offset += 8;
    emit(INSTR_LEA, OPT_MEM_REG,
        location(address(vararg.reg_save_area_offset, BP, 0, 0), 8),
        reg(AX, 8));
    emit(INSTR_MOV, OPT_REG_MEM,
        reg(AX, 8),
        location_of(args, 8));
}

/*
 * Output logic for fetching a parameter calling res = va_arg(args, T).
 * Type of args should always be pointer to first element of array.
 */
static void compile__builtin_va_arg(struct var res, struct var args)
{
    const int w = size_of(res.type);
    int n,
        integer_regs_loaded = 0,
        sse_regs_loaded = 0;
    struct param_class pc;

    struct var
        var_gp_offset,
        var_fp_offset,
        var_overflow_arg_area,
        var_reg_save_area;

    /* Get some unique jump labels. */
    const struct symbol
        *memory = sym_create_label(),
        *done = sym_create_label();

    /* Rewriting references only works for specific input. */
    assert(res.kind == DIRECT || res.kind == DEREF);
    assert(args.kind == ADDRESS || args.kind == DIRECT);
    assert(is_pointer(args.type));
    assert(!args.offset);

    /*
     * References into va_list object, offset and rewrite types to get
     * direct references to each field.
     */
    if (args.kind == ADDRESS) {
        args.kind = DIRECT;
    } else {
        args.kind = DEREF;
    }

    var_gp_offset = args;
    var_gp_offset.type = &basic_type__unsigned_int;
    var_fp_offset = args;
    var_fp_offset.offset += 4;
    var_fp_offset.type = &basic_type__unsigned_int;
    var_overflow_arg_area = args;
    var_overflow_arg_area.offset += 8;
    var_overflow_arg_area.type = &basic_type__unsigned_long;
    var_reg_save_area = args;
    var_reg_save_area.offset += 16;
    var_reg_save_area.type = &basic_type__unsigned_long;

    /*
     * Integer or SSE parameters are read from registers, if there are
     * enough of them left. Otherwise read from overflow area.
     */
    pc = classify(res.type);
    if (pc.eightbyte[0] != PC_MEMORY) {
        struct var slice = res;
        int i,
            width,
            num_gp = 0, /* General purpose registers needed. */
            num_fp = 0; /* Floating point registers needed. */

        n = EIGHTBYTES(res.type);
        count_register_classifications(pc, &num_gp, &num_fp);

        /*
         * Keep reg_save_area in register for the remainder, a pointer
         * to stack where registers are stored. This value does not
         * change.
         */
        load(var_reg_save_area, SI);

        /*
         * Check whether there are enough registers left for the
         * argument to be passed in, comparing gp_offset and fp_offset
         * with max values.
         */
        if (num_gp) {
            load(var_gp_offset, CX);
            emit(INSTR_CMP, OPT_IMM_REG,
                constant(MAX_INTEGER_ARGS*8 - 8*num_gp, 4), reg(CX, 4));
            emit(INSTR_JA, OPT_IMM, addr(memory));
        }
        if (num_fp) {
            /*
             * Max fp_offset is 6*8 + 8*16, pointing past also the first
             * integer registers. In ABI it sais 16*16, but that has to
             * be wrong. Only 8 registers are used for arguments.
             */
            load(var_fp_offset, DX);
            emit(INSTR_CMP, OPT_IMM_REG,
                constant(MAX_INTEGER_ARGS*8 + MAX_SSE_ARGS*16 - 16*num_fp, 4),
                reg(DX, 4));
            emit(INSTR_JA, OPT_IMM, addr(memory));
        }

        /*
         * Load argument, one eightbyte at a time. Similar to routines
         * moving object to and from register, but here we are getting
         * the values from spill area.
         */
        while ((i = integer_regs_loaded + sse_regs_loaded) < n) {
            switch (pc.eightbyte[i]) {
            case PC_INTEGER:
                if (!is_scalar(res.type)) {
                    slice.type = (i < n - 1) ?
                        &basic_type__unsigned_long :
                        BASIC_TYPE_UNSIGNED(size_of(res.type) % 8);
                }
                /*
                 * Advanced addressing, load (%rsi + 8*i + (%rcx * 1))
                 * into %rax. Base of registers are stored in %rsi,
                 * first pending register is at offset %rcx, and i
                 * counts number of registers done. In GNU assembly it
                 * is {i*8}(%rsi, %rcx, 1).
                 */
                i = integer_regs_loaded++;
                width = size_of(slice.type);
                emit(INSTR_MOV, OPT_MEM_REG,
                    location(address(i*8, SI, CX, 1), width), reg(AX, width));
                store(AX, slice);
                break;
            case PC_SSE:
                i = sse_regs_loaded++;
                /* Floating arguments to vararg is always double. */
                slice.type = &basic_type__double;
                emit(INSTR_MOVSD, OPT_MEM_REG,
                    location(address(i*16, SI, DX, 1), 8), reg(XMM0, 8));
                store(XMM0, slice);
                break;
            default: assert(0);
            }

            slice.offset += size_of(slice.type);
        }

        /* Store updated offsets to va_list. */
        if (num_gp) {
            if (var_gp_offset.kind == DIRECT) {
                emit(INSTR_ADD, OPT_IMM_MEM,
                    constant(8 * num_gp, 4), location_of(var_gp_offset, 4));
            } else {
                load(var_gp_offset, AX);
                emit(INSTR_ADD, OPT_IMM_REG,
                    constant(8 * num_gp, 4), reg(AX, 4));
                store(AX, var_gp_offset);
            }
        }
        if (num_fp) {
            if (var_fp_offset.kind == DIRECT) {
                emit(INSTR_ADD, OPT_IMM_MEM,
                    constant(16 * num_fp, 4), location_of(var_fp_offset, 4));
            } else {
                load(var_fp_offset, AX);
                emit(INSTR_ADD, OPT_IMM_REG,
                    constant(8 * num_fp, 4), reg(AX, 4));
                store(AX, var_fp_offset);
            }
        }

        emit(INSTR_JMP, OPT_IMM, addr(done));
        enter_context(memory);
    }

    /*
     * Parameters that are passed on stack will be read from
     * overflow_arg_area. This is also the fallback when arguments
     * do not fit in remaining registers.
     */
    load(var_overflow_arg_area, SI);
    if (w <= 8 && res.kind == DIRECT) {
        emit(INSTR_MOV, OPT_MEM_REG,
            location(address(0, SI, 0, 0), w), reg(AX, w));
        emit(INSTR_MOV, OPT_REG_MEM, reg(AX, w), location_of(res, w));
    } else {
        load_address(res, DI);
        emit(INSTR_MOV, OPT_IMM_REG, constant(w, 8), reg(DX, 8));
        emit(INSTR_CALL, OPT_IMM, addr(decl_memcpy));
    }

    /*
     * Move overflow_arg_area pointer to position of next memory
     * argument, aligning to 8 byte.
     */
    if (var_overflow_arg_area.kind == DIRECT) {
        emit(INSTR_ADD, OPT_IMM_MEM,
            constant(EIGHTBYTES(res.type) * 8, 4),
            location_of(var_overflow_arg_area, 4));
    } else {
        load(var_overflow_arg_area, AX);
        emit(INSTR_ADD, OPT_IMM_REG,
            constant(EIGHTBYTES(res.type) * 8, 4), reg(AX, 4));
        store(AX, var_overflow_arg_area);
    }

    if (pc.eightbyte[0] != PC_MEMORY) {
        enter_context(done);
    }
}

/*
 * Expressions are evaluated and result stored in either register, a set
 * of registers, or as an address to a memory location.
 */
struct result {
    enum {
        VAL_REG,     /* Regular register value. */
        VAL_VAR,     /* Return object not fitting in register. */
        VAL_FLAGS    /* From comparison. */
    } kind;
    enum reg r;
    enum opcode cmp;
    struct var var;
};

/*
 * Emit instructions for evaluating expression, and store the result in
 * a suitable register.
 */
static struct result compile_expression(struct expression expr)
{
    int w;
    struct var l, r;
    enum opcode opc;
    enum reg ax, cx, xmm0, xmm1;
    struct param_class pc;
    const struct typetree *type;
    struct result res = {0};

    w = size_of(expr.type);
    l = expr.l, r = expr.r;
    type = expr.type;
    res.kind = VAL_REG;

    switch (expr.op) {
    case IR_OP_CAST:
        if (is_array(type)) {
            assert(is_string(l));
            res.var = l;
            res.kind = VAL_VAR;
        } else if (!is_standard_register_width(size_of(type))) {
            res.var = l;
            res.kind = VAL_VAR;
        } else {
            if (is_struct_or_union(type)) {
                pc = classify(type);
                switch (pc.eightbyte[0]) {
                case PC_INTEGER:
                    type = BASIC_TYPE_UNSIGNED(size_of(type));
                    break;
                case PC_SSE:
                    type = (size_of(type) == 4)
                        ? &basic_type__float
                        : &basic_type__double;
                    break;
                default: assert(0);
                }
                l.type = type;
            }
            res.r = load_cast(l, type);
        }
        break;
    case IR_OP_CALL:
        if (is_scalar(expr.type)) {
            res.r = scalar_function_call(expr.l);
            break;
        }
    case IR_OP_VA_ARG:
        /* These are handled per statement. */
        assert(0);
        break;
    case IR_OP_NOT:
        res.r = load(l, AX);
        emit(INSTR_NOT, OPT_REG, reg(AX, w));
        break;
    case IR_OP_ADD:
        opc = is_float(type) ? INSTR_ADDSS
            : is_double(type) ? INSTR_ADDSD : INSTR_ADD;
        ax = load_cast(l, type);
        cx = load_cast(r, type);
        emit(opc, OPT_REG_REG, reg(cx, w), reg(ax, w));
        res.r = ax;
        break;
    case IR_OP_SUB:
        opc = is_float(type) ? INSTR_SUBSS
            : is_double(type) ? INSTR_SUBSD : INSTR_SUB;
        ax = load_cast(l, type);
        cx = load_cast(r, type);
        emit(opc, OPT_REG_REG, reg(cx, w), reg(ax, w));
        res.r = ax;
        break;
    case IR_OP_MUL:
        if (is_real(type)) {
            xmm0 = load_cast(l, type);
            xmm1 = load_cast(r, type);
            opc = is_float(type) ? INSTR_MULSS : INSTR_MULSD;
            emit(opc, OPT_REG_REG, reg(xmm1, w), reg(xmm0, w));
            res.r = xmm0;
        } else {
            res.r = load(r, AX);
            if (l.kind == DIRECT) {
                emit(INSTR_MUL, OPT_MEM, location_of(l, size_of(l.type)));
            } else {
                load(l, CX);
                emit(INSTR_MUL, OPT_REG, reg(CX, w));
            }
        }
        break;
    case IR_OP_DIV:
        if (is_real(type)) {
            xmm0 = load_cast(l, type);
            xmm1 = load_cast(r, type);
            opc = is_float(type) ? INSTR_DIVSS : INSTR_DIVSD;
            emit(opc, OPT_REG_REG, reg(xmm1, w), reg(xmm0, w));
            res.r = xmm0;
            break;
        }
    case IR_OP_MOD:
        assert(!is_real(type));
        res.r = load(l, AX);
        /* Divident is %rdx:%rax, zero extend if type is signed. */
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
        if (r.kind == DIRECT) {
            emit(is_signed(type) ? INSTR_IDIV : INSTR_DIV,
                OPT_MEM,
                location_of(r, size_of(r.type)));
        } else {
            load(r, CX);
            emit(is_signed(type) ? INSTR_IDIV : INSTR_DIV,
                OPT_REG,
                reg(CX, size_of(r.type)));
        }
        res.r = (expr.op == IR_OP_DIV) ? AX : DX;
        break;
    case IR_OP_AND:
        load(l, AX);
        load(r, CX);
        emit(INSTR_AND, OPT_REG_REG, reg(CX, 8), reg(AX, 8));
        res.r = AX;
        break;
    case IR_OP_OR:
        load(l, AX);
        load(r, CX);
        emit(INSTR_OR, OPT_REG_REG, reg(CX, 8), reg(AX, 8));
        res.r = AX;
        break;
    case IR_OP_XOR:
        load(l, AX);
        load(r, CX);
        emit(INSTR_XOR, OPT_REG_REG, reg(CX, 8), reg(AX, 8));
        res.r = AX;
        break;
    case IR_OP_SHL:
        load(l, AX);
        load(r, CX);
        /*
         * Shift instruction encoding is either by immediate, or
         * implicit %cl register. Encode as if something other than %cl
         * could be chosen. Behavior is undefined if shift is greater
         * than integer width, so don't care about overflow or sign.
         */
        emit(INSTR_SHL, OPT_REG_REG, reg(CX, 1), reg(AX, size_of(type)));
        res.r = AX;
        break;
    case IR_OP_SHR:
        load(l, AX);
        load(r, CX);
        emit((is_unsigned(type)) ? INSTR_SHR : INSTR_SAR,
            OPT_REG_REG,
            reg(CX, 1),
            reg(AX, size_of(type)));
        res.r = AX;
        break;
    case IR_OP_EQ:
        res.cmp = INSTR_SETZ;
        goto compare;
    case IR_OP_NE:
        res.cmp = INSTR_SETNE;
        goto compare;
    case IR_OP_GE:
        res.cmp =
            is_unsigned(l.type) || is_real(l.type) ? INSTR_SETAE : INSTR_SETGE;
        goto compare;
    case IR_OP_GT:
        res.cmp =
            is_unsigned(l.type) || is_real(l.type) ? INSTR_SETA : INSTR_SETG;
compare:
        res.kind = VAL_FLAGS;
        if (is_real(l.type)) {
            xmm0 = load_cast(l, l.type);
            xmm1 = load_cast(r, r.type);
            if (is_float(l.type)) {
                emit(INSTR_UCOMISS, OPT_REG_REG, reg(xmm1, 4), reg(xmm0, 4));
            } else {
                emit(INSTR_UCOMISD, OPT_REG_REG, reg(xmm1, 8), reg(xmm0, 8));
            }
        } else {
            load(l, AX);
            load(r, CX);
            emit(INSTR_CMP, OPT_REG_REG,
                reg(CX, size_of(l.type)), reg(AX, size_of(l.type)));
        }
        break;
    }

    return res;
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
static enum reg set_compare_value(
    const struct typetree *type,
    enum opcode op)
{
    emit(op, OPT_REG, reg(AX, 1));
    if (is_real(type)) {
        if (op == INSTR_SETZ) {
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
static void store_copy_object(struct var var, struct var res)
{
    if (is_array(var.type)) {
        assert(res.kind == DIRECT);
        assert(is_string(var));
        assert(type_equal(res.type, var.type));
        emit(INSTR_MOV, OPT_IMM_REG, addr(var.symbol), reg(SI, 8));
    } else {
        load_address(var, SI);
    }

    load_address(res, DI);
    emit(INSTR_MOV, OPT_IMM_REG, constant(size_of(res.type), 4), reg(DX, 4));
    emit(INSTR_CALL, OPT_IMM, addr(decl_memcpy));
}

/*
 */
static void compile_statement(struct statement stmt)
{
    struct param_class pc;
    struct result res;

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
    case IR_ASSIGN:
        if (stmt.expr.op == IR_OP_CALL) {
            function_call(stmt.t, stmt.expr.l);
            break;
        } else if (stmt.expr.op == IR_OP_VA_ARG) {
            assert(stmt.st == IR_ASSIGN);
            compile__builtin_va_arg(stmt.t, stmt.expr.l);
            break;
        }

        res = compile_expression(stmt.expr);
        switch (res.kind) {
        case VAL_FLAGS:
            assert(type_equal(stmt.expr.l.type, stmt.expr.r.type));
            res.r = set_compare_value(stmt.expr.l.type, res.cmp);
        case VAL_REG:
            if (is_struct_or_union(stmt.t.type)) {
                pc = classify(stmt.t.type);
                switch (pc.eightbyte[0]) {
                case PC_INTEGER:
                    assert(res.r >= AX && res.r <= R15);
                    stmt.t.type = BASIC_TYPE_UNSIGNED(size_of(stmt.t.type));
                    break;
                case PC_SSE:
                    assert(res.r >= XMM0 && res.r <= XMM8);
                    stmt.t.type = (size_of(stmt.t.type) == 4)
                        ? &basic_type__float
                        : &basic_type__double;
                    break;
                default: assert(0);
                }
            }
            store(res.r, stmt.t);
            break;
        case VAL_VAR:
            store_copy_object(res.var, stmt.t);
            break;
        }
        break;
    }

    relase_regs();
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
static void ret(struct var var, const struct typetree *type)
{
    struct param_class pc;

    assert(is_function(type));
    pc = classify(type->next);
    if (pc.eightbyte[0] != PC_MEMORY) {
        load_object_to_registers(var, pc, ret_int_reg, ret_sse_reg);
    } else {
        if (is_vararg(type)) {
            emit(INSTR_MOV, OPT_MEM_REG,
                location(address(-176, BP, 0, 0), 8), reg(DI, 8));
        } else {
            emit(INSTR_MOV, OPT_MEM_REG,
                location(address(-8, BP, 0, 0), 8), reg(DI, 8));
        }
        load_address(var, SI);
        emit(INSTR_MOV, OPT_IMM_REG,
            constant(size_of(type->next), 8), reg(DX, 4));
        emit(INSTR_CALL, OPT_IMM, addr(decl_memcpy));
        emit(INSTR_MOV, OPT_MEM_REG,
            location(address(-8, BP, 0, 0), 8), reg(AX, 8));
    }
}

/*
 * Emit code for all statements in a block, jump to children based on
 * compare result, or return value in case of no children.
 *
 * Most of the complexity deals with interpreting the last block->expr
 * object, branchhing to the correct next block. All scalar expressions
 * are allowed.
 */
static void compile_block(struct block *block, const struct typetree *type)
{
    int i;
    enum reg xmm0, xmm1;
    struct result res;
    struct statement st;
    struct param_class pc;

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
            assert(type_equal(block->expr.type, type->next));
            pc = classify(type->next);
            res = compile_expression(block->expr);
            switch (res.kind) {
            case VAL_FLAGS:
                res.r = set_compare_value(block->expr.l.type, res.cmp);
            case VAL_REG:
                i = size_of(type->next);
                switch (pc.eightbyte[0]) {
                case PC_INTEGER:
                    if (res.r != ret_int_reg[0]) {
                        assert(is_standard_register_width(i));
                        emit(INSTR_MOV, OPT_REG_REG,
                            reg(res.r, i), reg(ret_int_reg[0], i));
                    }
                    break;
                case PC_SSE:
                    if (res.r != ret_sse_reg[0]) {
                        assert(i == 8 || i == 4);
                        emit(INSTR_MOV, OPT_REG_REG,
                            reg(res.r, i), reg(ret_sse_reg[0], i));
                    }
                    break;
                default: assert(0);
                }
                break;
            case VAL_VAR:
                ret(res.var, type);
                break;
            }
            relase_regs();
        }
        emit(INSTR_LEAVE, OPT_NONE);
        emit(INSTR_RET, OPT_NONE);
    } else if (!block->jump[1]) {
        if (block->jump[0]->color == BLACK) {
            emit(INSTR_JMP, OPT_IMM, addr(block->jump[0]->label));
        } else {
            compile_block(block->jump[0], type);
        }
    } else {
        assert(is_scalar(block->expr.type));
        res = compile_expression(block->expr);
        if (res.kind == VAL_FLAGS) {
            assert(type_equal(&basic_type__int, block->expr.type));
            switch (res.cmp) {
            case INSTR_SETZ:
                if (is_real(block->expr.l.type)) {
                    emit(INSTR_JNE, OPT_IMM, addr(block->jump[0]->label));
                    emit(INSTR_JP, OPT_IMM, addr(block->jump[0]->label));
                    emit(INSTR_JMP, OPT_IMM, addr(block->jump[1]->label));
                } else {
                    emit(INSTR_JZ, OPT_IMM, addr(block->jump[1]->label));
                }
                break;
            case INSTR_SETNE:
                if (is_real(block->expr.l.type)) {
                    emit(INSTR_JNE, OPT_IMM, addr(block->jump[1]->label));
                    emit(INSTR_JP, OPT_IMM, addr(block->jump[1]->label));
                    emit(INSTR_JMP, OPT_IMM, addr(block->jump[0]->label));
                } else {
                    emit(INSTR_JNE, OPT_IMM, addr(block->jump[1]->label));
                }
                break;
            case INSTR_SETG:
                emit(INSTR_JG, OPT_IMM, addr(block->jump[1]->label));
                break;
            case INSTR_SETA:
                emit(INSTR_JA, OPT_IMM, addr(block->jump[1]->label));
                break;
            case INSTR_SETGE:
                emit(INSTR_JGE, OPT_IMM, addr(block->jump[1]->label));
                break;
            case INSTR_SETAE:
                emit(INSTR_JAE, OPT_IMM, addr(block->jump[1]->label));
                break;
            default: assert(0);
            }
        } else if (is_real(block->expr.type)) {
            assert(res.kind == VAL_REG);
            xmm0 = res.r;
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
            assert(res.kind == VAL_REG);
            i = size_of(block->expr.type);
            assert(i == 1 || i == 2 || i == 4 || i == 8);
            emit(INSTR_CMP, OPT_IMM_REG, constant(0, i), reg(res.r, i));
            emit(INSTR_JNE, OPT_IMM, addr(block->jump[1]->label));
        }

        relase_regs();
        if (block->jump[0]->color == BLACK) {
            emit(INSTR_JMP, OPT_IMM, addr(block->jump[0]->label));
        } else {
            compile_block(block->jump[0], type);
        }

        compile_block(block->jump[1], type);
    }
}

static void compile_data_assign(struct var target, struct var val)
{
    struct immediate imm = {0};
    assert(target.kind == DIRECT);

    imm.w = target.type->size;
    if (val.kind == IMMEDIATE) {
        switch (target.type->type) {
        case T_POINTER:
            if (is_string(val)) {
                imm.type = IMM_ADDR;
                imm.d.addr.sym = val.symbol;
                imm.d.addr.disp = val.offset;
                break;
            }
        case T_SIGNED:
        case T_UNSIGNED:
        case T_REAL:
            assert(type_equal(target.type, val.type));
            imm.type = IMM_INT;
            imm.d.qword = val.imm.i;
            break;
        case T_ARRAY:
            if (is_string(val)) {
                imm.type = IMM_STRING;
                imm.d.string = val.symbol->string_value;
                break;
            }
        default: assert(0);
        }
    } else {
        assert(val.kind == ADDRESS);
        assert(val.symbol->linkage != LINK_NONE);
        imm.type = IMM_ADDR;
        imm.d.addr.sym = val.symbol;
        imm.d.addr.disp = val.offset;
    }

    emit_data(imm);
}

static void zero_fill_data(size_t bytes)
{
    struct immediate
        zero_byte = {IMM_INT, 1},
        zero_quad = {IMM_INT, 8};

    while (bytes > size_of(&basic_type__long)) {
        emit_data(zero_quad);
        bytes -= size_of(&basic_type__long);
    }

    while (bytes--)
        emit_data(zero_byte);
}

static void compile_data(struct definition *def)
{
    struct statement stmt;
    int i,
        total_size = size_of(&def->symbol->type),
        initialized = 0;

    enter_context(def->symbol);
    for (i = 0; i < array_len(&def->body->code); ++i) {
        stmt = array_get(&def->body->code, i);

        assert(stmt.st == IR_ASSIGN);
        assert(stmt.t.kind == DIRECT);
        assert(stmt.t.symbol == def->symbol);
        assert(stmt.t.offset >= initialized);
        assert(is_identity(stmt.expr));

        zero_fill_data(stmt.t.offset - initialized);
        compile_data_assign(stmt.t, stmt.expr.l);
        initialized = stmt.t.offset + size_of(stmt.t.type);
    }

    assert(total_size >= initialized);
    zero_fill_data(total_size - initialized);
}

static void compile_function(struct definition *def)
{
    assert(is_function(&def->symbol->type));
    enter_context(def->symbol);
    emit(INSTR_PUSH, OPT_REG, reg(BP, 8));
    emit(INSTR_MOV, OPT_REG_REG, reg(SP, 8), reg(BP, 8));

    /* Make sure parameters and local variables are placed on stack. */
    enter(def);

    /* Recursively assemble body. */
    compile_block(def->body, &def->symbol->type);
}

void set_compile_target(FILE *stream)
{
    output_stream = stream;
    switch (context.target) {
    case TARGET_NONE:
    case TARGET_IR_DOT:
        break;
    case TARGET_x86_64_ASM:
        asm_output = stream;
        enter_context = asm_symbol;
        emit_instruction = asm_text;
        emit_data = asm_data;
        flush_backend = asm_flush;
        break;
    case TARGET_x86_64_ELF:
        object_file_output = stream;
        enter_context = elf_symbol;
        emit_instruction = elf_text;
        emit_data = elf_data;
        flush_backend = elf_flush;
        break;
    }
}

int compile(struct definition *def)
{
    assert(decl_memcpy);
    assert(def->symbol);

    switch (context.target) {
    case TARGET_IR_DOT:
        fdotgen(output_stream, def);
    case TARGET_NONE:
        break;
    case TARGET_x86_64_ASM:
    case TARGET_x86_64_ELF:
        if (is_function(&def->symbol->type))
            compile_function(def);
        else
            compile_data(def);
        break;
    }

    return 0;
}

int declare(const struct symbol *sym)
{
    switch (context.target) {
    case TARGET_x86_64_ASM:
    case TARGET_x86_64_ELF:
        return enter_context(sym);
    default:
        return 0;
    }
}

void flush(void)
{
    array_clear(&func_args);
    if (flush_backend) {
        flush_backend();
    }
}
