#include "graphviz/dot.h"
#include "x86_64/abi.h"
#include "x86_64/assemble.h"
#include "x86_64/elf.h"
#include "x86_64/instructions.h"
#include "compile.h"
#include <lacc/cli.h>

#include <assert.h>
#include <stdarg.h>

static enum compile_target compile_target;
static FILE *output_stream;

static int (*enter_context)(const struct symbol *);
static int (*emit_instruction)(struct instruction);
static int (*emit_data)(struct immediate);
static int (*flush_backend)(void);

/* Values from va_list initialization.
 */
static int gp_offset;
static int fp_offset;
static int overflow_arg_area_offset;
static int reg_save_area_offset;

/* Dummy declaration of memcpy, which is used for copy operations of objects
 * that do not fit in registers. The type does not matter, but it has to be
 * a prototype declaration with external linkage.
 */
static const struct symbol decl_memcpy = {
    "memcpy",
    {T_VOID},
    SYM_DECLARATION,
    LINK_EXTERN
};

static void compile_block(struct block *block, const enum param_class *res);

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
    assert(!is_array(var.type));

    if (is_string(var)) {
        assert(is_pointer(var.type));
        imm.type = IMM_ADDR;
        imm.d.addr.sym = var.symbol;
        imm.d.addr.disp = var.offset;
    } else {
        assert(!var.offset);
        assert(is_scalar(var.type));
        if (w == 1) {
            imm.type = IMM_BYTE;
            imm.d.byte = var.imm.i;
        } else if (w == 2) {
            imm.type = IMM_WORD;
            imm.d.word = var.imm.i;
        } else if (w == 4) {
            imm.type = IMM_DWORD;
            imm.d.dword = var.imm.i;
        } else {
            assert(w == 8);
            imm.type = IMM_QUAD;
            imm.d.quad = var.imm.i;
        }
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
    assert(var.kind == DIRECT);

    if (var.symbol->linkage != LINK_NONE) {
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

static struct immediate constant(int n, int w)
{
    return value_of(var_int(n), w);
}

/* Load variable v to register r, sign extended to fit register size. Width must
 * be either 4 (as in %eax) or 8 (as in %rax).
 */
static enum reg load_value(struct var v, enum reg r, int w)
{
    enum opcode opcode = INSTR_MOV;
    int s = size_of(v.type);

    /* We operate only with 32 or 64 bit register values, but variables can be
     * stored with byte or short width. Promote to 32 bit if required. */
    assert(w == 4 || w == 8);
    assert(s <= w);

    if (is_unsigned(v.type)) {
        if (s < w && s < 4)
            opcode = INSTR_MOVZX;
    } else if (s != w)
        opcode = INSTR_MOVSX;

    /* Special case for unsigned extension from 32 to 64 bit, for which there
     * is no instruction 'movzlq', but rather just 'movl'. */
    if (s == 4 && is_unsigned(v.type) && w == 8) {
        opcode = INSTR_MOV;
        w = 4;
    }

    switch (v.kind) {
    case DIRECT:
        emit(opcode, OPT_MEM_REG, location_of(v, s), reg(r, w));
        break;
    case DEREF:
        assert(is_pointer(&v.symbol->type));
        load_value(var_direct(v.symbol), R11, size_of(&v.symbol->type));
        emit(opcode, OPT_MEM_REG,
            location(address(v.offset, R11, 0, 0), s), reg(r, w));
        break;
    case IMMEDIATE:
        emit(INSTR_MOV, OPT_IMM_REG, value_of(v, w), reg(r, w));
        break;
    }

    return r;
}

/* Load variable to register, automatically sign/width extended to either 32 or
 * 64 bit.
 */
static enum reg load(struct var v, enum reg r)
{
    int w = size_of(v.type);
    if (w <= 4) w = 4;
    else
        assert(w == 8);

    return load_value(v, r, w);
}

static void load_address(struct var v, enum reg r)
{
    if (v.kind == DIRECT) {
        emit(INSTR_LEA, OPT_MEM_REG, location_of(v, 8), reg(r, 8));
    } else {
        assert(v.kind == DEREF);
        assert(v.symbol->stack_offset);
        assert(is_pointer(&v.symbol->type));

        load(var_direct(v.symbol), r);
        if (v.offset)
            emit(INSTR_ADD, OPT_IMM_REG, constant(v.offset, 8), reg(r, 8));
    }
}

static void store(enum reg r, struct var v)
{
    const int w = size_of(v.type);
    assert(is_scalar(v.type) || w <= 8);

    if (v.kind == DIRECT) {
        assert(!is_array(v.type));

        emit(INSTR_MOV, OPT_REG_MEM, reg(r, w), location_of(v, w));
    } else {
        assert(v.kind == DEREF);
        assert(is_pointer(&v.symbol->type));

        load_value(var_direct(v.symbol), R11, size_of(&v.symbol->type));
        emit(INSTR_MOV, OPT_REG_MEM,
            reg(r, w), location(address(v.offset, R11, 0, 0), w));
    }
}

/* Push value to stack, rounded up to always be 8 byte aligned.
 */
static void push(struct var v)
{
    int slices;

    if (is_scalar(v.type)) {
        if (v.kind == IMMEDIATE && size_of(v.type) == 8)
            emit(INSTR_PUSH, OPT_IMM, value_of(v, 8));
        else {
            load(v, AX);
            emit(INSTR_PUSH, OPT_REG, reg(AX, 8));
        }
    } else {
        slices = N_EIGHTBYTES(v.type);
        emit(INSTR_SUB, OPT_IMM_REG, constant(slices * 8, 8), reg(SP, 8));
        emit(INSTR_MOV, OPT_IMM_REG, constant(slices, 4), reg(CX, 4));
        emit(INSTR_MOV, OPT_REG_REG, reg(SP, 8), reg(DI, 8));
        load_address(v, SI);
        emit(INSTR_REP_MOVS, OPT_NONE);
    }
}

/* Compile a function call. For now this assumes only integer arguments passed
 * in %rdi, %rsi, %rdx, %rcx, %r8 and %r9, and arguments passed on stack.
 */
static void call(int n, const struct var *args, struct var res, struct var func)
{
    int i,
        mem_used = 0,
        next_integer_reg = 0;

    enum param_class
        *res_pc,
        **arg_pc;

    const struct typetree
        **arg_types,
        *type;

    /* Classify function arguments and return value. */
    arg_types = calloc(n, sizeof(*arg_types));
    for (i = 0; i < n; ++i) {
        arg_types[i] = args[i].type;
    }

    /* Handle both function call by direct reference and pointer. The former is
     * a special case. */
    type = is_pointer(func.type) ? func.type->next : func.type;
    assert(is_function(type));

    arg_pc = classify_call(arg_types, type->next, n, &res_pc);
    free(arg_types);

    /* Pass arguments on stack from right to left. Do this before populating
     * registers, because %rdi, %rsi etc will be used to do the pushing. */
    for (i = n - 1; i >= 0; --i) {
        if (*arg_pc[i] == PC_MEMORY) {
            mem_used += N_EIGHTBYTES(args[i].type) * 8;
            push(args[i]);
        }
    }

    /* When return value is MEMORY, pass a pointer to stack as hidden first
     * argument. */
    if (*res_pc == PC_MEMORY) {
        next_integer_reg = 1;
        load_address(res, param_int_reg[0]);
    }

    /* Pass arguments in registers from left to right. Partition arguments into
     * eightbyte slices and load into appropriate registers. */
    for (i = 0; i < n; ++i) {
        enum param_class *eightbyte = arg_pc[i];

        if (*eightbyte != PC_MEMORY) {
            if (is_struct_or_union(args[i].type)) {
                int chunks = N_EIGHTBYTES(args[i].type),
                    size = size_of(args[i].type),
                    j;
                struct var slice = args[i];

                for (j = 0; j < chunks; ++j) {
                    int w = (size < 8) ? size % 8 : 8;

                    size -= w;
                    assert(eightbyte[j] == PC_INTEGER);
                    assert(w == 1 || w == 2 || w == 4 || w == 8);

                    slice.type = BASIC_TYPE_UNSIGNED(w);
                    slice.offset = args[i].offset + j * 8;
                    load(slice, param_int_reg[next_integer_reg++]);
                }
                assert(!size);
            } else {
                assert(size_of(args[i].type) <= 8);
                load(args[i], param_int_reg[next_integer_reg++]);
            }
        }
    }

    /* For variable argument lists, %al contains the number of vector registers
     * used. */
    if (is_vararg(type))
        emit(INSTR_MOV, OPT_IMM_REG, constant(0, 4), reg(AX, 4));

    /* Call. */
    if (is_pointer(func.type)) {
        load(func, R11);
        emit(INSTR_CALL, OPT_REG, reg(R11, 8));
    } else {
        if (func.kind == DIRECT)
            emit(INSTR_CALL, OPT_IMM, addr(func.symbol));
        else {
            assert(func.kind == DEREF);
            load_address(func, R11);
            emit(INSTR_CALL, OPT_REG, reg(R11, 8));
        }
    }

    /* Reset stack pointer from overflow arguments. */
    if (mem_used)
        emit(INSTR_ADD, OPT_IMM_REG, constant(mem_used, 8), reg(SP, 8));

    /* Move return value from register(s) to memory. Return values with class
     * MEMORY have already been written by callee. */
    if (*res_pc != PC_MEMORY) {
        struct var slice = res;
        int n = N_EIGHTBYTES(res.type),
            size = size_of(res.type);

        /* Only have INTEGER class for now. */
        assert(n <= 2);

        next_integer_reg = 0;
        for (i = 0; i < n; ++i) {
            int width = (size < 8) ? size % 8 : 8;

            size -= width;
            assert(res_pc[i] == PC_INTEGER);

            slice.type = BASIC_TYPE_UNSIGNED(width);
            slice.offset = i * 8;
            store(ret_int_reg[next_integer_reg++], slice);
        }

        assert(!size);
    }

    for (i = 0; i < n; ++i)
        free(arg_pc[i]);

    free(arg_pc);
    free(res_pc);
}

/* Assign storage to local variables.
 */
static int assign_locals_storage(struct symbol_list locals, int offset)
{
    int i;

    for (i = 0; i < locals.length; ++i) {
        struct symbol *sym = locals.symbol[i];
        assert(!sym->stack_offset);

        if (sym->linkage == LINK_NONE) {
            offset -= size_of(&sym->type);
            sym->stack_offset = offset;
        }
    }

    return offset;
}

/* Load parameters into call frame on entering a function. Return parameter
 * class of return value.
 */
static enum param_class *enter(
    const struct typetree *type,
    struct symbol_list params,
    struct symbol_list locals)
{
    int i,
        next_integer_reg = 0,
        mem_offset = 16,    /* Offset of PC_MEMORY parameters. */
        stack_offset = 0;   /* Offset of %rsp to keep local variables. */

    enum param_class
        **params_pc,
        *ret_pc;

    assert(is_function(type));

    /* Get classification of function arguments and return value. */
    params_pc = classify_signature(type, &ret_pc);

    /* Address of return value is passed as first integer argument. If return
     * value is MEMORY, store the address at stack offset -8. */
    if (*ret_pc == PC_MEMORY) {
        stack_offset = -8;
        next_integer_reg = 1;
    }

    /* For functions with variable argument list, reserve a fixed area at the
     * beginning of the stack fram for register values. In total there are 8
     * bytes for each of the 6 integer registers, and 16 bytes for each of the 8
     * SSE registers, for a total of 176 bytes. We want to keep the register
     * save area fixed regardless of parameter class of return value, so skip
     * the first 8 bytes used for return value address. */
    if (is_vararg(type)) {
        stack_offset = -176 - 8;
    }

    /* Assign storage to parameters. */
    for (i = 0; i < params.length; ++i) {
        struct symbol *sym = params.symbol[i];

        assert(!sym->stack_offset);
        assert(sym->linkage == LINK_NONE);

        /* Guarantee that parameters are 8-byte aligned also for those passed by
         * register, which makes it easier to store in local frame after
         * entering function. Might want to revisit this and make it compact. */
        if (*params_pc[i] == PC_MEMORY) {
            sym->stack_offset = mem_offset;
            mem_offset += N_EIGHTBYTES(&sym->type) * 8;
        } else {
            stack_offset -= N_EIGHTBYTES(&sym->type) * 8;
            sym->stack_offset = stack_offset;
        }
    }

    /* Assign storage to locals. */
    stack_offset = assign_locals_storage(locals, stack_offset);
    if (stack_offset < 0)
        emit(INSTR_SUB, OPT_IMM_REG, constant(-stack_offset, 8), reg(SP, 8));

    /* Store return address to well known stack offset. */
    if (*ret_pc == PC_MEMORY)
        emit(INSTR_MOV, OPT_REG_MEM,
            reg(param_int_reg[0], 8), location(address(-8, BP, 0, 0), 8));

    /* Store all potential parameters to register save area. This includes
     * parameters that are known to be passed as registers, that will anyway be
     * stored to another stack location. Maybe potential for optimization. */
    if (is_vararg(type)) {
        const struct symbol *lbl = sym_create_label();

        /* It is desireable to skip touching floating point unit if possible,
         * %al holds the number of floating point registers passed. */
        emit(INSTR_TEST, OPT_REG_REG, reg(AX, 1), reg(AX, 1));
        emit(INSTR_JZ, OPT_IMM, addr(lbl));
        reg_save_area_offset = -8; /* Skip address of return value. */
        for (i = 0; i < 8; ++i) {
            reg_save_area_offset -= 16;
            emit(INSTR_MOVAPS, OPT_REG_MEM,
                reg(XMM0 + (7 - i), 16),
                location(address(reg_save_area_offset, BP, 0, 0), 16));
        }

        enter_context(lbl);
        for (i = 0; i < 6; ++i) {
            reg_save_area_offset -= 8;
            emit(INSTR_MOV, OPT_REG_MEM,
                reg(param_int_reg[5 - i], 8),
                location(address(reg_save_area_offset, BP, 0, 0), 8));
        }
    }

    /* Move arguments from register to stack. */
    for (i = 0; i < params.length; ++i) {
        enum param_class *eightbyte = params_pc[i];

        /* Here it is ok to not separate between object and other types. Data in
         * registers can always be treated as integer type. */
        if (*eightbyte != PC_MEMORY) {
            struct var ref = {NULL, NULL, DIRECT};
            int n = N_EIGHTBYTES(get_member(type, i)->type),
                size = size_of(get_member(type, i)->type),
                j;

            ref.symbol = params.symbol[i];
            for (j = 0; j < n; ++j) {
                int width = (size < 8) ? size : 8;
                ref.type = BASIC_TYPE_UNSIGNED(width);
                ref.offset = j * 8;
                store(param_int_reg[next_integer_reg++], ref);
                size -= width;
            }
            assert(!size);
        }
    }

    /* After loading parameters we know how many registers have been used for
     * fixed parameters. Update offsets to be used in va_start. */
    if (is_vararg(type)) {
        gp_offset = 8 * next_integer_reg;
        fp_offset = 0;
        overflow_arg_area_offset = mem_offset;
    }

    for (i = 0; i < params.length; ++i)
        free(params_pc[i]);
    free(params_pc);

    return ret_pc;
}

/* Return value from function, placing it in register(s) or writing it to stack
 * based on parameter class.
 */
static void ret(struct var val, const enum param_class *pc)
{
    assert(pc && *pc != PC_NO_CLASS);

    /* NB: This might break down for non-object values, in particular string
     * constants that cannot be interpreted as integers. */
    if (*pc != PC_MEMORY) {
        int i,
            n = N_EIGHTBYTES(val.type),
            size = size_of(val.type);
        struct var slice = val;

        /* As we only support integer class, limit to two registers. Note that
         * the classification algorithm will never allocate more than two
         * integer registers for a single type. */
        assert(n <= 2);

        /* This has a lot in common with call and enter, and could probably be
         * refactored. */
        for (i = 0; i < n; ++i) {
            int width = (size < 8) ? size % 8 : 8;

            size -= width;
            assert(pc[i] == PC_INTEGER);
            assert(width == 1 || width == 2 || width == 4 || width == 8);

            slice.type = BASIC_TYPE_UNSIGNED(width);
            slice.offset = val.offset + i * 8;
            load(slice, ret_int_reg[i]);
        }

        assert(!size);
    } else {
        /* Load return address from magic stack offset and copy result. */
        emit(INSTR_MOV, OPT_MEM_REG,
            location(address(-8, BP, 0, 0), 8), reg(DI, 8));
        load_address(val, SI);
        emit(INSTR_MOV, OPT_IMM_REG,
            constant(size_of(val.type), 8), reg(DX, 4));
        emit(INSTR_CALL, OPT_IMM, addr(&decl_memcpy));

        /* The ABI specifies that the address should be in %rax on return. */
        emit(INSTR_MOV, OPT_MEM_REG,
            location(address(-8, BP, 0, 0), 8), reg(AX, 8));
    }
}

/* Execute call to va_start, initializing the provided va_list object. Values
 * are taken from static context set during enter().
 */
static void compile__builtin_va_start(struct var args)
{
    assert(args.kind == DIRECT);

    emit(INSTR_MOV, OPT_IMM_MEM, constant(gp_offset, 4), location_of(args, 4));

    args.offset += 4;
    emit(INSTR_MOV, OPT_IMM_MEM, constant(fp_offset, 4), location_of(args, 4));

    args.offset += 4;
    emit(INSTR_LEA, OPT_MEM_REG,
        location(address(overflow_arg_area_offset, BP, 0, 0), 8), reg(AX, 8));
    emit(INSTR_MOV, OPT_REG_MEM, reg(AX, 8), location_of(args, 8));

    args.offset += 8;
    emit(INSTR_LEA, OPT_MEM_REG,
        location(address(reg_save_area_offset, BP, 0, 0), 8), reg(AX, 8));
    emit(INSTR_MOV, OPT_REG_MEM, reg(AX, 8), location_of(args, 8));
}

/* Output logic for fetching a parameter from calling va_arg(args, T).
 */
static void compile__builtin_va_arg(struct var res, struct var args)
{
    const int w = size_of(res.type);
    enum param_class *pc = classify(res.type);
    struct var
        var_gp_offset = args,
        var_fp_offset = args,
        var_overflow_arg_area = args,
        var_reg_save_area = args;

    /* Get some unique jump labels. */
    const struct symbol
        *memory = sym_create_label(),
        *done = sym_create_label();

    /* Might be too restrictive for res, but simplifies some codegen. */
    assert(res.kind == DIRECT);
    assert(args.kind == DIRECT);

    /* References into va_list object. */
    var_gp_offset.type = &basic_type__unsigned_int;
    var_fp_offset.offset += 4;
    var_fp_offset.type = &basic_type__unsigned_int;
    var_overflow_arg_area.offset += 8;
    var_overflow_arg_area.type = &basic_type__unsigned_long;
    var_reg_save_area.offset += 16;
    var_reg_save_area.type = &basic_type__unsigned_long;

    /* Integer or SSE parameters are read from registers, if there are enough of
     * them left. Otherwise read from overflow area. */
    if (*pc != PC_MEMORY) {
        struct var slice = res;
        int i,
            size = w,
            num_gp = 0, /* Number of general purpose registers needed. */
            num_fp = 0; /* Number of floating point registers needed. */

        for (i = 0; i < N_EIGHTBYTES(res.type); ++i) {
            if (pc[i] == PC_INTEGER) {
                num_gp++;
            } else {
                assert(pc[i] == PC_SSE);
                num_fp++;
            }
        }

        /* Keep reg_save_area in register for the remainder, a pointer to stack
         * where registers are stored. This value does not change. */
        load(var_reg_save_area, SI);

        /* Check whether there are enough registers left for the argument to be
         * passed in. */
        if (num_gp) {
            load(var_gp_offset, CX);
            emit(INSTR_CMP, OPT_IMM_REG,
                constant(6*8 - 8*num_gp, 4), reg(CX, 4));
            emit(INSTR_JA, OPT_IMM, addr(memory));
        }
        if (num_fp) {
            assert(0); /* No actual float support yet. */
            load(var_fp_offset, DX);
            emit(INSTR_CMP, OPT_IMM_REG,
                constant(6*8 - 8*num_fp, 4), reg(DX, 4));
            emit(INSTR_JA, OPT_IMM, addr(memory));
        }

        /* Load argument, one eightbyte at a time. This code has a lot in common
         * with enter, ret etc, potential for refactoring probably. */
        for (i = 0; i < N_EIGHTBYTES(res.type); ++i) {
            int width = (size < 8) ? size : 8;

            size -= width;
            assert(pc[i] == PC_INTEGER);
            assert(width == 1 || width == 2 || width == 4 || width == 8);

            slice.type = BASIC_TYPE_UNSIGNED(width);
            slice.offset = res.offset + i * 8;

            /* Advanced addressing, loading (%rsi + 8*i + (%rcx * 1)) into %rax.
             * Base of registers are stored in %rsi, first pending register is
             * at offset %rcx, and i counts number of registers done. In GNU
             * assembly it is {i*8}(%rsi, %rcx, 1). */
            emit(INSTR_MOV, OPT_MEM_REG,
                location(address(i*8, SI, CX, 1), width), reg(AX, width));
            store(AX, slice);
        }

        /* Store updated offsets to va_list. */
        if (num_gp) {
            assert(var_gp_offset.kind == DIRECT);
            emit(INSTR_ADD, OPT_IMM_MEM,
                constant(8 * num_gp, 4), location_of(var_gp_offset, 4));
        }
        if (num_fp) {
            assert(0);
            assert(var_fp_offset.kind == DIRECT);
            emit(INSTR_ADD, OPT_IMM_MEM,
                constant(16 * num_fp, 4), location_of(var_fp_offset, 4));
        }

        emit(INSTR_JMP, OPT_IMM, addr(done));
        enter_context(memory);
    }

    /* Parameters that are passed on stack will be read from overflow_arg_area.
     * This is also the fallback when arguments do not fit in remaining
     * registers. */
    load(var_overflow_arg_area, SI); /* Align overflow area before load? */
    if (w <= 8) {
        assert(res.kind == DIRECT);
        emit(INSTR_MOV, OPT_MEM_REG,
            location(address(0, SI, 0, 0), w), reg(AX, w));
        emit(INSTR_MOV, OPT_REG_MEM, reg(AX, w), location_of(res, w));
    } else {
        load_address(res, DI);
        emit(INSTR_MOV, OPT_IMM_REG, constant(w, 8), reg(DX, 8));
        emit(INSTR_CALL, OPT_IMM, addr(&decl_memcpy));
    }

    /* Move overflow_arg_area pointer to position of next memory argument, 
     * aligning to 8 byte. */
    emit(INSTR_ADD, OPT_IMM_MEM,
        constant(N_EIGHTBYTES(res.type) * 8, 8),
        location_of(var_overflow_arg_area, 8));

    if (*pc != PC_MEMORY)
        enter_context(done);
}

static void compile_op(const struct op *op)
{
    static int n_args, w;
    static struct var *args;

    switch (op->type) {
    case IR_ASSIGN:
        /* Handle special case of char [] = string literal. This will only occur
         * as part of initializer, at block scope. External definitions are
         * handled before this. At no other point should array types be seen in
         * assembly backend. We handle these assignments with memcpy, other
         * compilers load the string into register as ascii numbers. */
        if (is_array(op->a.type) || is_array(op->b.type)) {
            int size = size_of(op->a.type);

            assert(op->a.kind == DIRECT);
            assert(is_string(op->b));
            assert(type_equal(op->a.type, op->b.type));

            load_address(op->a, DI);
            emit(INSTR_MOV, OPT_IMM_REG, addr(op->b.symbol), reg(SI, 8));
            emit(INSTR_MOV, OPT_IMM_REG, constant(size, 8), reg(DX, 8));
            emit(INSTR_CALL, OPT_IMM, addr(&decl_memcpy));
            break;
        }
        /* Struct or union assignment, values that cannot be loaded into a
         * single register. */
        else if (size_of(op->a.type) > 8) {
            int size = size_of(op->a.type);
            assert(size_of(op->a.type) == size_of(op->b.type));

            load_address(op->a, DI);
            load_address(op->b, SI);

            emit(INSTR_MOV, OPT_IMM_REG, constant(size, 8), reg(DX, 8));
            emit(INSTR_CALL, OPT_IMM, addr(&decl_memcpy));
            break;
        }
        /* Fallthrough, assignment has implicit cast for convenience and to make
         * static initialization work without explicit casts. */
    case IR_CAST:
        w = (size_of(op->a.type) > size_of(op->b.type)) ?
            size_of(op->a.type) : size_of(op->b.type);
        w = (w < 4) ? 4 : w;
        assert(w == 4 || w == 8);
        load_value(op->b, AX, w);
        store(AX, op->a);
        break;
    case IR_DEREF:
        load(op->b, CX);
        emit(INSTR_MOV, OPT_MEM_REG,
            location(address(0, CX, 0, 0), size_of(op->a.type)),
            reg(AX, size_of(op->a.type)));
        store(AX, op->a);
        break;
    case IR_PARAM:
        args = realloc(args, ++n_args * sizeof(*args));
        args[n_args - 1] = op->a;
        break;
    case IR_CALL:
        call(n_args, args, op->a, op->b);
        if (args) {
            free(args);
            args = NULL;
            n_args = 0;
        }
        break;
    case IR_ADDR:
        load_address(op->b, AX);
        store(AX, op->a);
        break;
    case IR_NOT:
        load(op->b, AX);
        emit(INSTR_NOT, OPT_REG, reg(AX, size_of(op->a.type)));
        store(AX, op->a);
        break;
    case IR_OP_ADD:
        load(op->b, AX);
        load(op->c, CX);
        emit(INSTR_ADD, OPT_REG_REG,
            reg(CX, size_of(op->a.type)), reg(AX, size_of(op->a.type)));
        store(AX, op->a);
        break;
    case IR_OP_SUB:
        load(op->b, AX);
        load(op->c, CX);
        emit(INSTR_SUB, OPT_REG_REG,
            reg(CX, size_of(op->a.type)), reg(AX, size_of(op->a.type)));
        store(AX, op->a);
        break;
    case IR_OP_MUL:
        load(op->c, AX);
        if (op->b.kind == DIRECT) {
            emit(INSTR_MUL, OPT_MEM, location_of(op->b, size_of(op->b.type)));
        } else {
            load(op->b, CX);
            emit(INSTR_MUL, OPT_REG, reg(CX, size_of(op->b.type)));
        }
        store(AX, op->a);
        break;
    case IR_OP_DIV:
    case IR_OP_MOD:
        /* %rdx must be zero to avoid SIGFPE. */
        emit(INSTR_XOR, OPT_REG_REG, reg(DX, 8), reg(DX, 8));
        load(op->b, AX);
        if (op->c.kind == DIRECT) {
            emit(INSTR_DIV, OPT_MEM, location_of(op->c, size_of(op->c.type)));
        } else {
            load(op->c, CX);
            emit(INSTR_DIV, OPT_REG, reg(CX, size_of(op->c.type)));
        }
        store((op->type == IR_OP_DIV) ? AX : DX, op->a);
        break;
    case IR_OP_AND:
        load(op->b, AX);
        load(op->c, CX);
        emit(INSTR_AND, OPT_REG_REG, reg(CX, 8), reg(AX, 8));
        store(AX, op->a);
        break;
    case IR_OP_OR:
        load(op->b, AX);
        load(op->c, CX);
        emit(INSTR_OR, OPT_REG_REG, reg(CX, 8), reg(AX, 8));
        store(AX, op->a);
        break;
    case IR_OP_XOR:
        load(op->b, AX);
        load(op->c, CX);
        emit(INSTR_XOR, OPT_REG_REG, reg(CX, 8), reg(AX, 8));
        store(AX, op->a);
        break;
    case IR_OP_SHL:
        /* Shift amount must for some reason be in CX register, and appear as
         * %cl in instruction argument. Behavior is undefined if shift is
         * greater than integer width, so don't care about overflow or sign. */
        load(op->b, AX);
        load(op->c, CX);
        emit((is_unsigned(op->a.type)) ? INSTR_SHL : INSTR_SAL, OPT_REG_REG,
            reg(CX, 1), reg(AX, size_of(op->a.type)));
        store(AX, op->a);
        break;
    case IR_OP_SHR:
        load(op->b, AX);
        load(op->c, CX);
        emit((is_unsigned(op->a.type)) ? INSTR_SHR : INSTR_SAR, OPT_REG_REG,
            reg(CX, 1), reg(AX, size_of(op->a.type)));
        store(AX, op->a);
        break;
    case IR_OP_EQ:
        assert(size_of(op->a.type) == 4);
        load(op->b, AX);
        load(op->c, CX);
        emit(INSTR_CMP, OPT_REG_REG,
            reg(CX, size_of(op->a.type)), reg(AX, size_of(op->a.type)));
        emit(INSTR_SETZ, OPT_REG, reg(AX, 1));
        emit(INSTR_MOVZX, OPT_REG_REG, reg(AX, 1), reg(AX, 4));
        store(AX, op->a);
        break;
    case IR_OP_GE:
        assert(size_of(op->a.type) == 4);
        load(op->b, AX);
        load(op->c, CX);
        emit(INSTR_CMP, OPT_REG_REG,
            reg(CX, size_of(op->a.type)), reg(AX, size_of(op->a.type)));
        if (is_unsigned(op->b.type)) {
            assert(is_unsigned(op->c.type));
            emit(INSTR_SETAE, OPT_REG, reg(AX, 1));
        } else {
            emit(INSTR_SETGE, OPT_REG, reg(AX, 1));
        }
        emit(INSTR_MOVZX, OPT_REG_REG, reg(AX, 1), reg(AX, 4));
        store(AX, op->a);
        break;
    case IR_OP_GT:
        assert(size_of(op->a.type) == 4);
        load(op->b, AX);
        load(op->c, CX);
        emit(INSTR_CMP, OPT_REG_REG,
            reg(CX, size_of(op->a.type)), reg(AX, size_of(op->a.type)));
        if (is_unsigned(op->b.type)) {
            assert(is_unsigned(op->c.type));
            /* When comparison is unsigned, set flag without considering
             * overflow; CF=0 && ZF=0. */ 
            emit(INSTR_SETA, OPT_REG, reg(AX, 1));
        } else {
            emit(INSTR_SETG, OPT_REG, reg(AX, 1));
        }
        emit(INSTR_MOVZX, OPT_REG_REG, reg(AX, 1), reg(AX, 4));
        store(AX, op->a);
        break;
    case IR_VA_START:
        compile__builtin_va_start(op->a);
        break;
    case IR_VA_ARG:
        compile__builtin_va_arg(op->a, op->b);
        break;
    default:
        assert(0);
        break;
    }
}

static void tail_cmp_jump(struct block *block, const enum param_class *res)
{
    struct instruction instr = {0};
    struct op *cmp = block->code + block->n - 1;

    /* Target of assignment should be temporary, thus we do not lose any side
     * effects from not storing the value to stack. */
    assert(!cmp->a.lvalue);

    load(cmp->c, CX);
    load(cmp->b, AX);
    emit(INSTR_CMP, OPT_REG_REG,
        reg(CX, size_of(cmp->a.type)), reg(AX, size_of(cmp->a.type)));

    switch (cmp->type) {
    case IR_OP_EQ:
        instr.opcode = INSTR_JE;
        break;
    case IR_OP_GE:
        instr.opcode = (is_unsigned(cmp->b.type)) ? INSTR_JAE : INSTR_JGE;
        break;
    default:
        assert(cmp->type == IR_OP_GT);
        instr.opcode = (is_unsigned(cmp->b.type)) ? INSTR_JA : INSTR_JG;
        break;
    }

    instr.optype = OPT_IMM;
    instr.source.imm = addr(block->jump[1]->label);
    emit_instruction(instr);

    if (block->jump[0]->color == BLACK)
        emit(INSTR_JMP, OPT_IMM, addr(block->jump[0]->label));
    else
        compile_block(block->jump[0], res);

    compile_block(block->jump[1], res);
}

static void tail_generic(struct block *block, const enum param_class *res)
{
    if (!block->jump[0] && !block->jump[1]) {
        if (*res != PC_NO_CLASS && block->has_return_value) {
            assert(block->expr.type && !is_void(block->expr.type));
            ret(block->expr, res);
        }

        emit(INSTR_LEAVE, OPT_NONE);
        emit(INSTR_RET, OPT_NONE);
    } else if (!block->jump[1]) {
        if (block->jump[0]->color == BLACK)
            emit(INSTR_JMP, OPT_IMM, addr(block->jump[0]->label));
        else
            compile_block(block->jump[0], res);
    } else {
        load(block->expr, AX);
        emit(INSTR_CMP, OPT_IMM_REG, constant(0, 4), reg(AX, 4));
        emit(INSTR_JE, OPT_IMM, addr(block->jump[0]->label));
        if (block->jump[1]->color == BLACK)
            emit(INSTR_JMP, OPT_IMM, addr(block->jump[1]->label));
        else
            compile_block(block->jump[1], res);
        compile_block(block->jump[0], res);
    }
}

static void compile_block(struct block *block, const enum param_class *res)
{
    int i;

    if (block->color == BLACK)
        return;

    block->color = BLACK;
    enter_context(block->label);
    for (i = 0; i < block->n - 1; ++i)
        compile_op(block->code + i);

    /* Special case on comparison + jump, saving some space by not writing
     * the result of comparison (always a temporary). */
    if (block->n && IS_COMPARISON(block->code[i].type) && block->jump[1]) {
        assert(block->jump[0]);
        tail_cmp_jump(block, res);
    } else {
        if (block->n)
            compile_op(block->code + i);
        tail_generic(block, res);
    }
}

static void compile_data_assign(struct var target, struct var val)
{
    int size = target.type->size;
    struct immediate imm = {0};

    assert(target.kind == DIRECT);
    assert(val.kind == IMMEDIATE);

    switch (target.type->type) {
    case T_SIGNED:
        if (size == 1) {
            imm.type = IMM_BYTE;
            imm.d.byte = val.imm.i;
        } else if (size == 2) {
            imm.type = IMM_WORD;
            imm.d.word = val.imm.i;
        } else if (size == 4) {
            imm.type = IMM_DWORD;
            imm.d.dword = val.imm.i;
        } else {
            imm.type = IMM_QUAD;
            imm.d.quad = val.imm.i;
        }
        break;
    case T_UNSIGNED:
        if (size == 1) {
            imm.type = IMM_BYTE;
            imm.d.byte = val.imm.u;
        } else if (size == 2) {
            imm.type = IMM_WORD;
            imm.d.word = val.imm.u;
        } else if (size == 4) {
            imm.type = IMM_DWORD;
            imm.d.dword = val.imm.u;
        } else {
            imm.type = IMM_QUAD;
            imm.d.quad = val.imm.u;
        }
        break;
    case T_POINTER:
        if (is_string(val)) {
            imm.type = IMM_ADDR;
            imm.d.addr.sym = val.symbol;
            imm.d.addr.disp = val.offset;
        } else {
            imm.type = IMM_QUAD;
            imm.d.quad = val.imm.u;
        }
        break;
    case T_ARRAY:
        if (is_string(val)) {
            imm.type = IMM_STRING;
            imm.d.string = val.symbol->string_value;
            break;
        }
    default:
        assert(0);
        break;
    }

    emit_data(imm);
}

static void zero_fill_data(size_t bytes)
{
    struct immediate
        zero_byte = {IMM_BYTE},
        zero_quad = {IMM_QUAD};

    while (bytes > size_of(&basic_type__long)) {
        emit_data(zero_quad);
        bytes -= size_of(&basic_type__long);
    }

    while (bytes--)
        emit_data(zero_byte);
}

static void compile_data(const struct block *head)
{
    const struct typetree *type;
    int i, initialized;
    struct op *op;

    /* Point to the current type being initialized. */
    type = NULL;

    for (i = 0; i < head->n; ++i) {
        op = head->code + i;

        assert(op->type == IR_ASSIGN);
        assert(op->a.kind == DIRECT);

        /* Require that assignments come in sequentially per symbol, and for
         * each symbol sorted on increasing offsets. */
        if (!op->a.offset) {
            if (type) {
                assert(size_of(type) >= initialized);
                zero_fill_data(size_of(type) - initialized);
            }
            type = &op->a.symbol->type;
            enter_context(op->a.symbol);
            initialized = 0;
        }

        /* Insert necessary padding bytes before emitting initializer, which
         * can contain sparse assignments. */
        assert(op->a.offset >= initialized);
        zero_fill_data(op->a.offset - initialized);

        compile_data_assign(op->a, op->b);
        initialized = op->a.offset + size_of(op->a.type);
    }

    if (type) {
        assert(size_of(type) >= initialized);
        zero_fill_data(size_of(type) - initialized);
    }
}

static void compile_function(struct cfg *cfg)
{
    enum param_class *result_class;

    assert(is_function(&cfg->fun->type));
    enter_context(cfg->fun);
    emit(INSTR_PUSH, OPT_REG, reg(BP, 8));
    emit(INSTR_MOV, OPT_REG_REG, reg(SP, 8), reg(BP, 8));

    /* Make sure parameters and local variables are placed on stack. Keep
     * parameter class of return value for later assembling return. */
    result_class = enter(&cfg->fun->type, cfg->params, cfg->locals);

    /* Recursively assemble body. */
    compile_block(cfg->body, result_class);

    free(result_class);
}

void set_compile_target(FILE *stream, enum compile_target target)
{
    compile_target = target;
    output_stream = stream;
    switch (target) {
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

int compile_cfg(struct cfg *cfg)
{
    int i;

    /* Reset coloring before traversal. */
    for (i = 0; i < cfg->size; ++i)
        cfg->nodes[i]->color = WHITE;

    switch (compile_target) {
    case TARGET_IR_DOT:
        fdotgen(output_stream, cfg);
    case TARGET_NONE:
        break;
    case TARGET_x86_64_ASM:
    case TARGET_x86_64_ELF:
        compile_data(cfg->head);
        if (cfg->fun)
            compile_function(cfg);
        break;
    }

    return 0;
}

int compile_symbols(struct symbol_list list)
{
    int i;

    switch (compile_target) {
    case TARGET_x86_64_ASM:
    case TARGET_x86_64_ELF:
        for (i = 0; i < list.length; ++i)
            enter_context(list.symbol[i]);
    default:
        break;
    }

    free(list.symbol);
    return 0;
}

void flush(void)
{
    if (flush_backend)
        flush_backend();
}
