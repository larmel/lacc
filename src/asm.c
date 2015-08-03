#if _XOPEN_SOURCE < 500
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 500 /* snprintf */
#endif
#include "abi.h"
#include "ir.h"
#include "type.h"
#include "string.h"
#include "symbol.h"
#include "error.h"
#include "util/map.h"

#include <assert.h>
#include <stdio.h>
#include <ctype.h>

static char asmsuffix(const struct typetree *type)
{
    if (type->type == ARRAY) return 'q';
    if (type->size == 1) return 'b';
    if (type->size == 2) return 'w';
    if (type->size == 4) return 'l';
    return 'q';
}

static const char* reg(enum reg r, int w)
{
    static const char *x86_64_regs[][5] = {
        { "al", "ax", "eax", NULL, "rax" },
        { "bl", "bx", "ebx", NULL, "rbx" },
        { "cl", "cx", "ecx", NULL, "rcx" },
        { "dl", "dx", "edx", NULL, "rdx" },
        { NULL, NULL,  NULL, NULL, "rbp" },
        { NULL, NULL,  NULL, NULL, "rsp" },
        { NULL, "si", "esi", NULL, "rsi" },
        { NULL, "di", "edi", NULL, "rdi" },
        { "r8b",  "r8w",  "r8d",  NULL, "r8" },
        { "r9b",  "r9w",  "r9d",  NULL, "r9" },
        { "r10b", "r10w", "r10d", NULL, "r10" },
        { "r11b", "r11w", "r11d", NULL, "r11" },
        { "r12b", "r12w", "r12d", NULL, "r12" },
        { "r13b", "r13w", "r13d", NULL, "r13" },
        { "r14b", "r14w", "r14d", NULL, "r14" },
        { "r15b", "r15w", "r15d", NULL, "r15" },
    };

    return x86_64_regs[(int) r][w / 2];
}

/* Create a string representation of the given value, for example -16(%rbp), 
 * str(%rip), $.LC1+3, or $2. Format depending on type of variable.
 */
static char *refer(const struct var var)
{
    static char str[256];
    if (var.kind == IMMEDIATE) {
        assert(var.type->type != ARRAY);
        if (is_pointer(var.type) && var.string) {
            if (var.offset) {
                sprintf(str, "$%s%s%d", var.string,
                    (var.offset > 0) ? "+" : "", var.offset);
            } else {
                sprintf(str, "$%s", var.string);
            }
        } else {
            assert(!var.offset);
            assert(is_scalar(var.type));
            sprintf(str, "$%ld", var.value.i8);
        }
    } else {
        assert(var.kind == DIRECT);
        if (var.symbol->linkage != LINK_NONE) {
            if (var.offset) {
                sprintf(str, "%s%s%d(%%rip)",
                    sym_name(var.symbol),
                    (var.offset > 0) ? "+" : "",
                    var.offset);
            } else {
                sprintf(str, "%s(%%rip)", sym_name(var.symbol));
            }
        } else {
            sprintf(str, "%d(%%rbp)", var.symbol->stack_offset + var.offset);
        }
    }
    return str;
}

static void load_address(FILE *s, struct var v, enum reg r)
{
    switch (v.kind) {
    case IMMEDIATE:
        assert(0);
        break;
    case DIRECT:
        fprintf(s, "\t%s\t%s, %%%s\t# load &%s\n",
            ((v.symbol->linkage != LINK_NONE && !v.offset &&
                (v.type->type == ARRAY || v.type->type == FUNCTION)) ?
                "movq" : "leaq"),
            refer(v), reg(r, 8), v.symbol->name);
        break;
    case DEREF:
        /* Address of dereferenced variable is removed by evaluation. Exception
         * is loading plain array or function values, which decay into loading
         * their address. */
        assert(v.symbol->stack_offset);
        assert(is_pointer(v.symbol->type));
        fprintf(s, "\tmovq\t%d(%%rbp), %%r10\n", v.symbol->stack_offset);
        fprintf(s, "\tleaq\t%d(%%r10), %%%s\t# load (%s + %d)\n",
            v.offset, reg(r, 8), v.symbol->name, v.offset);
        break;
    }
}

/* Load variable v to register r, sign extended to fit register size. Width must
 * be either 4 (as in %eax) or 8 (as in %rax).
 */
static void load_value(FILE *s, struct var v, enum reg r, unsigned int w)
{
    const char *mov;

    /* We operate only with 32 or 64 bit register values, but variables can be
     * stored with byte or short width. Promote to 32 bit if required. */
    assert(w == 4 || w == 8);
    assert(v.type->size <= w);

    mov =
        (v.type->size == 1 && is_unsigned(v.type) && w == 4) ? "movzbl" :
        (v.type->size == 1 && is_unsigned(v.type) && w == 8) ? "movzbq" :
        (v.type->size == 1 && w == 4) ? "movsbl" :
        (v.type->size == 1 && w == 8) ? "movsbq" :
        (v.type->size == 2 && is_unsigned(v.type) && w == 4) ? "movzwl" :
        (v.type->size == 2 && is_unsigned(v.type) && w == 8) ? "movzwq" :
        (v.type->size == 2 && w == 4) ? "movswl" :
        (v.type->size == 2 && w == 8) ? "movswq" :
        (v.type->size == 4 && is_unsigned(v.type) && w == 8) ? "movl" :
        (v.type->size == 4 && w == 8) ? "movslq" :
        (v.type->size == w && w == 4) ? "movl" :
        (v.type->size == w && w == 8) ? "movq" :
        NULL;

    assert(mov);

    switch (v.kind) {
    case DIRECT:
        fprintf(s, "\t%s\t%s, %%%s\t# load %s\n",
            mov, refer(v), reg(r, w), v.symbol->name);
        break;
    case DEREF:
        assert(is_pointer(v.symbol->type));
        load_value(s, var_direct(v.symbol), R11, v.symbol->type->size);
        if (v.offset) {
            fprintf(s, "\t%s\t%d(%%%s), %%%s\t# load *(%s + %d)\n",
                mov, v.offset, reg(R11, 8), reg(r, w),
                v.symbol->name, v.offset);
        } else {
            fprintf(s, "\t%s\t(%%%s), %%%s\t# load *%s\n",
                mov, reg(R11, 8), reg(r, w), v.symbol->name);
        }
        break;
    case IMMEDIATE:
        fprintf(s, "\tmov%c\t%s, %%%s\n",
            (w == 4) ? 'l' : 'q', refer(v), reg(r, w));
        break;
    }
}

/* Load variable to register, automatically sign/width extended to either 32 or
 * 64 bit.
 */
static void load(FILE *s, struct var v, enum reg r)
{
    unsigned int w = (v.type->size < 4) ? 4 : v.type->size;
    assert(w == 4 || w == 8);
    load_value(s, v, r, w);
}

static void store(FILE *s, enum reg r, struct var v)
{
    assert(is_scalar(v.type) || v.type->size <= 8);

    if (v.kind == DIRECT) {
        assert(v.type->type != ARRAY);
        fprintf(s, "\tmov%c\t%%%s, %s\t# store %s\n",
            asmsuffix(v.type), reg(r, v.type->size), refer(v), v.symbol->name);
    } else {
        assert(v.kind == DEREF);
        assert(is_pointer(v.symbol->type));
        load_value(s, var_direct(v.symbol), R11, v.symbol->type->size);
        if (v.offset) {
            fprintf(s, "\tmov%c\t%%%s, %d(%%%s)\t# store *(%s + %d)\n",
                asmsuffix(v.type), reg(r, v.type->size), v.offset, reg(R11, 8),
                v.symbol->name, v.offset);
        } else {
            fprintf(s, "\tmov%c\t%%%s, (%%%s)\t# store *%s\n",
                asmsuffix(v.type), reg(r, v.type->size), reg(R11, 8),
                v.symbol->name);
        }
    }
}

/* Push value to stack, rounded up to always be 8 byte aligned.
 */
static void push(FILE *s, struct var v)
{
    if (is_scalar(v.type)) {
        if (v.kind == IMMEDIATE && v.type->size == 8) {
            fprintf(s, "\tpushq\t%s\n", refer(v));
        } else {
            load(s, v, AX);
            fprintf(s, "\tpushq\t%%%s\n", reg(AX, 8));
        }
    } else {
        int slices = N_EIGHTBYTES(v.type);

        fprintf(s, "\tsubq\t$%d, %%rsp\n", slices * 8);
        fprintf(s, "\tmovl\t$%d, %%%s\n", slices, reg(CX, 4));
        fprintf(s, "\tmovq\t%%rsp, %%rdi\n");
        load_address(s, v, SI);
        fprintf(s, "\trep movsq\n");
    }
}

/* Assemble a function call. For now this assumes only integer arguments passed
 * in %rdi, %rsi, %rdx, %rcx, %r8 and %r9, and arguments passed on stack.
 */
static void call(
    FILE *s,
    int n,
    const struct var *args,
    struct var res,
    struct var func)
{
    int i,
        mem_used = 0,
        next_integer_reg = 0;

    enum param_class *resc;
    enum param_class **argc;

    const struct typetree **arg_types;

    /* Classify function arguments and return value. */
    arg_types = calloc(n, sizeof(*arg_types));
    for (i = 0; i < n; ++i) {
        arg_types[i] = args[i].type;
    }

    argc = classify_call(arg_types, func.type->next, n, &resc);
    free(arg_types);

    /* Pass arguments on stack from right to left. Do this before populating
     * registers, because %rdi, %rsi etc will be used to do the pushing. */
    for (i = n - 1; i >= 0; --i) {
        if (*argc[i] == PC_MEMORY) {
            mem_used += N_EIGHTBYTES(args[i].type) * 8;
            push(s, args[i]);
        }
    }

    /* When return value is MEMORY, pass a pointer to stack as hidden first
     * argument. */
    if (*resc == PC_MEMORY) {
        next_integer_reg = 1;
        load_address(s, res, param_int_reg[0]);
    }

    /* Pass arguments in registers from left to right. Partition arguments into
     * eightbyte slices and load into appropriate registers. */
    for (i = 0; i < n; ++i) {
        enum param_class *eightbyte = argc[i];

        /*dump_classification(eightbyte, args[i].type);*/

        if (*eightbyte != PC_MEMORY) {
            if (args[i].type->type == OBJECT) {
                int chunks = N_EIGHTBYTES(args[i].type),
                    size = args[i].type->size,
                    j;
                struct var slice = args[i];

                for (j = 0; j < chunks; ++j) {
                    int w = (size < 8) ? size % 8 : 8;

                    size -= w;
                    assert(eightbyte[j] == PC_INTEGER);
                    assert(w == 1 || w == 2 || w == 4 || w == 8);

                    slice.type = type_init_integer(w);
                    slice.offset = args[i].offset + j * 8;
                    load(s, slice, param_int_reg[next_integer_reg++]);
                }
                assert(size == 0);
            } else {
                /* Non-objects can be loaded normally. */
                load(s, args[i], param_int_reg[next_integer_reg++]);
            }
        }
    }

    /* For variable argument lists, %al contains the number of vector registers
     * used. */
    if (is_vararg(func.type)) {
        fprintf(s, "\tmovl\t$0, %%eax\n");
    }

    assert(func.kind == DIRECT);
    fprintf(s, "\tcall\t%s\n", func.symbol->name);
    if (mem_used) {
        fprintf(s, "\taddq\t$%d, %%rsp\n", mem_used);
    }

    /* Move return value from register(s) to memory. Return values with class
     * MEMORY have already been written by callee. */
    if (*resc != PC_MEMORY) {
        int n = N_EIGHTBYTES(res.type);
        int size = res.type->size;
        struct var slice = res;

        /* Only have INTEGER class for now. */
        assert( n <= 2 );

        next_integer_reg = 0;
        for (i = 0; i < n; ++i) {
            int width = (size < 8) ? size % 8 : 8;

            size -= width;
            assert( resc[i] == PC_INTEGER );

            slice.type = type_init_integer(width);
            slice.offset = i * 8;
            store(s, ret_int_reg[next_integer_reg++], slice);
        }

        assert( size == 0 );
    }
}

/* Assign storage to local variables.
 */
static int assign_locals_storage(const struct decl *fun, int offset)
{
    int i;

    for (i = 0; i < fun->locals.length; ++i) {
        struct symbol *sym = fun->locals.elem[i];
        assert(!sym->stack_offset);

        if (sym->linkage == LINK_NONE) {
            offset -= sym->type->size;
            sym->stack_offset = offset;
        }
    }

    return offset;
}

/* Values from va_list initialization.
 */
static int gp_offset;
static int fp_offset;
static int overflow_arg_area_offset;
static int reg_save_area_offset;

/* Load parameters into call frame on entering a function. Return parameter
 * class of return value.
 */
static enum param_class *enter(FILE *s, const struct decl *func)
{
    int i,
        next_integer_reg = 0,
        mem_offset = 16,    /* Offset of PC_MEMORY parameters. */
        stack_offset = 0;   /* Offset of %rsp to keep local variables. */

    enum param_class
        **params,
        *ret;

    /* Get classification of function arguments and return value. */
    params = classify_signature(func->fun->type, &ret);

    /* Address of return value is passed as first integer argument. If return
     * value is MEMORY, store the address at stack offset -8. */
    if (*ret == PC_MEMORY) {
        stack_offset = -8;
        next_integer_reg = 1;
    }

    /* For functions with variable argument list, reserve a fixed area at the
     * beginning of the stack fram for register values. In total there are 8
     * bytes for each of the 6 integer registers, and 16 bytes for each of the 8
     * SSE registers, for a total of 176 bytes. We want to keep the register
     * save area fixed regardless of parameter class of return value, so skip
     * the first 8 bytes used for return value address. */
    if (is_vararg(func->fun->type)) {
        stack_offset = -176 - 8;
    }

    /* Assign storage to parameters. */
    for (i = 0; i < func->params.length; ++i) {
        struct symbol *sym = func->params.elem[i];

        assert( !sym->stack_offset );
        assert( sym->linkage == LINK_NONE );

        /* Guarantee that parameters are 8-byte aligned also for those passed by
         * register, which makes it easier to store in local frame after
         * entering function. Might want to revisit this and make it compact. */
        if (*params[i] == PC_MEMORY) {
            sym->stack_offset = mem_offset;
            mem_offset += N_EIGHTBYTES(sym->type) * 8;
        } else {
            stack_offset -= N_EIGHTBYTES(sym->type) * 8;
            sym->stack_offset = stack_offset;
        }
    }

    /* Assign storage to locals. */
    stack_offset = assign_locals_storage(func, stack_offset);
    if (stack_offset < 0) {
        fprintf(s, "\tsubq\t$%d, %%rsp\n", -stack_offset);
    }

    /* Store return address to well known stack offset. */
    if (*ret == PC_MEMORY) {
        fprintf(s, "\tmovq\t%%%s, -8(%%rbp)\n", reg(param_int_reg[0], 8));
    }

    /* Store all potential parameters to register save area. This includes
     * parameters that are known to be passed as registers, that will anyway be
     * stored to another stack location. Maybe potential for optimization. */
    if (is_vararg(func->fun->type)) {
        extern const char *mklabel(void);
        const char *label = mklabel();

        /* It is desireable to skip touching floating point unit if possible, al
         * holds the number of floating point registers passed. */
        fprintf(s, "\ttestb\t%%al, %%al\n");
        fprintf(s, "\tjz %s\n", label);
        reg_save_area_offset = -8; /* Skip address of return value. */
        for (i = 0; i < 8; ++i) {
            reg_save_area_offset -= 16;
            fprintf(s, "\tmovaps\t%%xmm%d, %d(%%rbp)\n",
                (7 - i), reg_save_area_offset);
        }
        fprintf(s, "%s:\n", label);
        for (i = 0; i < 6; ++i) {
            reg_save_area_offset -= 8;
            fprintf(s, "\tmovq\t%%%s, %d(%%rbp)\n",
                reg(param_int_reg[5 - i], 8), reg_save_area_offset);
        }
    }

    /* Move arguments from register to stack. */
    for (i = 0; i < func->params.length; ++i) {
        enum param_class *eightbyte = params[i];

        /* Here it is ok to not separate between object and other types. Data in
         * registers can always be treated as integer type. */
        if (*eightbyte != PC_MEMORY) {
            int n = N_EIGHTBYTES(func->fun->type->member[i].type),
                size = func->fun->type->member[i].type->size,
                j;
            struct var ref = { NULL, NULL, DIRECT };

            ref.symbol = func->params.elem[i];
            for (j = 0; j < n; ++j) {
                int width = (size < 8) ? size : 8;
                ref.type = type_init_integer(width);
                ref.offset = j * 8;
                store(s, param_int_reg[next_integer_reg++], ref);
                size -= width;
            }
            assert(!size);
        }
    }

    /* After loading parameters we know how many registers have been used for
     * fixed parameters. Update offsets to be used in va_start. */
    if (is_vararg(func->fun->type)) {
        gp_offset = 8 * next_integer_reg;
        fp_offset = 0;
        overflow_arg_area_offset = mem_offset;
    }

    return ret;
}

/* Return value from function, placing it in register(s) or writing it to stack
 * based on parameter class.
 */
static void ret(FILE *s, struct var val, const enum param_class *pc)
{
    int next_int_reg = 0;

    assert( *pc != PC_NO_CLASS );

    /* NB: This might break down for non-object values, in particular string
     * constants that cannot be interpreted as integers. */
    if (*pc != PC_MEMORY) {
        int i;
        int n = N_EIGHTBYTES(val.type);
        int size = val.type->size;
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

            slice.type = type_init_integer(width);
            slice.offset = val.offset + i * 8;
            load(s, slice, ret_int_reg[next_int_reg++]);
        }

        assert(size == 0);
    } else {
        /* Load return address from magic stack offset and copy result. */
        fprintf(s, "\tmovq\t-8(%%rbp), %%%s\n", reg(DI, 8));
        load_address(s, val, SI);
        fprintf(s, "\tmovl\t$%d, %%%s\n", val.type->size, reg(DX, 4));
        fprintf(s, "\tcall\tmemcpy\n");

        /* The ABI specifies that the address should be in %rax on return. */
        fprintf(s, "\tmovq\t-8(%%rbp), %%%s\n", reg(AX, 8));
    }
}

/* Execute call to va_start, initializing the provided va_list object. Values
 * are taken from static context set during enter.
 */
static void assemble__builtin_va_start(FILE *stream, struct var args)
{
    assert(args.kind == DIRECT);
    fprintf(stream, "\tmovl\t$%d, %s\t# gp_offset\n", gp_offset, refer(args));
    args.offset += 4;
    fprintf(stream, "\tmovl\t$%d, %s\t# fp_offset\n", fp_offset, refer(args));
    args.offset += 4;
    fprintf(stream, "\tleaq\t%d(%%rbp), %%rax\n", overflow_arg_area_offset);
    fprintf(stream, "\tmovq\t%%rax, %s\t# overflow_arg_area\n", refer(args));
    args.offset += 8;
    fprintf(stream, "\tleaq\t%d(%%rbp), %%rax\n", reg_save_area_offset);
    fprintf(stream, "\tmovq\t%%rax, %s\t# reg_save_area\n", refer(args));
}

/* Output logic for fetching a parameter from calling va_arg(args, T).
 */
static void assemble__builtin_va_arg(FILE *s, struct var res, struct var args)
{
    extern const char *mklabel(void);

    enum param_class *pc = classify(res.type);
    struct var
        var_gp_offset = args,
        var_fp_offset = args,
        var_overflow_arg_area = args,
        var_reg_save_area = args;
    const char
        *memory = mklabel(),
        *done = mklabel();

    /* Might be too restrictive for res, but simplifies some codegen. */
    assert(res.kind == DIRECT);
    assert(args.kind == DIRECT);

    /* References into va_list object. */
    var_gp_offset.type = type_init_unsigned(4);
    var_fp_offset.offset += 4;
    var_fp_offset.type = type_init_unsigned(4);
    var_overflow_arg_area.offset += 8;
    var_overflow_arg_area.type = type_init_unsigned(8);
    var_reg_save_area.offset += 16;
    var_reg_save_area.type = type_init_unsigned(8);

    /* Integer or SSE parameters are read from registers, if there are enough of
     * them left. Otherwise read from overflow area. */
    if (*pc != PC_MEMORY) {
        struct var slice = res;
        int i,
            size = res.type->size,
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
        load(s, var_reg_save_area, SI);

        /* Check whether there are enough registers left for the argument to be
         * passed in. */
        if (num_gp) {
            load(s, var_gp_offset, CX);
            fprintf(s, "\tcmpl\t$%d, %%%s\n", (6*8 - 8*num_gp), reg(CX, 4));
            fprintf(s, "\tja %s\n", memory);
        }
        if (num_fp) {
            assert(0); /* No actual float support yet. */
            load(s, var_fp_offset, DX);
            fprintf(s, "\tcmpl\t$%d, %%%s\n", (8*8 - 8*num_fp), reg(DX, 4));
            fprintf(s, "\tja %s\n", memory);
        }

        /* Load argument, one eightbyte at a time. This code has a lot in common
         * with enter, ret etc, potential for refactoring probably. */
        for (i = 0; i < N_EIGHTBYTES(res.type); ++i) {
            int width = (size < 8) ? size : 8;

            size -= width;
            assert(pc[i] == PC_INTEGER);
            assert(width == 1 || width == 2 || width == 4 || width == 8);

            slice.type = type_init_unsigned(width);
            slice.offset = res.offset + i * 8;

            /* Advanced addressing, loading (%rsi + 8*i + (%rcx * 1)) into %rax.
             * Base of registers are stored in %rsi, first pending register is
             * at offset %rcx, and i counts number of registers done. */
            fprintf(s, "\tmov%c\t%d(%%%s, %%%s, 1), %%%s\n",
                asmsuffix(slice.type), (i * 8), reg(SI, 8), reg(CX, 8),
                reg(AX, width));
            store(s, AX, slice);
        }

        /* Store updated offsets to va_list. */
        if (num_gp) {
            fprintf(s, "\taddl\t$%d, %s\t# Update gp_offset\n",
                (8 * num_gp), refer(var_gp_offset));
        }
        if (num_fp) {
            assert(0);
            fprintf(s, "\taddl\t$%d, %s\t# Update fp_offset\n",
                (16 * num_fp), refer(var_fp_offset));
        }

        fprintf(s, "\tjmp %s\n", done);
        fprintf(s, "%s:\t# memory\n", memory);
    }

    /* Parameters that are passed on stack will be read from overflow_arg_area.
     * This is also the fallback when arguments do not fit in remaining
     * registers. */
    load(s, var_overflow_arg_area, SI); /* Align overflow area before load? */
    if (res.type->size <= 8) {
        assert(res.kind == DIRECT);
        fprintf(s, "\tmov%c\t(%%%s), %%%s\n",
            asmsuffix(res.type), reg(SI, 8), reg(AX, res.type->size));
        fprintf(s, "\tmov%c\t%%%s, %s\t# Load vararg\n",
            asmsuffix(res.type), reg(AX, res.type->size), refer(res));
    } else {
        load_address(s, res, DI);
        fprintf(s, "\tmovq\t$%d, %%rdx\n", res.type->size);
        fprintf(s, "\tcall\tmemcpy\t# Load vararg\n");
    }

    /* Move overflow_arg_area pointer to position of next memory argument, 
     * aligning to 8 byte. */
    fprintf(s, "\taddl\t$%d, %s\t# Update overflow_arg_area\n",
        N_EIGHTBYTES(res.type) * 8, refer(var_overflow_arg_area));

    if (*pc != PC_MEMORY) {
        fprintf(s, "%s:\t# done\n", done);
    }
}

static void asm_op(FILE *stream, const struct op *op)
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
        if (op->a.type->type == ARRAY || op->b.type->type == ARRAY) {
            struct var str = op->b;
            assert(type_equal(op->a.type, op->b.type));
            assert(op->a.kind == DIRECT);
            assert(op->b.kind == IMMEDIATE && op->b.string);

            /* The string value is an immediate, that has not yet gotten a label
             * that we can refer to. Add to string table, decay to address. */
            str.string = strlabel(str.string);
            str.type = type_init_pointer(str.type->next);
            fprintf(stream, "\tmovq\t%s, %%%s\n", refer(str), reg(SI, 8));

            load_address(stream, op->a, DI);
            fprintf(stream, "\tmovq\t$%d, %%rdx\n", op->a.type->size);
            fprintf(stream, "\tcall\tmemcpy\n");
            break;
        }
        /* Struct or union assignment, values that cannot be loaded into a
         * single register. */
        else if (op->a.type->size > 8) {
            assert(op->a.type->size == op->b.type->size);
            load_address(stream, op->a, DI);
            load_address(stream, op->b, SI);
            fprintf(stream, "\tmovq\t$%d, %%rdx\n", op->a.type->size);
            fprintf(stream, "\tcall\tmemcpy\n");
            break;
        }
        /* Fallthrough, assignment has implicit cast for convenience and to make
         * static initialization work without explicit casts. */
    case IR_CAST:
        w = (op->a.type->size > op->b.type->size) ?
            op->a.type->size : op->b.type->size;
        w = (w < 4) ? 4 : w;
        assert(w == 4 || w == 8);
        load_value(stream, op->b, AX, w);
        store(stream, AX, op->a);
        break;
    case IR_DEREF:
        load(stream, op->b, BX);
        fprintf(stream, "\tmov%c\t(%%rbx), %%%s\n",
            asmsuffix(op->a.type), reg(AX, op->a.type->size));
        store(stream, AX, op->a);
        break;
    case IR_PARAM:
        args = realloc(args, ++n_args * sizeof(*args));
        args[n_args - 1] = op->a;
        break;
    case IR_CALL:
        call(stream, n_args, args, op->a, op->b);
        if (args) {
            free(args);
            args = NULL;
            n_args = 0;
        }
        break;
    case IR_ADDR:
        load_address(stream, op->b, AX);
        store(stream, AX, op->a);
        break;
    case IR_OP_ADD:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\tadd%c\t%%%s, %%%s\n",
            asmsuffix(op->a.type), reg(BX, op->a.type->size),
            reg(AX, op->a.type->size));
        store(stream, AX, op->a);
        break;
    case IR_OP_SUB:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\tsub%c\t%%%s, %%%s\n",
            asmsuffix(op->a.type), reg(BX, op->a.type->size),
            reg(AX, op->a.type->size));
        store(stream, AX, op->a);
        break;
    case IR_OP_MUL:
        load(stream, op->c, AX);
        if (op->b.kind == DIRECT) {
            fprintf(stream, "\tmul%c\t%s\n",
                asmsuffix(op->b.type), refer(op->b));
        } else {
            load(stream, op->b, BX);
            fprintf(stream, "\tmul%c\t%%%s\n",
                asmsuffix(op->b.type), reg(BX, op->b.type->size));
        }
        store(stream, AX, op->a);
        break;
    case IR_OP_DIV:
    case IR_OP_MOD:
        /* %rdx must be zero to avoid SIGFPE. */
        fprintf(stream, "\txorq\t%%%s, %%%s\n", reg(DX, 8), reg(DX, 8));
        load(stream, op->b, AX);
        if (op->c.kind == DIRECT) {
            fprintf(stream, "\tdiv%c\t%s\n",
                asmsuffix(op->c.type), refer(op->c));
        } else {
            load(stream, op->c, BX);
            fprintf(stream, "\tdiv%c\t%%%s\n",
                asmsuffix(op->c.type), reg(BX, op->c.type->size));
        }
        if (op->type == IR_OP_DIV) {
            store(stream, AX, op->a);
        } else {
            store(stream, DX, op->a);
        }
        break;
    case IR_OP_AND:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\tand\t%%rbx, %%rax\n");
        store(stream, AX, op->a);
        break;
    case IR_OP_OR:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\tor\t%%rbx, %%rax\n");
        store(stream, AX, op->a);
        break;
    case IR_OP_XOR:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\txor\t%%rbx, %%rax\n");
        store(stream, AX, op->a);
        break;
    case IR_OP_SHL:
        /* Shift amount must for some reason be in CX register, and appear as
         * %cl in instruction argument. Behavior is undefined if shift is
         * greater than integer width, so don't care about overflow or sign. */
        load(stream, op->b, AX);
        load(stream, op->c, CX);
        if (is_unsigned(op->a.type)) {
            fprintf(stream, "\tshl%c\t%%%s, %%%s\n",
                asmsuffix(op->a.type),
                reg(CX, 1),
                reg(AX, op->a.type->size));
        } else {
            fprintf(stream, "\tsal%c\t%%%s, %%%s\n",
                asmsuffix(op->a.type),
                reg(CX, 1),
                reg(AX, op->a.type->size));
        }
        store(stream, AX, op->a);
        break;
    case IR_OP_SHR:
        load(stream, op->b, AX);
        load(stream, op->c, CX);
        if (is_unsigned(op->a.type)) {
            fprintf(stream, "\tshr%c\t%%%s, %%%s\n",
                asmsuffix(op->a.type),
                reg(CX, 1),
                reg(AX, op->a.type->size));
        } else {
            fprintf(stream, "\tsar%c\t%%%s, %%%s\n",
                asmsuffix(op->a.type),
                reg(CX, 1),
                reg(AX, op->a.type->size));
        }
        store(stream, AX, op->a);
        break;
    case IR_OP_EQ:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\tcmp\t%%%s, %%%s\n",
            reg(BX, op->a.type->size), reg(AX, op->a.type->size));
        fprintf(stream, "\tsetz\t%%al\n");
        fprintf(stream, "\tmovzbl\t%%al, %%eax\n");
        store(stream, AX, op->a);
        break;
    case IR_OP_GE:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\tcmp\t%%%s, %%%s\n",
            reg(BX, op->a.type->size), reg(AX, op->a.type->size));
        if (is_unsigned(op->b.type)) {
            assert(is_unsigned(op->c.type));
            fprintf(stream, "\tsetae\t%%al\n");
        } else {
            fprintf(stream, "\tsetge\t%%al\n");
        }
        fprintf(stream, "\tmovzbl\t%%al, %%eax\n");
        store(stream, AX, op->a);
        break;
    case IR_OP_GT:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\tcmp\t%%%s, %%%s\n",
            reg(BX, op->a.type->size), reg(AX, op->a.type->size));
        if (is_unsigned(op->b.type)) {
            assert( is_unsigned(op->c.type) );
            /* When comparison is unsigned, set flag without considering
             * overflow; CF=0 && ZF=0. */ 
            fprintf(stream, "\tseta\t%%al\n");
        } else {
            fprintf(stream, "\tsetg\t%%al\n");
        }
        fprintf(stream, "\tmovzbl\t%%al, %%eax\n");
        store(stream, AX, op->a);
        break;
    case IR_VA_START:
        fprintf(stream, "\t# va_start\n");
        assemble__builtin_va_start(stream, op->a);
        break;
    case IR_VA_ARG:
        fprintf(stream, "\t# va_arg (%s)\n", typetostr(op->a.type));
        assemble__builtin_va_arg(stream, op->a, op->b);
        break;
    default:
        assert(0);
        break;
    }
}

static void asm_block(
    FILE *stream,
    map_t *memo,
    const struct block *block,
    const enum param_class *res);

static void tail_cmp_jump(
    FILE *stream,
    map_t *memo,
    const struct block *block,
    const enum param_class *res)
{
    struct op *cmp = block->code + block->n - 1;

    /* Target of assignment should be temporary, thus we do not lose any side
     * effects from not storing the value to stack. */
    assert(!cmp->a.lvalue);

    load(stream, cmp->b, AX);
    load(stream, cmp->c, BX);
    fprintf(stream, "\tcmp\t%%%s, %%%s\n",
        reg(BX, cmp->a.type->size), reg(AX, cmp->a.type->size));
    switch (cmp->type) {
    case IR_OP_EQ:
        fprintf(stream, "\tje\t%s\n", block->jump[1]->label);
        break;
    case IR_OP_GE:
        fprintf(stream, "\t%s\t%s\n",
            (is_unsigned(cmp->b.type) ? "jae" : "jge"), block->jump[1]->label);
        break;
    case IR_OP_GT:
        fprintf(stream, "\t%s\t%s\n",
            (is_unsigned(cmp->b.type) ? "ja" : "jg"), block->jump[1]->label);
        break;
    default:
        assert(0);
    }

    if (map_lookup(memo, block->jump[0]->label)) {
        fprintf(stream, "\tjmp\t%s\n", block->jump[0]->label);
    }

    asm_block(stream, memo, block->jump[0], res);
    asm_block(stream, memo, block->jump[1], res);
}

static void tail_generic(
    FILE *stream,
    map_t *memo,
    const struct block *block,
    const enum param_class *res)
{
    if (!block->jump[0] && !block->jump[1]) {
        if (*res != PC_NO_CLASS && block->has_return_value) {
            assert(block->expr.type && block->expr.type->type != NONE);
            ret(stream, block->expr, res);
        }

        fprintf(stream, "\tleaveq\n");
        fprintf(stream, "\tretq\n");
    } else if (!block->jump[1]) {
        if (map_lookup(memo, block->jump[0]->label)) {
            fprintf(stream, "\tjmp\t%s\n", block->jump[0]->label);
        }

        asm_block(stream, memo, block->jump[0], res);
    } else {
        load(stream, block->expr, AX);
        fprintf(stream, "\tcmpq\t$0, %%rax\n");
        fprintf(stream, "\tje\t%s\n", block->jump[0]->label);
        if (map_lookup(memo, block->jump[1]->label)) {
            fprintf(stream, "\tjmp\t%s\n", block->jump[1]->label);
        }

        asm_block(stream, memo, block->jump[1], res);
        asm_block(stream, memo, block->jump[0], res);
    }
}

static void asm_block(
    FILE *stream,
    map_t *memo,
    const struct block *block,
    const enum param_class *res)
{
    int i;

    assert(block && res);

    if (!map_lookup(memo, block->label)) {
        map_insert(memo, block->label, (void*)"done");
        fprintf(stream, "%s:\n", block->label);
        for (i = 0; i < block->n - 1; ++i) {
            asm_op(stream, block->code + i);
        }

        /* Special case on comparison + jump, saving some space by not writing
         * the result of comparison (always a temporary). */
        if (
            block->n && IS_COMPARISON(block->code[i].type) &&
            block->jump[0] && block->jump[1])
        {
            tail_cmp_jump(stream, memo, block, res);
        } else {
            if (block->n) {
                asm_op(stream, block->code + i);
            }

            tail_generic(stream, memo, block, res);
        }
    }
}

static void asm_immediate(FILE *stream, struct var target, struct var val)
{
    assert(target.kind == DIRECT);
    assert(val.kind == IMMEDIATE);

    switch (target.type->type) {
    case INTEGER:
        switch (target.type->size) {
        case 1:
            fprintf(stream, "\t.byte\t%d\n", val.value.i1);
            break;
        case 2:
            fprintf(stream, "\t.short\t%d\n", val.value.i2);
            break;
        case 4:
            fprintf(stream, "\t.int\t%d\n", val.value.i4);
            break;
        default:
            assert(target.type->size == 8);
            fprintf(stream, "\t.quad\t%ld\n", val.value.i8);
            break;
        }
        break;
    case POINTER:
        fprintf(stream, "\t.quad\t");
        if (val.string) {
            fprintf(stream, "%s\n", refer(val) + 1); /* Skip the leading '$' */
        } else {
            fprintf(stream, "%lu\n", val.value.u8);
        }
        break;
    case ARRAY:
        if (val.string) {
            fprintf(stream, "\t.string\t\"");
            output_string(stream, val.string);
            fprintf(stream, "\"\n");
            break;
        }
    default:
        internal_error("Invalid assignment of type '%s'.",
            typetostr(target.type));
        exit(1);
    }
}

static void assemble_data(FILE *stream, struct block *head)
{
    int i;
    int initialized = 0;
    const struct symbol *symbol = NULL;

    fprintf(stream, "\t.data\n");
    for (i = 0; i < head->n; ++i) {
        struct op *op = head->code + i;
        symbol = op->a.symbol;

        assert(op->type == IR_ASSIGN);
        assert(op->a.kind == DIRECT);
        assert(symbol == op->a.symbol || !op->a.offset);

        /* Assume that assignments come in sequentially per symbol, and for each
         * symbol sorted on increasing offsets. */
        if (!op->a.offset) {
            symbol = op->a.symbol;
            initialized = 0;
            if (symbol->linkage == LINK_EXTERN) {
                fprintf(stream, "\t.globl\t%s\n", sym_name(symbol));
            }
            if (is_aggregate(symbol->type)) {
                fprintf(stream, "\t.align\t16\n");
            }
            fprintf(stream, "%s:\n", sym_name(symbol));
        }

        /* Insert necessary padding bytes before emitting initializer, which
         * does not handle offsets in any way. */
        if (op->a.offset > initialized) {
            fprintf(stream, "\t.zero\t%d\n",
                op->a.offset - initialized);
        }
        asm_immediate(stream, op->a, op->b);
        initialized = op->a.offset + op->a.type->size;
    }
}

static void asm_function(FILE *stream, const struct decl *decl)
{
    map_t memo;
    enum param_class *res;

    map_init(&memo);

    fprintf(stream, "\t.text\n");
    if (decl->fun->linkage == LINK_EXTERN) {
        fprintf(stream, "\t.globl\t%s\n", sym_name(decl->fun));
    }

    fprintf(stream, "\t.type\t%s, @function\n", sym_name(decl->fun));
    fprintf(stream, "%s:\n", sym_name(decl->fun));
    fprintf(stream, "\tpushq\t%%rbp\n");
    fprintf(stream, "\tmovq\t%%rsp, %%rbp\n");

    /* Make sure parameters and local variables are placed on stack. Keep
     * parameter class of return value for later assembling return. */
    res = enter(stream, decl);

    /* Recursively assemble body. */
    asm_block(stream, &memo, decl->body, res);

    /* This is required to see function names in valgrind. */
    fprintf(stream, "\t.size\t%s, .-%s\n",
        sym_name(decl->fun), sym_name(decl->fun));

    free(res);
    map_finalize(&memo);
}

void assemble(FILE *stream, const struct decl *decl)
{
    if (decl->head->n) {
        assemble_data(stream, decl->head);
    }
    if (decl->fun) {
        assert(decl->fun->type->type == FUNCTION);
        asm_function(stream, decl);
    }
}
