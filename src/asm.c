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

static const char *sym_name(const struct symbol *sym)
{
    if (sym->n) {
        static char name[128];
        snprintf(name, 127, "%s.%d", sym->name, sym->n);
        return name;
    }

    return sym->name;
}

static char *refer(const struct var var)
{
    static char str[256];
    assert( var.kind == IMMEDIATE || var.kind == DIRECT );

    if (var.kind == IMMEDIATE) {
        if (var.type->type == ARRAY) {
            sprintf(str, "$%s", var.value.string);
        } else {
            sprintf(str, "$%ld", var.value.integer);
        }
    } else {
        if (var.symbol->linkage != LINK_NONE) {
            if (var.type->type == ARRAY || var.type->type == FUNCTION) {
                sprintf(str, "$%s", sym_name(var.symbol));
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
    const char *mov;

    switch (v.kind) {
    case IMMEDIATE:
        assert( v.type->type == ARRAY );
        fprintf(s, "\tmovq\t%s, %%%s\n", refer(v), reg(r, 8));
        break;
    case DIRECT:
        mov =
            (v.symbol->linkage != LINK_NONE &&
                (v.type->type == ARRAY || v.type->type == FUNCTION)) ? "movq" :
            "leaq";
        fprintf(s, "\t%s\t%s, %%%s\t# load &%s\n",
            mov, refer(v), reg(r, 8), v.symbol->name);
        break;
    case DEREF:
        /* Address of dereferenced variable is removed by evaluation. Exception
         * is loading plain array or function values, which decay into loading
         * their address. */
        fprintf(s, "\tmovq\t%d(%%rbp), %%r10\n", v.symbol->stack_offset);
        fprintf(s, "\tleaq\t%d(%%r10), %%%s\t# load (%s + %d)\n",
            v.offset, reg(r, 8), v.symbol->name, v.offset);
        break;
    }
}

/* Load variable v to register r, cast to type t. Handles sign- and width
 * extension for integer types.
 */
static void load_as(FILE *s, struct var v, enum reg r, const struct typetree *t)
{
    const char *mov;

    assert(is_integer(t) || is_pointer(t));
    assert(t->size == 4 || t->size == 8);
    assert(v.type->size <= t->size);

    mov =
        (v.type->size == 1 && is_unsigned(v.type) && t->size == 4) ? "movzbl" :
        (v.type->size == 1 && is_unsigned(v.type) && t->size == 8) ? "movzbq" :
        (v.type->size == 1 && t->size == 4) ? "movsbl" :
        (v.type->size == 1 && t->size == 8) ? "movsbq" :
        (v.type->size == 2 && is_unsigned(v.type) && t->size == 4) ? "movzwl" :
        (v.type->size == 2 && is_unsigned(v.type) && t->size == 8) ? "movzwq" :
        (v.type->size == 2 && t->size == 4) ? "movswl" :
        (v.type->size == 2 && t->size == 8) ? "movswq" :
        (v.type->size == 4 && is_unsigned(v.type) && t->size == 8) ? "movl" :
        (v.type->size == 4 && t->size == 8) ? "movslq" :
        (v.type->size == t->size && t->size == 4) ? "movl" :
        (v.type->size == t->size && t->size == 8) ? "movq" :
        NULL;

    assert(mov);

    switch (v.kind) {
    case DIRECT:
        fprintf(s, "\t%s\t%s, %%%s\t# load %s\n",
            mov, refer(v), reg(r, t->size), v.symbol->name);
        break;
    case DEREF:
        /* Suspiciously similar to refer(), should consider refactoring. */
        if (v.symbol->linkage != LINK_NONE) {
            if (v.type->type == ARRAY || v.type->type == FUNCTION) {
                fprintf(s, "\tmovq\t$%s, %%r10\n", sym_name(v.symbol));
            } else {
                fprintf(s, "\tmovq\t%s(%%rip), %%r10\n", sym_name(v.symbol));
            }
        } else {
            fprintf(s, "\tmovq\t%d(%%rbp), %%r10\n", v.symbol->stack_offset);
        }
        fprintf(s, "\t%s\t%d(%%r10), %%%s\t# load *%s\n",
            mov, v.offset, reg(r, t->size), v.symbol->name);
        break;
    case IMMEDIATE:
        fprintf(s, "\tmov%c\t%s, %%%s\n",
            asmsuffix(t), refer(v), reg(r, t->size));
        break;
    }
}

/* Load variable to register.
 */
static void load(FILE *s, struct var v, enum reg r)
{
    if (v.type->type == ARRAY) {
        load_address(s, v, r);
    } else {
        struct typetree t = *v.type;

        /* We only operate with 32 or 64 bit register values, but variables can
         * be stored with byte or short width. Promote to 32 bit if required. */
        t.size = (t.size < 4) ? 4 : t.size;
        if (t.type == OBJECT) {
            assert(t.size <= 8);
            t.type = INTEGER;
            t.flags = 0x0001; /* Unsigned. */
        }
        load_as(s, v, r, &t);
    }
}

static void store(FILE *s, enum reg r, struct var v)
{
    assert( is_scalar(v.type) || v.type->size <= 8 );
    assert( v.kind == DIRECT || v.kind == DEREF );

    if (v.kind == DIRECT) {
        fprintf(s, "\tmov%c\t%%%s, %s\t# store %s\n",
            asmsuffix(v.type), reg(r, v.type->size), refer(v), v.symbol->name);
    } else {
        fprintf(s, "\tmovq\t%d(%%rbp), %%r10\n", v.symbol->stack_offset);
        fprintf(s, "\tmov%c\t%%%s, %d(%%r10)\t# store *%s\n",
            asmsuffix(v.type), reg(r, v.type->size), v.offset, v.symbol->name);
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
static void 
call(FILE *s, int n, const struct var *args, struct var res, struct var func)
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
            int chunks = N_EIGHTBYTES(args[i].type),
                size = args[i].type->size,
                j;
            struct var slice = args[i];

            for (j = 0; j < chunks; ++j) {
                int width = (size < 8) ? size % 8 : 8;

                size -= width;
                assert( eightbyte[j] == PC_INTEGER );
                assert( width == 1 || width == 2 || width == 4 || width == 8 );

                slice.type = type_init_integer(width);
                slice.offset = args[i].offset + j * 8;
                load(s, slice, param_int_reg[next_integer_reg++]);
            }

            assert( size == 0 );
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

    /* Assign storage to parameters.  */
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

    /* Move arguments from register to stack. */
    for (i = 0; i < func->params.length; ++i) {
        enum param_class *eightbyte = params[i];

        if (*eightbyte != PC_MEMORY) {
            int n = N_EIGHTBYTES(func->fun->type->member[i].type), j;
            struct var ref = { NULL, NULL, DIRECT };

            ref.symbol = func->params.elem[i];
            for (j = 0; j < n; ++j) {
                ref.type = type_init_integer(8);
                ref.offset = j * 8;
                store(s, param_int_reg[next_integer_reg++], ref);
            }
        }
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

    if (*pc != PC_MEMORY) {
        int i;
        int n = N_EIGHTBYTES(val.type);
        int size = val.type->size;
        struct var slice = val;

        /* As we only support integer class, limit to two registers. Note that
         * the classification algorithm will never allocate more than two
         * integer registers for a single type. */
        assert( n <= 2 );

        /* This has a lot in common with call(..), and could probably be
         * refactored. */
        for (i = 0; i < n; ++i) {
            int width = (size < 8) ? size % 8 : 8;

            size -= width;
            assert( pc[i] == PC_INTEGER );
            assert( width == 1 || width == 2 || width == 4 || width == 8 );

            slice.type = type_init_integer(width);
            slice.offset = val.offset + i * 8;
            load(s, slice, ret_int_reg[next_int_reg++]);
        }

        assert( size == 0 );
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

static void asm_op(FILE *stream, const struct op *op)
{
    static int n_args;
    static struct var *args;

    switch (op->type) {
    case IR_ASSIGN:
        if (op->a.type->size == op->b.type->size && op->a.type->size > 8) {
            load_address(stream, op->a, DI);
            load_address(stream, op->b, SI);
            fprintf(stream, "\tmovq\t$%d, %%rdx\n", op->a.type->size);
            fprintf(stream, "\tcall\tmemcpy\n");
        } else {
            /* TODO: consider cast before assignment. */
            /*if (!type_equal(op->a.type, op->b.type)) {
                error("Unequal types:");
                error(" -> %s", typetostr(op->a.type));
                error(" -> %s", typetostr(op->b.type));
            }*/
            /*assert( type_equal(op->a.type, op->b.type) );*/
            load(stream, op->b, AX);
            store(stream, AX, op->a);
        }
        break;
    case IR_CAST:
        assert(op->a.type->size != op->b.type->size);
        load_as(stream, op->b, AX, op->a.type);
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
        fprintf(stream, "\tsetge\t%%al\n");
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
        if (*res != PC_NO_CLASS) {
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
    const struct symbol *symbol;
    union value value;

    symbol = target.symbol;
    value = val.value;

    if (!target.offset) {
        if (symbol->linkage == LINK_EXTERN) {
            fprintf(stream, "\t.globl\t%s\n", sym_name(symbol));
        }

        if (is_aggregate(symbol->type)) {
            fprintf(stream, "\t.align\t16\n");
        }

        fprintf(stream, "%s:\n", sym_name(symbol));
    }

    switch (target.type->type) {
    case INTEGER:
        switch (target.type->size) {
        case 1:
            fprintf(stream, "\t.byte\t%d\n", (unsigned char) value.integer);
            break;
        case 2:
            fprintf(stream, "\t.short\t%d\n", (short) value.integer);
            break;
        case 4:
            fprintf(stream, "\t.int\t%d\n", (int) value.integer);
            break;
        case 8:
            fprintf(stream, "\t.quad\t%ld\n", value.integer);
            break;
        default:
            assert(0);
        }
        break;
    case POINTER:
        fprintf(stream, "\t.quad\t");
        if (target.type->next->type == INTEGER &&
            target.type->next->size == 1 &&
            value.string)
        {
            fprintf(stream, "%s", value.string);
        } else {
            fprintf(stream, "%ld", value.integer);
        }
        fprintf(stream, "\n");
        break;
    case ARRAY:
        if (
            target.type->next->type == INTEGER && 
            target.type->next->size == 1 &&
            value.string
        ) {
            /* Special handling for string type. */
            fprintf(stream, "\t.string\t\"");
            output_string(stream, value.string);
            fprintf(stream, "\"\n");
        } else {
            /* Default to this for static initialization. */
            assert(val.type->type == INTEGER && !val.value.integer);
            fprintf(stream, "\t.skip %d, 0\n", target.type->size);
        }
        break;
    case OBJECT:
        /* Only way this happens is static initialization to zero. */
        assert(val.type->type == INTEGER && !val.value.integer);
        fprintf(stream, "\t.skip %d, 0\n", target.type->size);
        break;
    default:
        assert(0);
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
    int i;

    if (decl->head->n) {
        fprintf(stream, "\t.data\n");
        for (i = 0; i < decl->head->n; ++i) {
            assert(decl->head->code[i].type == IR_ASSIGN);

            asm_immediate(stream,
                decl->head->code[i].a, decl->head->code[i].b);
        }
    }

    if (decl->fun) {
        assert(decl->fun->type->type == FUNCTION);
        asm_function(stream, decl);
    }
}
