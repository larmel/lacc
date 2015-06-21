#include "ir.h"
#include "type.h"
#include "string.h"
#include "symbol.h"
#include "error.h"
#include "util/map.h"

#include <assert.h>
#include <stdio.h>
#include <ctype.h>

/* Assembly instruction suffix based on value size. Char is 'b', short is 'w',
 * int is 'l' and quadword (long) is 'q'. */
static char asmsuffix(const struct typetree *type)
{
    if (type->type == ARRAY) return 'q';
    if (type->size == 1) return 'b';
    if (type->size == 2) return 'w';
    if (type->size == 4) return 'l';
    return 'q';
}

enum reg {
    AX = 0,
    BX,
    CX,
    DX,
    BP,
    SP,
    SI,
    DI,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15
};

/* Promote all operands to 32 bit, to not have to worry about partial register
 * not being zeroed properly. On 64-bit, the upper half will be zeroed auto-
 * matically. */
static const char* reg(enum reg r, unsigned w)
{
    const char *x86_64_regs[] = {
        "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
    };

    switch (r) {
    case AX: return (w==1) ? "al" : (w==2) ? "ax" : (w==4) ? "eax" : "rax";
    case BX: return (w==1) ? "bl" : (w==2) ? "bx" : (w==4) ? "ebx" : "rbx";
    case CX: return (w==1) ? "cl" : (w==2) ? "cx" : (w==4) ? "ecx" : "rcx";
    case DX: return (w==1) ? "dl" : (w==2) ? "dx" : (w==4) ? "edx" : "rdx";
    case BP: return "rbp";
    case SP: return "rsp";
    case SI: return (w==2) ? "si" : (w==4) ? "esi" : "rsi";
    case DI: return (w==2) ? "di" : (w==4) ? "edi" : "rdi";
    default:
        return x86_64_regs[(int)r - 8];
    }
}

static enum reg pregs[] = {DI, SI, DX, CX, R8, R9};

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

    assert( is_integer(t) );
    assert( t->size == 4 || t->size == 8 );
    assert( v.type->size <= t->size );

    mov =
        (v.type->size == 1 && v.type->is_unsigned && t->size == 4) ? "movzbl" :
        (v.type->size == 1 && v.type->is_unsigned && t->size == 8) ? "movzbq" :
        (v.type->size == 1 && t->size == 4) ? "movsbl" :
        (v.type->size == 1 && t->size == 8) ? "movsbq" :
        (v.type->size == 2 && v.type->is_unsigned && t->size == 4) ? "movzwl" :
        (v.type->size == 2 && v.type->is_unsigned && t->size == 8) ? "movzwq" :
        (v.type->size == 2 && t->size == 4) ? "movswl" :
        (v.type->size == 2 && t->size == 8) ? "movswq" :
        (v.type->size == 4 && v.type->is_unsigned && t->size == 8) ? "movl" :
        (v.type->size == 4 && t->size == 8) ? "movslq" :
        "mov";

    switch (v.kind) {
    case DIRECT:
        fprintf(s, "\t%s\t%s, %%%s\t# load %s\n",
            mov, refer(v), reg(r, t->size), v.symbol->name);
        break;
    case DEREF:
        assert(v.symbol->depth);
        fprintf(s, "\tmovq\t%d(%%rbp), %%r10\n", v.symbol->stack_offset);
        fprintf(s, "\t%s\t%d(%%r10), %%%s\t# load *%s\n",
            mov, v.offset, reg(r, t->size), v.symbol->name);
        break;
    case IMMEDIATE:
        fprintf(s, "\tmov\t%s, %%%s\n", refer(v), reg(r, t->size));
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
        t.type = (t.type == POINTER) ? INTEGER : t.type;
        load_as(s, v, r, &t);
    }
}

static void store(FILE *s, enum reg r, struct var v)
{
    assert( is_scalar(v.type) );
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

static int fassembleop(FILE *stream, const struct op *op)
{
    int i, n = 0;

    switch (op->type) {
    case IR_ASSIGN:
        if (op->a.type->size == op->b.type->size && op->a.type->size > 8) {
            load_address(stream, op->a, DI);
            load_address(stream, op->b, SI);
            fprintf(stream, "\tmovq\t$%d, %%rdx\n", op->a.type->size);
            fprintf(stream, "\tcall\tmemcpy\n");
        } else {
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
        i = 0;
        while (op->type == IR_PARAM) {
            op = op + 1;
            i++;
        }
        n = i;
        assert(op->type == IR_CALL);
        while (i-- > 0) {
            op = op - 1;
            assert(op->type == IR_PARAM);
            switch (op->a.type->type) {
            case INTEGER:
            case POINTER:
            case ARRAY:
            case FUNCTION:
                if (i < 6) {
                    load(stream, op->a, pregs[i]);
                } else {
                    fprintf(stream, "\tpush\t%s\n", refer(op->a));
                }
                break;
            default:
                error("Parameter type not supported.");
                exit(1);
            }
        }
        op = op + n;
        assert(op->type == IR_CALL);
    case IR_CALL:
        if (op->b.symbol->type->type != FUNCTION) {
            error("Only supports call by name directly.");
            exit(1);
        }
        if (op->b.type->is_vararg) {
            /* Reset flag over number of vector registers used for variable
             * argument list. */
            fprintf(stream, "\tmovq\t$0, %%rax\n");
        }
        fprintf(stream, "\tcall\t%s\n", op->b.symbol->name);
        if (n > 6) {
            /* Restore stack pointer after pushing arguments that did not
             * fit in registers. */
            fprintf(stream, "\tadd\t$%d, %%rsp\t# remove arguments\n",
                (n - 6) * 8);
        }
        if (op->a.type->type != NONE) {
            /* No result for void function. */
            store(stream, AX, op->a);
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
    case IR_OP_BITWISE_AND:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\tand\t%%rbx, %%rax\n");
        store(stream, AX, op->a);
        break;
    case IR_OP_BITWISE_XOR:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\txor\t%%rbx, %%rax\n");
        store(stream, AX, op->a);
        break;
    case IR_OP_LOGICAL_AND:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\tand\t%%rbx, %%rax\n");
        fprintf(stream, "\tcmp\t$0, %%rax\n");
        fprintf(stream, "\tsetg\t%%al\n");
        fprintf(stream, "\tmovzx\t%%al, %%rax\n");
        store(stream, AX, op->a);
        break;
    case IR_OP_LOGICAL_OR:
        load(stream, op->b, AX);
        load(stream, op->c, BX);
        fprintf(stream, "\tor\t%%rbx, %%rax\n");
        fprintf(stream, "\tcmp\t$0, %%rax\n");
        fprintf(stream, "\tsetg\t%%al\n");
        fprintf(stream, "\tmovzx\t%%al, %%rax\n");
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
        if (op->b.type->is_unsigned) {
            assert( op->b.type->is_unsigned == op->c.type->is_unsigned );
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

    return n + 1;
}

static void fassembleblock(FILE *stream, map_t *memo, const struct block *block)
{
    int i;

    if (map_lookup(memo, block->label) != NULL) 
        return;

    map_insert(memo, block->label, (void*)"done");

    fprintf(stream, "%s:\n", block->label);
    for (i = 0; i < block->n; ) {
        i += fassembleop(stream, block->code + i);
    }

    if (block->jump[0] == NULL && block->jump[1] == NULL) {
        /* By default return the last expression evaluated in the block.
         * Check size <= 8 to avoid returning object types by accident. */
        if (block->expr.type && block->expr.type->type != NONE
            && block->expr.type->size <= 8)
        {
            load(stream, block->expr, AX);
        }
        fprintf(stream, "\tleaveq\n");
        fprintf(stream, "\tretq\n");
    } else if (block->jump[1] == NULL) {
        if (map_lookup(memo, block->jump[0]->label) != NULL) {
            fprintf(stream, "\tjmp\t%s\n", block->jump[0]->label);
        }
        fassembleblock(stream, memo, block->jump[0]);
    } else {
        load(stream, block->expr, AX);
        fprintf(stream, "\tcmpq\t$0, %%rax\n");
        fprintf(stream, "\tje\t%s\n", block->jump[0]->label);
        if (map_lookup(memo, block->jump[1]->label) != NULL) {
            fprintf(stream, "\tjmp\t%s\n", block->jump[1]->label);
        }
        fassembleblock(stream, memo, block->jump[1]);
        fassembleblock(stream, memo, block->jump[0]);
    }
}

static void assemble_immediate(FILE *stream, struct var target, struct var val)
{
    const struct symbol *symbol;
    union value value;

    symbol = target.symbol;
    value = val.value;

    if (!target.offset) {
        if (symbol->linkage == LINK_EXTERN) {
            fprintf(stream, "\t.globl\t%s\n", sym_name(symbol));
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

static int assign_storage(const struct decl *fun)
{
    int i,
        offset = 0;
    struct symbol *sym;

    /* Assume < 6 integer parameters that fit in registers and needs to be
     * pushed to stack. */
    for (i = 0; i < fun->params.length; ++i) {
        sym = fun->params.elem[i];

        assert(!sym->stack_offset);
        assert(sym->linkage == LINK_NONE);
        offset -= sym->type->size;
        sym->stack_offset = offset;
    }

    for (i = 0; i < fun->locals.length; ++i) {
        sym = fun->locals.elem[i];
        assert(!sym->stack_offset);

        if (sym->linkage == LINK_NONE) {
            offset -= sym->type->size;
            sym->stack_offset = offset;
        }
    }

    return -offset;
}

static void assemble_function(FILE *stream, const struct decl *decl)
{
    map_t memo;
    int i, locals_size;

    map_init(&memo);

    locals_size = assign_storage(decl);

    fprintf(stream, "\t.text\n");
    if (decl->fun->linkage == LINK_EXTERN) {
        fprintf(stream, "\t.globl\t%s\n", sym_name(decl->fun));
    }

    fprintf(stream, "\t.type\t%s, @function\n", sym_name(decl->fun));
    fprintf(stream, "%s:\n", sym_name(decl->fun));
    fprintf(stream, "\tpushq\t%%rbp\n");
    fprintf(stream, "\tmovq\t%%rsp, %%rbp\n");

    if (locals_size) {
        fprintf(stream, "\tsubq\t$%d, %%rsp\n", locals_size);
    }

    for (i = 0; i < decl->params.length; ++i) {
        struct symbol *sym = decl->params.elem[i];

        fprintf(stream, "\tmov%c\t%%%s, %d(%%rbp)\n",
            asmsuffix(sym->type),
            reg(pregs[i], sym->type->size),
            sym->stack_offset);
    }

    fassembleblock(stream, &memo, decl->body);

    /* This is required to see function names in valgrind. */
    fprintf(stream, "\t.size\t%s, .-%s\n",
        sym_name(decl->fun), sym_name(decl->fun));

    map_finalize(&memo);
}

void fassemble(FILE *stream, const struct decl *decl)
{
    int i;

    if (decl->head->n) {
        fprintf(stream, "\t.data\n");
        for (i = 0; i < decl->head->n; ++i) {
            assert(decl->head->code[i].type == IR_ASSIGN);

            assemble_immediate(stream,
                decl->head->code[i].a, decl->head->code[i].b);
        }
    }

    if (decl->fun) {
        assert(decl->fun->type->type == FUNCTION);
        assemble_function(stream, decl);
    }
}
