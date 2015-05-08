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
static char asmsuffix(const typetree_t *type)
{
    if (type->type == ARRAY) return 'q';
    if (type->size == 1) return 'b';
    if (type->size == 2) return 'w';
    if (type->size == 4) return 'l';
    return 'q';
}

typedef enum {
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
} reg_t;

/* Promote all operands to 32 bit, to not have to worry about partial register
 * not being zeroed properly. On 64-bit, the upper half will be zeroed auto-
 * matically. */
static const char* reg(reg_t r, unsigned w)
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

static reg_t pregs[] = {DI, SI, DX, CX, R8, R9};

static const char *sym_name(const symbol_t *sym)
{
    if (sym->n) {
        static char name[128];
        snprintf(name, 127, "%s.%d", sym->name, sym->n);
        return name;
    }
    return sym->name;
}

static char *
refer(const var_t var)
{
    static char str[256];

    switch (var.kind) {
        case DIRECT:
            if (!var.symbol->depth || var.symbol->linkage == LINK_INTERN) {
                if (var.type->type == ARRAY || var.type->type == FUNCTION) {
                    sprintf(str, "$%s", sym_name(var.symbol));
                } else {
                    sprintf(str, "%s(%%rip)", sym_name(var.symbol));
                }
            } else {
                sprintf(str, "%d(%%rbp)",
                    var.symbol->stack_offset + var.offset);
            }
            break;
        case IMMEDIATE:
            switch (var.type->type) {
                case ARRAY:
                    sprintf(str, "$%s", var.value.string);
                    break;
                case INTEGER:
                case POINTER:
                    sprintf(str, "$%ld", var.value.integer);
                    break;
                default:
                    internal_error("Refer to symbol with type %s.",
                        typetostr(var.type));
                    exit(1);
            }
            break;
        default:
            assert(0);
    }

    return str;
}

static void
load(FILE *stream, var_t var, reg_t dest)
{
    char suffix = asmsuffix(var.type);
    unsigned w = (var.type->type == ARRAY) ? 8 : var.type->size;
    char mov[] = { 'm', 'o', 'v', 'x', '\0', '\0', '\0' };

    mov[3] = suffix;

    if (w == 1) {
        mov[3] = 'z';
        mov[4] = 'b';
        mov[5] = 'l';
        w = 4;
    }

    switch (var.kind) {
        case IMMEDIATE:
            fprintf(stream, "\t%s\t%s, %%%s\n", mov, refer(var), reg(dest, w));
            break;
        case DIRECT:
            if (var.type->type == ARRAY &&
                var.symbol->depth &&
                var.symbol->linkage == LINK_NONE)
            {
                fprintf(stream, "\tleaq\t%d(%%rbp), %%%s\t# load %s\n",
                    var.symbol->stack_offset, reg(dest, w), var.symbol->name);
            } else {
                fprintf(stream, "\t%s\t%s, %%%s\t# load %s\n",
                    mov, refer(var), reg(dest, w), var.symbol->name);
            }
            break;
        case DEREF:
            assert(var.symbol->depth);
            fprintf(stream, "\tmovq\t%d(%%rbp), %%r10\n",
                var.symbol->stack_offset);
            if (var.type->type == ARRAY) {
                fprintf(stream, "\tleaq\t%d(%%r10), %%%s\t# load *%s\n",
                    var.offset, reg(dest, 8), var.symbol->name);
            } else {
                fprintf(stream, "\t%s\t%d(%%r10), %%%s\t# load *%s\n",
                    mov, var.offset, reg(dest, w), var.symbol->name);
            }
            break;
    }
}

static void
store(FILE *stream, reg_t source, var_t var)
{
    char suffix = asmsuffix(var.type);
    unsigned w = (var.type->type == ARRAY) ? 8 : var.type->size;

    switch (var.kind) {
        case DIRECT:
            fprintf(stream, "\tmov%c\t%%%s, %s\t# store %s\n",
                suffix, reg(source, w), refer(var), var.symbol->name);
            break;
        case DEREF:
            fprintf(stream, "\tmovq\t%d(%%rbp), %%r10\n",
                var.symbol->stack_offset);
            if (var.offset) {
                fprintf(stream, "\tmov%c\t%%%s, %d(%%r10)\t# store *%s\n",
                    suffix, reg(source, w), var.offset, var.symbol->name);
            } else {
                fprintf(stream, "\tmov%c\t%%%s, (%%r10)\t# store *%s\n",
                    suffix, reg(source, w), var.symbol->name);
            }
            break;
        default:
            internal_error("Store immediate value with type %s.",
                typetostr(var.type));
            exit(1);
    }
}

static int
fassembleop(FILE *stream, const op_t *op)
{
    int i, n = 0;

    switch (op->type) {
        case IR_ASSIGN:
            load(stream, op->b, AX);
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
            if (op->b.type->vararg) {
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
            fprintf(stream, "\tleaq\t%s, %%rax\n", refer(op->b));
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
            fprintf(stream, "\tcmp\t%%rbx, %%rax\n");
            fprintf(stream, "\tsetz\t%%al\n");
            fprintf(stream, "\tmovzx\t%%al, %%rax\n");
            store(stream, AX, op->a);
            break;
        case IR_OP_GE:
            load(stream, op->b, AX);
            load(stream, op->c, BX);
            fprintf(stream, "\tcmp\t%%rbx, %%rax\n");
            fprintf(stream, "\tsetge\t%%al\n");
            fprintf(stream, "\tmovzx\t%%al, %%rax\n");
            store(stream, AX, op->a);
            break;
        case IR_OP_GT:
            load(stream, op->b, AX);
            load(stream, op->c, BX);
            fprintf(stream, "\tcmp\t%%rbx, %%rax\n");
            fprintf(stream, "\tsetg\t%%al\n");
            fprintf(stream, "\tmovzx\t%%al, %%rax\n");
            store(stream, AX, op->a);
            break;
        case IR_OP_NOT:
            load(stream, op->b, AX);
            fprintf(stream, "\tcmp\t$0, %%rax\n");
            fprintf(stream, "\tsetz\t%%al\n");
            fprintf(stream, "\tmovzx\t%%al, %%rax\n");
            store(stream, AX, op->a);
            break;
        default:
            internal_error("%s", "IR_OP not implemented.");
            exit(1);
    }

    return n + 1;
}

static void
fassembleblock(FILE *stream, map_t *memo, const block_t *block)
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
        if (block->expr.type->type != NONE) {
            load(stream, block->expr, AX);
        }
        fprintf(stream, "\tleaveq\n");
        fprintf(stream, "\tretq\n");
    } else if (block->jump[1] == NULL) {
        if (map_lookup(memo, block->jump[0]->label) != NULL)
            fprintf(stream, "\tjmp\t%s\n", block->jump[0]->label);
        fassembleblock(stream, memo, block->jump[0]);
    } else {
        load(stream, block->expr, AX);
        fprintf(stream, "\tcmpq\t$0, %%rax\n");
        fprintf(stream, "\tje\t%s\n", block->jump[0]->label);
        
        if (map_lookup(memo, block->jump[1]->label) != NULL)
            fprintf(stream, "\tjmpq\t%s\n", block->jump[1]->label);
        fassembleblock(stream, memo, block->jump[1]);
        fassembleblock(stream, memo, block->jump[0]);
    }
}

static void
assemble_immediate(FILE *stream, var_t target, var_t val)
{
    const symbol_t *symbol;
    value_t value;

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
                    fprintf(stream, "\t.byte\t%d\n",
                        (unsigned char) value.integer);
                    break;
                case 2:
                    fprintf(stream, "\t.short\t%d\n",
                        (short) value.integer);
                    break;
                case 4:
                    fprintf(stream, "\t.int\t%d\n",
                        (int) value.integer);
                    break;
                case 8:
                    fprintf(stream, "\t.quad\t%ld\n",
                        value.integer);
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

static int assign_storage(const decl_t *fun)
{
    int i,
        offset = 0;
    struct symbol *sym;

    /* Assume < 6 integer parameters that fit in registers and needs to be
     * pushed to stack. */
    for (i = 0; i < fun->params.length; ++i) {
        sym = fun->params.elem[i];

        assert(!sym->stack_offset);
        offset -= sym->type->size;
        sym->stack_offset = offset;
    }

    for (i = 0; i < fun->locals.length; ++i) {
        sym = fun->locals.elem[i];

        assert(!sym->stack_offset);
        offset -= sym->type->size;
        sym->stack_offset = offset;
    }

    return -offset;
}

static void
assemble_function(FILE *stream, const decl_t *decl)
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
        symbol_t *sym = decl->params.elem[i];

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

void
fassemble(FILE *stream, const decl_t *decl)
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
