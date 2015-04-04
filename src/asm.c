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

static const char* reg(reg_t r, unsigned w)
{
    const char *x86_64_regs[] = {"r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"};
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


static char *
refer(const var_t var)
{
    static char str[256];

    switch (var.kind) {
        case DIRECT:
            if (var.symbol->param_n && !var.symbol->stack_offset) {
                sprintf(str, "%%%s", reg(pregs[var.symbol->param_n - 1], var.type->size));
            } else if (!var.symbol->depth) {
                if (var.type->type == ARRAY) {
                    sprintf(str, "$%s", var.symbol->name);
                } else {
                    sprintf(str, "%s(%%rip)", var.symbol->name);
                }
            } else {
                sprintf(str, "%d(%%rbp)", var.symbol->stack_offset + var.offset);
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
                    internal_error("Refer to symbol with type %s.", typetostr(var.type));
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
    unsigned w = var.type->size;
    char mov[] = { 'm', 'o', 'v', 'x', '\0', '\0' };

    mov[3] = suffix;

    /* Special write to registers that do not have byte resolution. */
    if (w == 1 && dest > 3) {
        mov[3] = 'z';
        mov[4] = 'b';
    }

    switch (var.kind) {
        case IMMEDIATE:
            fprintf(stream, "\t%s\t%s, %%%s\n", mov, refer(var), reg(dest, w));
            break;
        case DIRECT:
            if (var.type->type == ARRAY && var.symbol->depth) {
                fprintf(stream, "\tleaq\t%d(%%rbp), %%%s\t# load %s\n", var.symbol->stack_offset, reg(dest, w), var.symbol->name);
            } else {
                fprintf(stream, "\t%s\t%s, %%%s\t# load %s\n", mov, refer(var), reg(dest, w), var.symbol->name);
            }
            break;
        case DEREF:
            assert(var.symbol->depth);
            fprintf(stream, "\tmovq\t%d(%%rbp), %%r10\n", var.symbol->stack_offset);
            if (var.type->type == ARRAY) {
                fprintf(stream, "\tleaq\t%d(%%r10), %%%s\t# load *%s\n", var.offset, reg(dest, 8), var.symbol->name);
            } else {
                fprintf(stream, "\t%s\t%d(%%r10), %%%s\t# load *%s\n", mov, var.offset, reg(dest, w), var.symbol->name);
            }
            break;
    }
}

static void
store(FILE *stream, reg_t source, var_t var)
{
    char suffix = asmsuffix(var.type);
    unsigned w = var.type->size;

    switch (var.kind) {
        case DIRECT:
            fprintf(stream, "\tmov%c\t%%%s, %s\t# store %s\n", suffix, reg(source, w), refer(var), var.symbol->name);
            break;
        case DEREF:
            fprintf(stream, "\tmovq\t%d(%%rbp), %%r10\n", var.symbol->stack_offset);
            if (var.offset) {
                fprintf(stream, "\tmov%c\t%%%s, %d(%%r10)\t# store *%s\n", suffix, reg(source, w), var.offset, var.symbol->name);
            } else {
                fprintf(stream, "\tmov%c\t%%%s, (%%r10)\t# store *%s\n", suffix, reg(source, w), var.symbol->name);
            }
            break;
        default:
            internal_error("Store immediate value with type %s.", typetostr(var.type));
            exit(1);
    }
}

/* Follow AMD64 ABI: http://www.x86-64.org/documentation/abi.pdf.
 * Registers %rsp, %rbp, %rbx, %r12, %r13, %r14, %r15 are preserved by callee.
 * Other registers must be saved by caller before invoking a function.
 */
static int
fassembleparams(FILE *stream, op_t *ops, int i)
{
    int n;

    /* Assume there is always a IR_CALL after params, not getting out of bounds. */
    if (ops[i].type != IR_PARAM)
        return 0;

    if (ops[i].a.type->type != INTEGER && ops[i].a.type->type != POINTER
        && ops[i].a.type->type != ARRAY) {
        error("Parameter other than integer types are not supported.");
        exit(1);
    }

    /* Need to preserve full width of existing register values. */
    if (i < 6) {
        fprintf(stream, "\tpushq\t%%%s\t\t# save arg %d\n", reg(pregs[i], 8), i);
        load(stream, ops[i].a, pregs[i]);
        return 1 + fassembleparams(stream, ops, i + 1);
    }

    n = fassembleparams(stream, ops, i + 1);
    fprintf(stream, "\tpushq\t%s\n", refer(ops[i].a));
    return n + 1;
}

static void
fassembleop(FILE *stream, const op_t op)
{
    int i;
    switch (op.type) {
        case IR_ASSIGN:
            load(stream, op.b, AX);
            store(stream, AX, op.a);
            break;
        case IR_DEREF:
            load(stream, op.b, BX);
            fprintf(stream, "\tmov%c\t(%%rbx), %%%s\n", asmsuffix(op.a.type), reg(AX, op.a.type->size));
            store(stream, AX, op.a);
            break;
        case IR_CALL:
            if (op.b.symbol->type->type != FUNCTION) {
                error("Only supports call by name directly.");
                exit(1);
            }
            fprintf(stream, "\tcall\t%s\n", op.b.symbol->name);
            for (i = op.b.type->n_args - 1; i >= 0; --i) {
                fprintf(stream, "\tpopq\t%%%s\t\t# restore\n", reg(pregs[i], 8));
            }
            if (op.a.type->type != NONE) {
                store(stream, AX, op.a);
            }
            break;
        case IR_ADDR:
            fprintf(stream, "\tleaq\t%s, %%rax\n", refer(op.b));
            store(stream, AX, op.a);
            break;
        case IR_OP_ADD:
            load(stream, op.b, AX);
            if (op.c.kind == DIRECT || op.c.kind == IMMEDIATE) {
                fprintf(stream, "\taddq\t%s, %%rax\n", refer(op.c));
            } else {
                load(stream, op.c, BX);
                fprintf(stream, "\taddq\t%%rbx, %%rax\n");
            }
            store(stream, AX, op.a);
            break;
        case IR_OP_SUB:
            load(stream, op.b, AX);
            if (op.c.kind == DIRECT || op.c.kind == IMMEDIATE) {
                fprintf(stream, "\tsubq\t%s, %%rax\n", refer(op.c));
            } else {
                load(stream, op.c, BX);
                fprintf(stream, "\tsubq\t%%rbx, %%rax\n");
            }
            store(stream, AX, op.a);
            break;
        case IR_OP_MUL:
            load(stream, op.c, AX);
            if (op.b.kind == DIRECT || op.b.kind == IMMEDIATE) {
                fprintf(stream, "\tmul%c\t%s\n", asmsuffix(op.b.type), refer(op.b));
            } else {
                load(stream, op.b, BX);
                fprintf(stream, "\tmul%c\t%%%s\n", asmsuffix(op.b.type), reg(BX, op.b.type->size));
            }
            store(stream, AX, op.a);
            break;
        case IR_OP_BITWISE_AND:
            load(stream, op.b, AX);
            load(stream, op.c, BX);
            fprintf(stream, "\tand\t%%rbx, %%rax\n");
            store(stream, AX, op.a);
            break;
        case IR_OP_BITWISE_XOR:
            load(stream, op.b, AX);
            load(stream, op.c, BX);
            fprintf(stream, "\txor\t%%rbx, %%rax\n");
            store(stream, AX, op.a);
            break;
        case IR_OP_LOGICAL_AND:
            load(stream, op.b, AX);
            load(stream, op.c, BX);
            fprintf(stream, "\tand\t%%rbx, %%rax\n");
            fprintf(stream, "\tcmp\t$0, %%rax\n");
            fprintf(stream, "\tsetg\t%%al\n");
            fprintf(stream, "\tmovzx\t%%al, %%rax\n");
            store(stream, AX, op.a);
            break;
        case IR_OP_LOGICAL_OR:
            load(stream, op.b, AX);
            load(stream, op.c, BX);
            fprintf(stream, "\tor\t%%rbx, %%rax\n");
            fprintf(stream, "\tcmp\t$0, %%rax\n");
            fprintf(stream, "\tsetg\t%%al\n");
            fprintf(stream, "\tmovzx\t%%al, %%rax\n");
            store(stream, AX, op.a);
            break;
        case IR_OP_EQ:
            load(stream, op.b, AX);
            load(stream, op.c, BX);
            fprintf(stream, "\tcmp\t%%rbx, %%rax\n");
            fprintf(stream, "\tsetz\t%%al\n");
            fprintf(stream, "\tmovzx\t%%al, %%rax\n");
            store(stream, AX, op.a);
            break;
        case IR_OP_GE:
            load(stream, op.b, AX);
            load(stream, op.c, BX);
            fprintf(stream, "\tcmp\t%%rbx, %%rax\n");
            fprintf(stream, "\tsetge\t%%al\n");
            fprintf(stream, "\tmovzx\t%%al, %%rax\n");
            store(stream, AX, op.a);
            break;
        case IR_OP_GT:
            load(stream, op.b, AX);
            load(stream, op.c, BX);
            fprintf(stream, "\tcmp\t%%rbx, %%rax\n");
            fprintf(stream, "\tsetg\t%%al\n");
            fprintf(stream, "\tmovzx\t%%al, %%rax\n");
            store(stream, AX, op.a);
            break;
        case IR_OP_NOT:
            load(stream, op.b, AX);
            fprintf(stream, "\tcmp\t$0, %%rax\n");
            fprintf(stream, "\tsetz\t%%al\n");
            fprintf(stream, "\tmovzx\t%%al, %%rax\n");
            store(stream, AX, op.a);
            break;
        default:
            internal_error("%s", "IR_OP not implemented.");
            exit(1);
    }
}

static void
fassembleblock(FILE *stream, map_t *memo, const block_t *block)
{
    int i;
    if (map_lookup(memo, block->label) != NULL) 
        return;
    map_insert(memo, block->label, (void*)"done");

    fprintf(stream, "%s:\n", block->label);
    for (i = 0; i < block->n; ++i) {
        if (block->code[i].type == IR_PARAM) {
            i += fassembleparams(stream, block->code + i, 0) - 1;
        } else {
            fassembleop(stream, block->code[i]);
        }
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
            fprintf(stream, "\t.globl\t%s\n", symbol->name);
        }
        fprintf(stream, "%s:\n", symbol->name);
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
            assert(target.type->next->type == INTEGER && target.type->next->size == 1);
            fprintf(stream, "\t.quad\t");
            fprintf(stream, "%s", value.string);
            fprintf(stream, "\n");
            break;
        case ARRAY:
            assert(target.type->next->type == INTEGER && target.type->next->size == 1);
            fprintf(stream, "\t.string\t\"");
            output_string(stream, value.string);
            fprintf(stream, "\"\n");
            break;
        default:
            assert(0);
    }
}

static void
assemble_function(FILE *stream, const symbol_t *sym, block_t *body, int locals_size)
{
    map_t memo;

    map_init(&memo);

    fprintf(stream, "\t.text\n");
    if (sym->linkage == LINK_EXTERN) {
        fprintf(stream, "\t.globl\t%s\n", sym->name);
    }
    fprintf(stream, "%s:\n", sym->name);
    fprintf(stream, "\tpushq\t%%rbp\n");
    fprintf(stream, "\tmovq\t%%rsp, %%rbp\n");
    if (locals_size)
        fprintf(stream, "\tsubq\t$%d, %%rsp\n", locals_size);

    fassembleblock(stream, &memo, body);

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

            assemble_immediate(stream, decl->head->code[i].a, decl->head->code[i].b);
        }
    }

    if (decl->fun) {
        assert(decl->fun->type->type == FUNCTION);
        
        assemble_function(stream, decl->fun, decl->body, decl->locals_size);
    }
}
