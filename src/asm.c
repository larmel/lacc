#include "ir.h"
#include "symbol.h"
#include "error.h"
#include "util/map.h"

#include <stdio.h>

static const char *pregs[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};

static char *
refer(const var_t var)
{
    static char str[256];
    switch (var.kind) {
        case DIRECT:
            if (var.symbol->param_n && !var.symbol->stack_offset)
                sprintf(str, "%%%s", pregs[var.symbol->param_n - 1]);
            else if (var.symbol->depth == 0)
                if (var.type->type == ARRAY || var.type->type == POINTER)
                    sprintf(str, "$%s", var.symbol->name);
                else
                    sprintf(str, "%s(%%rip)", var.symbol->name);
            else
                sprintf(str, "%d(%%rbp)", var.symbol->stack_offset);
            break;
        case IMMEDIATE:
            sprintf(str, "$%ld", var.value.v_long);
            break;
        default:
            error("Could not assemble reference to offset variable.");
            exit(1);
    }
    return str;
}

static void
load(FILE *stream, var_t var, const char *dest)
{
    switch (var.kind) {
        case IMMEDIATE:
            fprintf(stream, "\tmovq\t$%ld, %%%s\n", var.value.v_long, dest);
            break;
        case DIRECT:
            if (var.type->type == ARRAY && var.symbol->depth)
                fprintf(stream, "\tleaq\t%d(%%rbp), %%%s\t# load %s\n", var.symbol->stack_offset, dest, var.symbol->name);
            else
                fprintf(stream, "\tmovq\t%s, %%%s\t# load %s\n", refer(var), dest, var.symbol->name);
            break;
        case OFFSET:
            fprintf(stream, "\tmovq\t%d(%%rbp), %%r10\t# load *%s\n", var.symbol->stack_offset, var.symbol->name);
            if (var.type->type == ARRAY) {
                if (var.offset)
                    fprintf(stream, "\tleaq\t%d(%%r10), %%%s\n", var.offset, dest);
                else
                    fprintf(stream, "\tleaq\t(%%r10), %%%s\n", dest);
            } else {
                if (var.offset)
                    fprintf(stream, "\tmovq\t%d(%%r10), %%%s\n", var.offset, dest);
                else
                    fprintf(stream, "\tmovq\t(%%r10), %%%s\n", dest);
            }
            break;
    }
}

static void
store(FILE *stream, const char *source, var_t var)
{
    switch (var.kind) {
        case IMMEDIATE:
            fprintf(stream, "\t(error: cannot write to immediate)\n");
            break;
        case DIRECT:
            fprintf(stream, "\tmovq\t%%%s, %d(%%rbp)\t# store %s\n", source, var.symbol->stack_offset, var.symbol->name);
            break;
        case OFFSET:
            fprintf(stream, "\tmovq\t%d(%%rbp), %%r10\t# store *%s\n", var.symbol->stack_offset, var.symbol->name);
            if (var.offset)
                fprintf(stream, "\tmovq\t%%%s, %d(%%r10)\n", source, var.offset);
            else
                fprintf(stream, "\tmovq\t%%%s, (%%r10)\n", source);
            break;
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

    if (ops[i].a.type->type != INT64_T && ops[i].a.type->type != POINTER
        && ops[i].a.type->type != ARRAY) {
        error("Parameter other than integer types are not supported.");
        exit(1);
    }

    if (i < 6) {
        fprintf(stream, "\tpushq\t%%%s\t\t# save arg %d\n", pregs[i], i);
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
            load(stream, op.b, "rax");
            store(stream, "rax", op.a);
            break;
        case IR_DEREF:
            load(stream, op.b, "rbx");
            fprintf(stream, "\tmovq\t(%%rbx), %%rax\n");
            store(stream, "rax", op.a);
            break;
        case IR_PARAM:
            error("Rogue parameter.");
            break;
        case IR_CALL:
            if (op.b.symbol->type->type != FUNCTION) {
                error("Only supports call by name directly.");
                exit(1);
            }
            fprintf(stream, "\tcall\t%s\n", op.b.symbol->name);
            for (i = op.b.type->n_args - 1; i >= 0; --i)
                fprintf(stream, "\tpopq\t%%%s\t\t# restore\n", pregs[i]);
            store(stream, "rax", op.a);
            break;
        case IR_ADDR:
            fprintf(stream, "\tleaq\t%s, %%rax\n", refer(op.b));
            store(stream, "rax", op.a);
            break;
        case IR_OP_ADD:
            load(stream, op.b, "rax");
            if (op.c.kind == DIRECT || op.c.kind == IMMEDIATE)
                fprintf(stream, "\taddq\t%s, %%rax\n", refer(op.c));
            else {
                load(stream, op.c, "rbx");
                fprintf(stream, "\taddq\t%%rbx, %%rax\n");
            }
            store(stream, "rax", op.a);
            break;
        case IR_OP_SUB:
            load(stream, op.b, "rax");
            if (op.c.kind == DIRECT || op.c.kind == IMMEDIATE)
                fprintf(stream, "\tsubq\t%s, %%rax\n", refer(op.c));
            else {
                load(stream, op.c, "rbx");
                fprintf(stream, "\tsubq\t%%rbx, %%rax\n");
            }
            store(stream, "rax", op.a);
            break;
        case IR_OP_MUL:
            load(stream, op.c, "rax");
            if (op.b.kind == DIRECT || op.b.kind == IMMEDIATE)
                fprintf(stream, "\tmulq\t%s\n", refer(op.b));
            else {
                load(stream, op.b, "rbx");
                fprintf(stream, "\tmulq\t%%rbx\n");
            }
            store(stream, "rax", op.a);
            break;
        case IR_OP_BITWISE_AND:
            load(stream, op.b, "rax");
            load(stream, op.c, "rbx");
            fprintf(stream, "\tandq\t%%rbx, %%rax\n");
            store(stream, "rax", op.a);
            break;
        case IR_OP_BITWISE_XOR:
            load(stream, op.b, "rax");
            load(stream, op.c, "rbx");
            fprintf(stream, "\txorq\t%%rbx, %%rax\n");
            store(stream, "rax", op.a);
            break;
        default:
            fprintf(stream, "\t(none)\n");
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
        load(stream, block->expr, "rax");
        fprintf(stream, "\tleaveq\n");
        fprintf(stream, "\tretq\n");
    } else if (block->jump[1] == NULL) {
        if (map_lookup(memo, block->jump[0]->label) != NULL)
            fprintf(stream, "\tjmp\t%s\n", block->jump[0]->label);
        fassembleblock(stream, memo, block->jump[0]);
    } else {
        load(stream, block->expr, "rax");
        fprintf(stream, "\tcmpq\t$0, %%rax\n");
        fprintf(stream, "\tje\t%s\n", block->jump[0]->label);
        
        if (map_lookup(memo, block->jump[1]->label) != NULL)
            fprintf(stream, "\tjmpq\t%s\n", block->jump[1]->label);
        fassembleblock(stream, memo, block->jump[1]);
        fassembleblock(stream, memo, block->jump[0]);
    }
}

void
fasmimmediate(FILE *stream, const block_t *body)
{
    int i;
    const symbol_t *symbol;
    value_t value;

    fprintf(stream, "\t.data\n");
    for (i = 0; i < body->n; ++i) {
        if (body->n != 1 || body->code[0].type != IR_ASSIGN) {
            error("Internal error: External declaration must have constant value.");
            exit(1);
        }
        symbol = body->code[i].a.symbol;
        value = body->code[i].b.value;
        
        fprintf(stream, "\t.globl\t%s\n", symbol->name);
        fprintf(stream, "%s:\n", symbol->name);
        switch (symbol->type->type) {
            case INT64_T:
                fprintf(stream, "\t.quad\t%ld\n", value.v_long);
                break;
            case POINTER:
            case ARRAY:
                fprintf(stream, "\t.string \"%s\"\n", value.v_string);
                break;
            default:
                fprintf(stream, "\t (immediate)\n");
        }
    }
}

void
fassemble(FILE *stream, const function_t *func)
{
    map_t memo;

    if (!func->symbol) {
        fasmimmediate(stream, func->body);
        return;
    }

    map_init(&memo);

    fprintf(stream, "\t.text\n");
    fprintf(stream, "\t.globl\t%s\n", func->symbol->name);

    fprintf(stream, "%s:\n", func->symbol->name);
    fprintf(stream, "\tpushq\t%%rbp\n");
    fprintf(stream, "\tmovq\t%%rsp, %%rbp\n");
    if (func->locals_size)
        fprintf(stream, "\tsubq\t$%d, %%rsp\n", func->locals_size);

    fassembleblock(stream, &memo, func->body);

    map_finalize(&memo);
}
