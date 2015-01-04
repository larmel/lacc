#include "ir.h"
#include "symbol.h"
#include "error.h"
#include "util/map.h"

#include <stdio.h>


static void
load(FILE *stream, var_t var, const char *dest)
{
    switch (var.kind) {
        case IMMEDIATE:
            fprintf(stream, "\tmovq\t$%ld, %%%s\n", var.value.v_long, dest);
            break;
        case DIRECT:
            if (var.type->type == ARRAY)
                fprintf(stream, "\tleaq\t%d(%%rbp), %%%s\t# load %s\n", var.symbol->stack_offset, dest, var.symbol->name);
            else
                fprintf(stream, "\tmovq\t%d(%%rbp), %%%s\t# load %s\n", var.symbol->stack_offset, dest, var.symbol->name);
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

static char *
refer(const var_t var)
{
    static char str[256];
    switch (var.kind) {
        case DIRECT:
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
fassembleop(FILE *stream, const op_t op)
{
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
        case IR_ADDR:
            fprintf(stream, "\tleaq\t%s, %%rax\n", refer(op.b));
            store(stream, "rax", op.a);
            break;
        case IR_OP_ADD:
            load(stream, op.b, "rax");
            load(stream, op.c, "rbx");
            fprintf(stream, "\taddq\t%%rbx, %%rax\n");
            store(stream, "rax", op.a);
            break;
        case IR_OP_MUL:
            load(stream, op.c, "rax");
            if (op.b.kind == DIRECT)
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
        fassembleop(stream, block->code[i]);
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
fassemble(FILE *stream, const function_t *func)
{
    map_t memo;
    map_init(&memo);

    /* Print header, assume only one function. */
    fprintf(stream, "\t.text\n");
    fprintf(stream, "\t.globl\t%s\n", func->symbol->name);

    fprintf(stream, "%s:\n", func->symbol->name);
    fprintf(stream, "\tpushq\t%%rbp\n");
    fprintf(stream, "\tmovq\t%%rsp, %%rbp\n");
    fprintf(stream, "\tsubq\t$%d, %%rsp\n", func->locals_size);

    fassembleblock(stream, &memo, func->body);

    map_finalize(&memo);
}
