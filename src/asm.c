#include "ir.h"
#include "symbol.h"
#include "util/map.h"

#include <stdio.h>


static void
load(FILE *stream, const symbol_t *sym, const char *dest)
{
    if (sym->stack_offset) {
        fprintf(stream, "\tmov\t%d(%%rbp), %%%s\n", sym->stack_offset, dest);
    } else {
        fprintf(stream, "\tmov\t$%d, %%%s\n", sym->immediate.intval, dest);
    }
}

static void
fassembleop(FILE *stream, const op_t op)
{
    switch (op.type) {
        case IR_ASSIGN:
            load(stream, op.b, "eax");
            fprintf(stream, "\tmov\t%%eax, %d(%%rbp)\n", op.a->stack_offset);
            break;
        case IR_DEREF:
            load(stream, op.b, "ebx");
            fprintf(stream, "\tmovl\t(%%ebx), %%eax\n");
            fprintf(stream, "\tmov\t%%eax, %d(%%rbp)\n", op.a->stack_offset);
            break;
        case IR_OP_ADD:
            load(stream, op.b, "eax");
            load(stream, op.c, "ebx");
            fprintf(stream, "\taddl\t%%ebx, %%eax\n");
            fprintf(stream, "\tmov\t%%eax, %d(%%rbp)\n", op.a->stack_offset);
            break;
        case IR_OP_BITWISE_AND:
            load(stream, op.b, "eax");
            load(stream, op.c, "ebx");
            fprintf(stream, "\tandl\t%%ebx, %%eax\n");
            fprintf(stream, "\tmov\t%%eax, %d(%%rbp)\n", op.a->stack_offset);
            break;
        case IR_OP_BITWISE_XOR:
            load(stream, op.b, "eax");
            load(stream, op.c, "ebx");
            fprintf(stream, "\txor\t%%ebx, %%eax\n");
            fprintf(stream, "\tmov\t%%eax, %d(%%rbp)\n", op.a->stack_offset);
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
        if (block->expr != NULL)
            load(stream, block->expr, "eax");
        fprintf(stream, "\tleave\n");
        fprintf(stream, "\tret\n");
    } else if (block->jump[1] == NULL) {
        if (map_lookup(memo, block->jump[0]->label) != NULL)
            fprintf(stream, "\tjmp\t%s\n", block->jump[0]->label);
        fassembleblock(stream, memo, block->jump[0]);
    } else {
        load(stream, block->expr, "eax");
        fprintf(stream, "\tcmp\t$0, %%eax\n");
        fprintf(stream, "\tje\t%s\n", block->jump[0]->label);
        
        if (map_lookup(memo, block->jump[1]->label) != NULL)
            fprintf(stream, "\tjmp\t%s\n", block->jump[1]->label);
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
    fprintf(stream, "\tsub\t$%d, %%rsp\n", func->locals_size);

    fassembleblock(stream, &memo, func->body);

    map_finalize(&memo);
}
