/* Take a control flow graph (block_t structure) and output it in .dot format,
 * which can then be rendered. */

#include "ir.h"
#include "symbol.h"
#include "util/map.h"

#include <stdlib.h>
#include <stdio.h>

static const char *
sanitize(const char *label)
{
	return (label[0] == '.') ? &label[1] : label;
}

static const char *
escape(const char *label)
{
	static char buffer[256];
	if (label[0] == '.') {
		buffer[0] = '\\';
		strncpy(buffer + 1, label, 254);
		return buffer;
	}
	return label;
}

static void
foutputnode(FILE *stream, map_t *memo, const block_t *node)
{
	int i;
	if (map_lookup(memo, node->label) != NULL)
		return;

	map_insert(memo, node->label, (void*)"done");

	fprintf(stream, "\t%s [label=\"{ %s", sanitize(node->label), escape(node->label));
	for (i = 0; i < node->n; ++i) {
		op_t op = node->code[i];
        switch (op.type) {
            case IR_ASSIGN:
                fprintf(stream, " | %s = %s", op.a->name, op.b->name);
                break;
            case IR_DEREF:
                fprintf(stream, " | %s = *%s", op.a->name, op.b->name);
                break;
            case IR_OP_ADD:
                fprintf(stream, " | %s = %s + %s", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_SUB:
                fprintf(stream, " | %s = %s - %s", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_MUL:
                fprintf(stream, " | %s = %s * %s", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_DIV:
                fprintf(stream, " | %s = %s / %s", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_MOD:
                fprintf(stream, " | %s = %s %% %s", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_LOGICAL_AND:
                fprintf(stream, " | %s = %s && %s", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_LOGICAL_OR:
                fprintf(stream, " | %s = %s || %s", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_BITWISE_AND:
                fprintf(stream, " | %s = %s & %s", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_BITWISE_OR:
                fprintf(stream, " | %s = %s | %s", op.a->name, op.b->name, op.c->name);
                break;
            case IR_OP_BITWISE_XOR:
                fprintf(stream, " | %s = %s ^ %s", op.a->name, op.b->name, op.c->name);
                break;
        }
	}
    if (node->jump[0] == NULL && node->jump[1] == NULL) {
        fprintf(stream, " | return");
        if (node->expr != NULL) {
            fprintf(stream, " %s", node->expr->name);
        }
		fprintf(stream, " }\"];\n");
    } else if (node->jump[1] != NULL) {
        fprintf(stream, " | if %s goto %s", node->expr->name, escape(node->jump[1]->label));
		fprintf(stream, " }\"];\n");
        foutputnode(stream, memo, node->jump[0]);
        foutputnode(stream, memo, node->jump[1]);
        fprintf(stream, "\t%s:s -> %s:n;\n", sanitize(node->label), sanitize(node->jump[0]->label));
        fprintf(stream, "\t%s:s -> %s:n;\n", sanitize(node->label), sanitize(node->jump[1]->label));
    } else {
		fprintf(stream, " }\"];\n");
        foutputnode(stream, memo, node->jump[0]);
        fprintf(stream, "\t%s:s -> %s:n;\n", sanitize(node->label), sanitize(node->jump[0]->label));
    }
}

void
fdotgen(FILE *stream, const block_t *cfg)
{
	map_t *map = map_init();

	fprintf(stream, "digraph {\n");
	fprintf(stream, "\tnode [fontname=\"Courier_New\",fontsize=10,style=\"setlinewidth(0.1)\",shape=record];\n");
	fprintf(stream, "\tedge [fontname=\"Courier_New\",fontsize=10,style=\"setlinewidth(0.1)\"];\n");

	foutputnode(stream, map, cfg);

	fprintf(stream, "}\n");
}
