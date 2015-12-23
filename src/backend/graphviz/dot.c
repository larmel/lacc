#include "dot.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

static const char *sanitize(const char *label)
{
    return (label[0] == '.') ? &label[1] : label;
}

static const char *escape(const char *label)
{
    static char buffer[256];

    if (label[0] == '.') {
        buffer[0] = '\\';
        strncpy(buffer + 1, label, 254);
        return buffer;
    }

    return label;
}

static char *vartostr(const struct var var)
{
    char *buffer = calloc(64, sizeof(char)); /* memory leak. */

    switch (var.kind) {
    case IMMEDIATE:
        switch (var.type->type) {
        case T_POINTER:
            assert(var.symbol && var.symbol->symtype == SYM_STRING_VALUE);
            if (var.offset) {
                sprintf(buffer, "$%s%s%d", sym_name(var.symbol),
                    (var.offset > 0) ? "+" : "", var.offset);
            } else {
                sprintf(buffer, "$%s", sym_name(var.symbol));
            }
            break;
        case T_UNSIGNED:
            sprintf(buffer, "%lu", var.imm.u);
            break;
        case T_SIGNED:
            sprintf(buffer, "%ld", var.imm.i);
            break;
        case T_ARRAY:
            assert(var.symbol && var.symbol->symtype == SYM_STRING_VALUE);
            sprintf(buffer, "\\\"%s\\\"", var.symbol->string_value);
            break;
        default:
            assert(0);
        }
        break;
    case DIRECT:
        if (var.offset) {
            sprintf(buffer, "%s[%d]", sym_name(var.symbol), var.offset);    
        } else {
            sprintf(buffer, "%s", sym_name(var.symbol));
        }
        break;
    case DEREF:
        if (!var.offset)
            sprintf(buffer, "*(%s)", sym_name(var.symbol));
        else if (var.offset > 0)
            sprintf(buffer, "*(%s + %d)", sym_name(var.symbol), var.offset);
        else
            sprintf(buffer, "*(%s - %d)", sym_name(var.symbol), -var.offset);
        break;
    }

    return buffer;
}

static void foutputnode(FILE *stream, struct block *node)
{
    int i;

    if (node->color == BLACK)
        return;

    node->color = BLACK;
    fprintf(stream, "\t%s [label=\"{ %s",
        sanitize(node->label), escape(node->label));

    for (i = 0; i < node->n; ++i) {
        struct op op = node->code[i];
        switch (op.type) {
        case IR_ASSIGN:
            fprintf(stream, " | %s = %s",
                vartostr(op.a), vartostr(op.b));
            break;
        case IR_CAST:
            fprintf(stream, " | %s = (%s) %s",
                vartostr(op.a), typetostr(op.a.type), vartostr(op.b));
            break;
        case IR_DEREF:
            fprintf(stream, " | %s = *%s",
                vartostr(op.a), vartostr(op.b));
            break;
        case IR_ADDR:
            fprintf(stream, " | %s = &%s",
                vartostr(op.a), vartostr(op.b));
            break;
        case IR_NOT:
            fprintf(stream, " | %s = ~%s",
                vartostr(op.a), vartostr(op.b));
            break;
        case IR_PARAM:
            fprintf(stream, " | param %s",
                vartostr(op.a));
            break;
        case IR_CALL:
            if (is_void(op.a.type)) {
                fprintf(stream, " | call %s",
                    vartostr(op.b));    
            } else {
                fprintf(stream, " | %s = call %s",
                    vartostr(op.a), vartostr(op.b));
            }
            break;
        case IR_OP_ADD:
            fprintf(stream, " | %s = %s + %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_SUB:
            fprintf(stream, " | %s = %s - %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_MUL:
            fprintf(stream, " | %s = %s * %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_DIV:
            fprintf(stream, " | %s = %s / %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_MOD:
            fprintf(stream, " | %s = %s %% %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_AND:
            fprintf(stream, " | %s = %s & %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_OR:
            fprintf(stream, " | %s = %s | %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_XOR:
            fprintf(stream, " | %s = %s ^ %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_SHL:
            fprintf(stream, " | %s = %s \\<\\< %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_SHR:
            fprintf(stream, " | %s = %s \\>\\> %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_EQ:
            fprintf(stream, " | %s = %s == %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_GT:
            fprintf(stream, " | %s = %s \\> %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_OP_GE:
            fprintf(stream, " | %s = %s \\>= %s",
                vartostr(op.a), vartostr(op.b), vartostr(op.c));
            break;
        case IR_VA_START:
            fprintf(stream, " | va_start(%s)", vartostr(op.a));
            break;
        case IR_VA_ARG:
            fprintf(stream, " | %s = va_arg(%s, %s)",
                vartostr(op.a), vartostr(op.b), typetostr(op.a.type));
            break;
        }
    }

    if (node->jump[0] == NULL && node->jump[1] == NULL) {
        if (node->has_return_value) {
            fprintf(stream, " | return");
            fprintf(stream, " %s", vartostr(node->expr));
        }
        fprintf(stream, " }\"];\n");
    } else if (node->jump[1] != NULL) {
        fprintf(stream, " | if %s goto %s",
            vartostr(node->expr), escape(node->jump[1]->label));
        fprintf(stream, " }\"];\n");
        foutputnode(stream, node->jump[0]);
        foutputnode(stream, node->jump[1]);
        fprintf(stream, "\t%s:s -> %s:n;\n",
            sanitize(node->label), sanitize(node->jump[0]->label));
        fprintf(stream, "\t%s:s -> %s:n;\n",
            sanitize(node->label), sanitize(node->jump[1]->label));
    } else {
        fprintf(stream, " }\"];\n");
        foutputnode(stream, node->jump[0]);
        fprintf(stream, "\t%s:s -> %s:n;\n",
            sanitize(node->label), sanitize(node->jump[0]->label));
    }
}

static void output_prefix(FILE *stream)
{
    fprintf(stream, "digraph {\n");
    fprintf(stream,
        "\tnode [fontname=\"Courier_New\",fontsize=10,"
        "style=\"setlinewidth(0.1)\",shape=record];\n");
    fprintf(stream,
        "\tedge [fontname=\"Courier_New\",fontsize=10,"
        "style=\"setlinewidth(0.1)\"];\n");
}

static void output_suffix(FILE *stream)
{
    fprintf(stream, "}\n");
}

/* Take a control flow graph (struct block structure) and output it in .dot 
 * format, which can then be rendered.
 */
void fdotgen(FILE *stream, struct cfg *cfg)
{
    if (cfg->head->n) {
        output_prefix(stream);
        foutputnode(stream, cfg->head);
        output_suffix(stream);
    }

    if (cfg->fun) {
        output_prefix(stream);
        fprintf(stream, "\tlabel=\"%s\"\n", cfg->fun->name);
        fprintf(stream, "\tlabelloc=\"t\"\n");
        foutputnode(stream, cfg->body);
        output_suffix(stream);
    }
}
