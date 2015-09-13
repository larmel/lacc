#include "ir.h"
#include "symbol.h"
#include "util/memoize.h"

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
        case POINTER:
            if (var.string) {
                if (var.offset) {
                    sprintf(buffer, "$%s%s%d", var.string,
                        (var.offset > 0) ? "+" : "", var.offset);
                } else {
                    sprintf(buffer, "$%s", var.string);
                }
                break;
            }
        case INTEGER:
            sprintf(buffer, "%ld", var.value.i8);
            break;
        case ARRAY:
            sprintf(buffer, "\\\"%s\\\"", var.string);
            break;
        default:
            assert(0);
        }
        break;
    case DIRECT:
        if (var.offset) {
            sprintf(buffer, "%s[%d]", var.symbol->name, var.offset);    
        } else {
            sprintf(buffer, "%s", var.symbol->name);
        }
        break;
    case DEREF:
        if (!var.offset)
            sprintf(buffer, "*(%s)", var.symbol->name);
        else if (var.offset > 0)
            sprintf(buffer, "*(%s + %d)", var.symbol->name, var.offset);
        else
            sprintf(buffer, "*(%s - %d)", var.symbol->name, -var.offset);
        break;
    }

    return buffer;
}

static void foutputnode(
    FILE *stream,
    struct memo *memo,
    const struct block *node)
{
    if (memoize_guard(memo, node->label)) {
        int i;

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
                if (op.a.type->type == NONE) {
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
            foutputnode(stream, memo, node->jump[0]);
            foutputnode(stream, memo, node->jump[1]);
            fprintf(stream, "\t%s:s -> %s:n;\n",
                sanitize(node->label), sanitize(node->jump[0]->label));
            fprintf(stream, "\t%s:s -> %s:n;\n",
                sanitize(node->label), sanitize(node->jump[1]->label));
        } else {
            fprintf(stream, " }\"];\n");
            foutputnode(stream, memo, node->jump[0]);
            fprintf(stream, "\t%s:s -> %s:n;\n",
                sanitize(node->label), sanitize(node->jump[0]->label));
        }
    }
}

/* Take a control flow graph (struct block structure) and output it in .dot 
 * format, which can then be rendered.
 */
void fdotgen(FILE *stream, const struct decl *cfg)
{
    struct memo *memo = memoize_init();

    if (cfg->head->n && cfg->fun) {
        struct decl copy = *cfg;
        copy.fun = NULL;
        fdotgen(stream, &copy);
        copy.fun = cfg->fun;
        copy.head->n = 0;
        fdotgen(stream, &copy);
    } else {
        fprintf(stream, "digraph {\n");
        fprintf(stream,
            "\tnode [fontname=\"Courier_New\",fontsize=10,"
            "style=\"setlinewidth(0.1)\",shape=record];\n");
        fprintf(stream,
            "\tedge [fontname=\"Courier_New\",fontsize=10,"
            "style=\"setlinewidth(0.1)\"];\n");
        if (cfg->head->n) {
            foutputnode(stream, memo, cfg->head);
        } else {
            assert(cfg->fun);
            fprintf(stream, "\tlabel=\"%s\"\n", cfg->fun->name);
            fprintf(stream, "\tlabelloc=\"t\"\n");
            foutputnode(stream, memo, cfg->body);
        }
        fprintf(stream, "}\n");
    }

    memoize_free(memo);
}
