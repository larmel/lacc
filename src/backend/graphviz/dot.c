#include "dot.h"

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define MAX_BUF_LEN 256
#define BUFFERS 4

static char buffer[BUFFERS][MAX_BUF_LEN];

/*
 * Return a buffer which can be used to write string representation of
 * variable or symbol. Need to keep at most four alive in order to print
 * a full statement. Circulate using counter.
 */
static char *get_buffer(void)
{
    static int i;

    i = (i + 1) % BUFFERS;
    return buffer[i];
}

static const char *sanitize(const struct symbol *sym)
{
    const char *label;
    char *buffer;
    assert(sym->symtype == SYM_LABEL);

    label = sym_name(sym);
    buffer = get_buffer();
    strncpy(
        buffer,
        (label[0] == '.') ? label + 1 : label,
        MAX_BUF_LEN);
    return buffer;
}

static const char *escape(const struct symbol *sym)
{
    const char *label;
    char *buffer;
    assert(sym->symtype == SYM_LABEL);

    label = sym_name(sym);
    buffer = get_buffer();
    if (label[0] == '.') {
        buffer[0] = '\\';
        strncpy(buffer + 1, label, MAX_BUF_LEN - 1);
    } else {
        strncpy(buffer, label, MAX_BUF_LEN);
    }

    return buffer;
}

static char *vartostr(const struct var var)
{
    char *buffer = get_buffer();

    switch (var.kind) {
    case IMMEDIATE:
        switch (var.type->type) {
        case T_POINTER:
            if (var.symbol) {
                assert(var.symbol->symtype == SYM_STRING_VALUE);
                if (var.offset) {
                    sprintf(buffer, "$%s%s%d", sym_name(var.symbol),
                        (var.offset > 0) ? "+" : "", var.offset);
                } else {
                    sprintf(buffer, "$%s", sym_name(var.symbol));
                }
                break;
            }
        case T_UNSIGNED:
            sprintf(buffer, "%lu", var.imm.u);
            break;
        case T_SIGNED:
            sprintf(buffer, "%ld", var.imm.i);
            break;
        case T_REAL:
            if (is_float(var.type)) {
                sprintf(buffer, "%ff", var.imm.f);
            } else {
                sprintf(buffer, "%f", var.imm.d);
            }
            break;
        case T_ARRAY:
            assert(var.symbol && var.symbol->symtype == SYM_STRING_VALUE);
            sprintf(buffer, "\\\"%s\\\"", str_raw(var.symbol->string_value));
            break;
        default:
            assert(0);
        }
        break;
    case DIRECT:
        if (var.offset > 0) {
            sprintf(buffer, "*(&%s + %d)", sym_name(var.symbol), var.offset);
        } else if (var.offset < 0) {
            sprintf(buffer, "*(&%s - %d)", sym_name(var.symbol), -var.offset);
        } else {
            if (is_field(var)) {
                sprintf(buffer, "%s:%d", sym_name(var.symbol), var.width);
            } else {
                sprintf(buffer, "%s", sym_name(var.symbol));
            }
        }
        break;
    case ADDRESS:
        if (var.offset > 0) {
            sprintf(buffer, "(&%s + %d)", sym_name(var.symbol), var.offset);
        } else if (var.offset < 0) {
            sprintf(buffer, "(&%s - %d)", sym_name(var.symbol), -var.offset);
        } else {
            sprintf(buffer, "&%s", sym_name(var.symbol));
        }
        break;
    case DEREF:
        if (var.offset > 0) {
            sprintf(buffer, "*(%s + %d)", sym_name(var.symbol), var.offset);
        } else if (var.offset < 0) {
            sprintf(buffer, "*(%s - %d)", sym_name(var.symbol), -var.offset);
        } else {
            sprintf(buffer, "*%s", sym_name(var.symbol));
        }
        break;
    }

    return buffer;
}

static char *exprtostr(struct expression expr)
{
    char *buffer;

    buffer = get_buffer();
    switch (expr.op) {
    case IR_OP_CAST:
        if (type_equal(expr.type, expr.l.type)) {
            sprintf(buffer, "%s", vartostr(expr.l));
        } else {
            sprintf(buffer, "(%s) %s", typetostr(expr.type), vartostr(expr.l));
        }
        break;
    case IR_OP_CALL:
        sprintf(buffer, "call %s", vartostr(expr.l));
        break;
    case IR_OP_VA_ARG:
        sprintf(buffer, "va_arg(%s, %s)",
            vartostr(expr.l), typetostr(expr.type));
        break;
    case IR_OP_NOT:
        sprintf(buffer, "~%s", vartostr(expr.l));
        break;
    case IR_OP_ADD:
        sprintf(buffer, "%s + %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_SUB:
        sprintf(buffer, "%s - %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_MUL:
        sprintf(buffer, "%s * %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_DIV:
        sprintf(buffer, "%s / %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_MOD:
        sprintf(buffer, "%s %% %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_AND:
        sprintf(buffer, "%s & %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_OR:
        sprintf(buffer, "%s | %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_XOR:
        sprintf(buffer, "%s ^ %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_SHL:
        sprintf(buffer, "%s \\<\\< %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_SHR:
        sprintf(buffer, "%s \\>\\> %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_EQ:
        sprintf(buffer, "%s == %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_NE:
        sprintf(buffer, "%s != %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_GE:
        sprintf(buffer, "%s \\>= %s", vartostr(expr.l), vartostr(expr.r));
        break;
    case IR_OP_GT:
        sprintf(buffer, "%s \\> %s", vartostr(expr.l), vartostr(expr.r));
        break;
    }

    return buffer;
}

static void foutputnode(FILE *stream, struct block *node)
{
    int i;
    struct statement s;

    if (node->color == BLACK)
        return;

    node->color = BLACK;
    fprintf(stream, "\t%s [label=\"{ %s",
        sanitize(node->label), escape(node->label));

    for (i = 0; i < array_len(&node->code); ++i) {
        s = array_get(&node->code, i);
        switch (s.st) {
        case IR_ASSIGN:
            fprintf(stream, " | %s = %s", vartostr(s.t), exprtostr(s.expr));
            break;
        case IR_PARAM:
            fprintf(stream, " | param %s", exprtostr(s.expr));
            break;
        case IR_VA_START:
            fprintf(stream, " | va_start(%s)", exprtostr(s.expr));
            break;
        case IR_EXPR:
            fprintf(stream, " | %s", exprtostr(s.expr));
            break;
        }
    }

    if (node->jump[0] == NULL && node->jump[1] == NULL) {
        if (node->has_return_value) {
            fprintf(stream, " | return");
            fprintf(stream, " %s", exprtostr(node->expr));
        }
        fprintf(stream, " }\"];\n");
    } else if (node->jump[1] != NULL) {
        fprintf(stream, " | if %s goto %s",
            exprtostr(node->expr), escape(node->jump[1]->label));
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

void fdotgen(FILE *stream, struct definition *def)
{
    fprintf(stream, "digraph {\n");
    fprintf(stream, "\tnode [fontname=\"Courier_New\",fontsize=10,"
                    "style=\"setlinewidth(0.1)\",shape=record];\n");
    fprintf(stream, "\tedge [fontname=\"Courier_New\",fontsize=10,"
                    "style=\"setlinewidth(0.1)\"];\n");
    if (is_function(&def->symbol->type)) {
        fprintf(stream, "\tlabel=\"%s\"\n", sym_name(def->symbol));
        fprintf(stream, "\tlabelloc=\"t\"\n");
    }

    foutputnode(stream, def->body);
    fprintf(stream, "}\n");
}
