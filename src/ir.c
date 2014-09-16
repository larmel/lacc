#include "lcc.h"

#include <stdio.h>


/* IR */
enum irtype { IR_LABEL, IR_PUSH_STACKFRAME, IR_POP_STACKFRAME, IR_RETURN };

typedef struct irop {
    enum irtype type;
    struct irop *next;
    union {
        struct { 
            const char *text;
            int visibility;
        } label;

    } data;
} irop_t;

static irop_t **program;
static int length;
static int cap;

static void
append(irop_t *irop)
{
    if (length == cap) {
        cap += 64;
        program = realloc(program, sizeof(irop_t*) * cap);
    }
    program[length++] = irop;
}

static irop_t *
mkirop(enum irtype t)
{
    irop_t *op = malloc(sizeof(irop_t));
    op->type = t;
    return op;
}


static void generate_function(node_t *);

/* convert syntax tree into cfg intermediate representation */
void
codegen()
{
    node_t *declaration;
    push_scope();

    while ((declaration = parse()) != NULL) {
        if (!strcmp("function-definition", declaration->text)) {
            generate_function(declaration);
        } else {
        }
    }

    pop_scope();
}

static void
generate_function(node_t *fd)
{
    node_t *identifier = fd->children[1]->children[0]->children[0]->children[0];
    irop_t *label = mkirop(IR_LABEL);
    label->data.label.text = identifier->token.value;
    append(label);
    append(mkirop(IR_PUSH_STACKFRAME));
    append(mkirop(IR_POP_STACKFRAME));
    append(mkirop(IR_RETURN));
}


void
printir(FILE *file)
{
    int i;
    for (i = 0; i < length; ++i) {
        switch (program[i]->type) {
            case IR_LABEL:
                fprintf(file, ".%s:\n", program[i]->data.label.text);
                break;
            case IR_PUSH_STACKFRAME:
                fprintf(file, "\tpushq %%rbp\n");
                fprintf(file, "\tmovq %%rsp %%rbp\n");
                break;
            case IR_POP_STACKFRAME:
                fprintf(file, "\tpopq %%rbp\n");
                break;
            case IR_RETURN:
                fprintf(file, "\tret\n");
                break;
        }
    }
}


