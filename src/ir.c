#include "lcc.h"

#include <stdio.h>
#include <string.h>

enum irtype {
    IR_ARITHMETIC,  /* t1 = t2 <op> t3 */
    IR_ASSIGN,      /* t1 = t2 */
    IR_RET          /* ret t1 */
};

enum iroptype {
    IR_OP_ADD,
    IR_OP_SUB,
    IR_OP_MUL,
    IR_OP_DIV,
    IR_OP_LOGICAL_AND,
    IR_OP_LOGICAL_OR,
    IR_OP_BITWISE_AND,
    IR_OP_BITWISE_OR,
    IR_OP_BITWISE_XOR
};

typedef struct irop {
    enum irtype type;
    enum iroptype optype;

    symbol_t *a;
    symbol_t *b;
    symbol_t *c;

    block_t *target;
} irop_t;


/* Hold program representation as a list of blocks (functions)
 * Must store stable pointers (no realloc), as jump targets are
 * stored in ir operations */
static block_t **blocks;
static int length;
static int cap;

static irop_t *
allocirop()
{
    block_t *block;
    if (!length)  {
        fprintf(stderr, "No block to add to!\n");
        exit(0);
    }
    block = blocks[length - 1];

    block->n++;
    block->ops = realloc(block->ops, sizeof(irop_t) * block->n);
    return &block->ops[block->n - 1];
}

/* called from parsing, generate a new block */
block_t *
mkblock(const char *label)
{
    block_t *block = malloc(sizeof(block_t));
    block->label = label;
    block->ops = NULL;
    block->n = 0;
    if (length == cap) {
        cap += 32;
        blocks = realloc(blocks, sizeof(block_t*) * cap);
    }
    blocks[length++] = block;
    return block;
}

/* add new ir operations to the current block */
void mkir_arithmetic(symbol_t *a, symbol_t *b, symbol_t *c, enum token_type type) {
    irop_t *op = allocirop();
    op->type = IR_ARITHMETIC;
    switch (type) {
        case '+':
            op->optype = IR_OP_ADD;
            break;
        case '-':
            op->optype = IR_OP_SUB;
            break;
        case '*':
            op->optype = IR_OP_MUL;
            break;
        case '/':
            op->optype = IR_OP_DIV;
            break;
        case LOGICAL_AND:
            op->optype = IR_OP_LOGICAL_AND;
            break;
        case LOGICAL_OR:
            op->optype = IR_OP_LOGICAL_OR;
            break;
        case '&':
            op->optype = IR_OP_BITWISE_AND;
            break;
        case '|':
            op->optype = IR_OP_BITWISE_OR;
            break;
        case '^':
            op->optype = IR_OP_BITWISE_XOR;
            break;
        default:
            /* nothing */
            error("Unrecognized optype, aborting");
            exit(0);
    }
    op->a = a;
    op->b = b;
    op->c = c;
}

void mkir_assign(symbol_t *a, symbol_t *b) {
    irop_t *op = allocirop();
    op->type = IR_ASSIGN;
    op->a = a;
    op->b = b;
}

void mkir_ret(symbol_t *val) {
    irop_t *op = allocirop();
    op->type = IR_RET;
    op->a = val;
}

void
compile()
{
    node_t *declaration;
    push_scope();

    while ((declaration = parse()) != NULL) {
        ;
    }

    pop_scope();
}


const char *iroptype_tostr(enum iroptype iroptype)
{
    switch (iroptype) {
        case IR_OP_ADD: return "+";
        case IR_OP_SUB: return "-";
        case IR_OP_MUL: return "*";
        case IR_OP_DIV: return "/";
        case IR_OP_LOGICAL_AND: return "&&";
        case IR_OP_LOGICAL_OR: return "||";
        case IR_OP_BITWISE_AND: return "&";
        case IR_OP_BITWISE_OR: return "|";
        case IR_OP_BITWISE_XOR: return "xor";
    }
    return "";
}

/* should do this in dot format */
void
printir(FILE *file)
{
    int i, j;
    for (i = 0; i < length; ++i) {
        fprintf(file, "%s:\n", blocks[i]->label);
        for (j = 0; j < blocks[i]->n; ++j) {
            irop_t *op = &blocks[i]->ops[j];
            switch (op->type) {
                case IR_ARITHMETIC:
                    fprintf(file, "%s = %s %s %s\n", op->a->name, op->b->name, iroptype_tostr(op->optype), op->c->name);
                    break;
                case IR_ASSIGN:
                    fprintf(file, "%s = %s\n", op->a->name, op->b->name);
                    break;
                case IR_RET:
                    fprintf(file, "ret %s\n", op->a == NULL ? "" : op->a->name);
                    break;
            }
        }
    }
}
