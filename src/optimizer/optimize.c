#include "optimize.h"
#include "liveness.h"
#include "transform.h"

#include <lacc/array.h>
#include <lacc/context.h>
#include <assert.h>

static int optimization_level;

/*
 * Store all edge nodes in one contiguous list. Blocks point into this
 * list, and owns as many elements as there are IR operations, plus
 * input and output edges.
 */
static array_of(struct dataflow) nodes;

/*
 * Serialized control flow graph. Topologically sorted if non-cyclical.
 */
static array_of(struct block *) blocks;

/*
 * List of symbols used in the control flow graph.
 */
static array_of(struct symbol *) symbols;

/*
 * Serialize basic blocks by recursively, visiting each node and
 * appending to list. Assign number to each symbol in use. Return
 * number of edges in the flow graph.
 */
static int serialize_basic_blocks(struct block *block)
{
    int r = 0;

    if (block->color != BLACK) {
        block->color = BLACK;
        array_push_back(&blocks, block);

        r = edges(block);
        if (block->jump[0]) {
            r += serialize_basic_blocks(block->jump[0]);
            if (block->jump[1]) {
                r += serialize_basic_blocks(block->jump[1]);
            }
        }
    }

    return r;
}

/*
 * Initialize array of dataflow structs, and set up pointers from each
 * basic block into array of nodes. Must be called after serialization.
 */
static void initialize_dataflow(int count)
{
    struct block *block;
    int i, n;

    assert(!array_len(&nodes));
    array_realloc(&nodes, count);
    array_zero(&nodes);

    /* Point into array, reserving size for all edges in a block. */
    for (i = 0, n = 0; i < array_len(&blocks); ++i) {
        block = array_get(&blocks, i);
        block->flow = &array_get(&nodes, n);
        n += edges(block);
    }
}

static int count_symbol(struct symbol *sym)
{
    if (sym && !sym->index && is_object(&sym->type)) {
        array_push_back(&symbols, sym);
        sym->index = array_len(&symbols);
        return 1;
    }

    return 0;
}

/*
 * Assign numbers from [1 .. N] to all symbols referenced by operations
 * in the basic block.
 */
static int enumerate_used_symbols(struct block *block)
{
    int i, n = 0;
    struct statement s;

    for (i = 0; i < array_len(&block->code); ++i) {
        s = array_get(&block->code, i);
        switch (s.expr.op) {
        default:
            n += count_symbol((struct symbol *) s.expr.r.symbol);
        case IR_OP_CAST:
        case IR_OP_NOT:
        case IR_OP_CALL:
        case IR_OP_VA_ARG:
            n += count_symbol((struct symbol *) s.expr.l.symbol);
            break;
        }

        if (s.st == IR_ASSIGN) {
            n += count_symbol((struct symbol *) s.t.symbol);
        }
    }

    if (block->has_return_value || block->jump[1]) {
        switch (block->expr.op) {
        default:
            n += count_symbol((struct symbol *) block->expr.r.symbol);
        case IR_OP_CAST:
        case IR_OP_NOT:
        case IR_OP_CALL:
        case IR_OP_VA_ARG:
            n += count_symbol((struct symbol *) block->expr.l.symbol);
            break;
        }
    }

    return n;
}

static int color_white(struct block *block)
{
    block->color = WHITE;
    return 0;
}

/*
 * Traverse all reachable nodes in a graph, invoking callback on each
 * basic block.
 */
static int traverse(int (*callback)(struct block *))
{
    int i, n = 0;
    struct block *block;

    for (i = 0; i < array_len(&blocks); ++i) {
        block = array_get(&blocks, i);
        n += callback(block);
    }

    return n;
}

/*
 * Solve generic dataflow problem iteratively, going through each basic
 * block until visit function returns 0 for all nodes.
 */
static void execute_iterative_dataflow(int (*callback)(struct block *))
{
    int changes;

    do {
        changes = traverse(callback);
    } while (changes);
}

int print_liveness(struct block *block)
{
    int i, j, k;
    const struct symbol *sym;
    unsigned long live;

    printf("%s:\n", sym_name(block->label));
    for (i = 0; i < edges(block); ++i) {
        printf("--- {");
        live = block->flow[i].live.bits;
        for (j = 0, k = 0; j < array_len(&symbols); ++j) {
            sym = array_get(&symbols, j);
            if (live & (1ul << (sym->index - 1))) {
                if (k) {
                    printf(", ");
                }
                printf("%s", sym_name(sym));
                k = 1;
            }
        }
        printf("}\n");
    }

    return 0;
}

void push_optimization(int level)
{
    optimization_level = level;
}

void optimize(struct definition *def)
{
    int iops, syms;

    if (optimization_level && is_function(&def->symbol->type)) {
        array_empty(&blocks);
        array_empty(&symbols);
        array_empty(&nodes);
        iops = serialize_basic_blocks(def->body);
        syms = traverse(&enumerate_used_symbols);

        if (syms < 64) {
            initialize_dataflow(iops);
            execute_iterative_dataflow(&live_variable_analysis);

            /*traverse(&print_liveness);*/
            traverse(&merge_chained_assignment);
        }

        traverse(&color_white);
    }
}

void pop_optimization(void)
{
    array_clear(&nodes);
    array_clear(&blocks);
    array_clear(&symbols);
}
