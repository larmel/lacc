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

/*
 * Assign numbers from [1 .. N] to all symbols referenced by operations
 * in the basic block.
 */
static int enumerate_used_symbols(struct block *block)
{
    int i, j, k, n = 0;
    struct symbol *sym[3];
    struct op op;

    for (i = 0; i < array_len(&block->code); ++i) {
        op = array_get(&block->code, i);
        j = OPERAND_COUNT(op.type);
        sym[0] = (struct symbol *) op.a.symbol;
        if (j > 1) {
            sym[1] = (struct symbol *) op.b.symbol;
            if (j > 2) {
                sym[2] = (struct symbol *) op.c.symbol;
            }
        }

        for (k = 0; k < j; ++k) {
            if (sym[k] && !sym[k]->index && is_object(&sym[k]->type)) {
                array_push_back(&symbols, sym[k]);
                sym[k]->index = array_len(&symbols);
                n++;
            }
        }
    }

    if (block->has_return_value || block->jump[1]) {
        sym[0] = (struct symbol *) block->expr.symbol;
        if (sym[0] && !sym[0]->index) {
            array_push_back(&symbols, sym[0]);
            sym[0]->index = array_len(&symbols);
            n++;
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
