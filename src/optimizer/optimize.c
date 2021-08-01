#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "optimize.h"
#include "liveness.h"
#include "transform.h"

#include <lacc/array.h>
#include <lacc/context.h>
#include <assert.h>

static int optimization_level;

/*
 * Serialized control flow graph. Topologically sorted if non-cyclical.
 */
static array_of(struct block *) blocklist;

/*
 * List of symbols used in the control flow graph.
 */
static array_of(struct symbol *) symbols;

/*
 * Serialize basic blocks by recursively visiting each node and
 * appending to list. Assign number to each symbol in use. Return
 * number of edges in the flow graph.
 */
static int serialize_basic_blocks(struct block *block)
{
    if (block->color == BLACK)
        return 0;

    block->color = BLACK;
    array_push_back(&blocklist, block);
    if (block->jump[0]) {
        serialize_basic_blocks(block->jump[0]);
        if (block->jump[1]) {
            serialize_basic_blocks(block->jump[1]);
        }
    }

    return 1;
}

/* Initialize liveness information in each block. */
static void initialize_dataflow(struct definition *def)
{
    int i;
    struct block *block;
    struct statement *st;

    for (i = 0; i < array_len(&blocklist); ++i) {
        block = array_get(&blocklist, i);
        block->in = 0;
        block->out = 0;
    }

    for (i = 0; i < array_len(&def->statements); ++i) {
        st = &array_get(&def->statements, i);
        st->out = 0;
    }
}

static int count_symbol(struct var v)
{
    int len;
    struct symbol *sym;

    if (!v.is_symbol
        || !is_object(v.value.symbol->type))
    {
        return 0;
    }

    sym = (struct symbol *) v.value.symbol;
    if (!sym->index) {
        len = array_len(&symbols);
        if (len < 64) {
            array_push_back(&symbols, sym);
            sym->index = len + 1;
            return 1;
        }
    }

    return 0;
}

static void reset_symbol_indexes(void)
{
    int i;
    struct symbol *sym;

    for (i = 0; i < array_len(&symbols); ++i) {
        sym = array_get(&symbols, i);
        assert(sym->index);
        sym->index = 0;
    }
}

/*
 * Assign numbers from [1 .. N] to all symbols referenced by operations
 * in the basic block.
 */
static int enumerate_used_symbols(
    struct definition *def,
    struct block *block)
{
    int i, n = 0;
    struct statement *s;

    for (i = block->head; i < block->head + block->count; ++i) {
        s = &array_get(&def->statements, i);
        assert(s->st != IR_ASM);
        switch (s->expr.op) {
        default:
            n += count_symbol(s->expr.r);
        case IR_OP_CAST:
        case IR_OP_NOT:
        case IR_OP_NEG:
        case IR_OP_CALL:
        case IR_OP_VA_ARG:
            n += count_symbol(s->expr.l);
            break;
        }

        if (s->st == IR_ASSIGN) {
            n += count_symbol(s->t);
        }
    }

    if (block->has_return_value || block->jump[1]) {
        switch (block->expr.op) {
        default:
            n += count_symbol(block->expr.r);
        case IR_OP_CAST:
        case IR_OP_NOT:
        case IR_OP_NEG:
        case IR_OP_CALL:
        case IR_OP_VA_ARG:
            n += count_symbol(block->expr.l);
            break;
        }
    }

    return n;
}

static int color_white(struct definition *def, struct block *block)
{
    block->color = WHITE;
    return 0;
}

/* Forward jumps through blocks with no instructions. */
static int skip_empty_blocks(struct definition *def, struct block *block)
{
    int i;
    struct block *next;

    for (i = 0; i < 2 && block->jump[i]; ++i) {
        do {
            next = block->jump[i];
            if (!next->count && next->jump[0] && !next->jump[1]) {
                block->jump[i] = next->jump[0];
            } else break;
        } while (1);
    }

    return 0;
}

/*
 * Traverse all reachable nodes in a graph, invoking callback on each
 * basic block.
 */
static int traverse(
    struct definition *def,
    int (*callback)(struct definition *def, struct block *))
{
    int i, n;
    struct block *block;

    for (i = 0, n = 0; i < array_len(&blocklist); ++i) {
        block = array_get(&blocklist, i);
        n += callback(def, block);
    }

    return n;
}

/*
 * Solve generic dataflow problem iteratively, going through each basic
 * block until visit function returns 0 for all nodes.
 */
static void execute_iterative_dataflow(
    struct definition *def,
    int (*callback)(struct definition *def, struct block *))
{
    int changes;

    do {
        changes = traverse(def, callback);
    } while (changes);
}

#if !NDEBUG
static void print_liveness_statement(unsigned long live)
{
    int j, k;
    const struct symbol *sym;

    printf("--- {");
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

int print_liveness(struct definition *def, struct block *block)
{
    int i;
    struct statement *st;

    printf("%s:\n", sym_name(block->label));
    print_liveness_statement(block->in);
    for (i = block->head; i < block->head + block->count; ++i) {
        st = &array_get(&def->statements, i);
        print_liveness_statement(st->out);
    }

    if (block->jump[1] || block->has_return_value) {
        print_liveness_statement(block->out);
    }

    return 0;
}
#endif

INTERNAL int is_live_after(const struct symbol *sym, const struct statement *st)
{
    if (optimization_level && is_object(sym->type)) {
        assert(sym->index);
        return (st->out & (1ul << (sym->index - 1))) != 0;
    }

    return 1;
}

INTERNAL void push_optimization(int level)
{
    optimization_level = level;
}

INTERNAL void optimize(struct definition *def)
{
    int syms, n;

    if (!optimization_level
        || !is_function(def->symbol->type)
        || array_len(&def->asm_statements))
    {
        return;
    }

    array_empty(&blocklist);
    array_empty(&symbols);
    serialize_basic_blocks(def->body);
    traverse(def, &skip_empty_blocks);
    syms = traverse(def, &enumerate_used_symbols);

    if (syms < 64) {
        initialize_dataflow(def);
        do {
            n = 0;
            execute_iterative_dataflow(def, &live_variable_analysis);

            /*traverse(&print_liveness);*/
            n += traverse(def, &dead_store_elimination);
            n += traverse(def, &merge_chained_assignment);
            /*if (n) printf("Did %d changes!\n", n);*/
        } while (n);
    }

    reset_symbol_indexes();
    traverse(def, &color_white);
}

INTERNAL void pop_optimization(void)
{
    array_clear(&blocklist);
    array_clear(&symbols);
}
