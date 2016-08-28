#include "declaration.h"
#include "parse.h"
#include "symtab.h"
#include <lacc/deque.h>

#include <assert.h>

/*
 * Parser consumes whole declaration statements, which can include
 * multiple definitions. For example 'int foo = 1, bar = 2;'. These
 * are buffered and returned one by one on calls to parse().
 */
static deque_of(struct definition *) definitions;

/*
 * A list of blocks kept for housekeeping when parsing declarations
 * that do not have a full definition object associated. For example,
 * the following constant expression would be evaluated by a dummy
 * block holding the immediate value:
 *
 *  enum { A = 1 };
 *
 */
static array_of(struct block *) expressions;

/*
 * Holds blocks that are allocated and free to use (not bound to any
 * definition).
 */
static array_of(struct block *) blocks;

/* Free memory in static buffers. */
static void cleanup(void)
{
    int i;
    struct block *block;

    for (i = 0; i < array_len(&expressions); ++i) {
        block = array_get(&expressions, i);
        array_clear(&block->code);
        free(block);
    }
    for (i = 0; i < array_len(&blocks); ++i) {
        block = array_get(&blocks, i);
        array_clear(&block->code);
        free(block);
    }

    deque_destroy(&definitions);
    array_clear(&expressions);
    array_clear(&blocks);
}

static void cfg_block_release(struct block *block)
{
    struct var value = {0};

    array_empty(&block->code);
    block->label = NULL;
    block->expr = value;
    block->has_return_value = 0;
    block->jump[0] = block->jump[1] = NULL;
    block->color = WHITE;
    array_push_back(&blocks, block);
}

static void cfg_clear(struct definition *def)
{
    int i;
    struct symbol *sym;

    array_clear(&def->params);
    for (i = 0; i < array_len(&def->locals); ++i) {
        sym = array_get(&def->locals, i);
        if (!str_cmp(sym->name, str_init(".t"))) {
            sym_release_temporary(sym);
        }
    }

    array_clear(&def->locals);
    for (i = 0; i < array_len(&def->nodes); ++i) {
        cfg_block_release(array_get(&def->nodes, i));
    }

    array_clear(&def->nodes);
    free(def);
}

struct block *cfg_block_init(struct definition *def)
{
    struct block *block;

    if (array_len(&blocks)) {
        block = array_pop_back(&blocks);
    } else {
        block = calloc(1, sizeof(*block));
    }

    if (def) {
        block->label = sym_create_label();
        array_push_back(&def->nodes, block);
    } else {
        array_push_back(&expressions, block);
    }

    return block;
}

struct definition *cfg_init(const struct symbol *sym)
{
    struct definition *def;
    assert(sym->symtype == SYM_DEFINITION);

    def = calloc(1, sizeof(*def));
    def->symbol = sym;
    deque_push_back(&definitions, def);
    def->body = cfg_block_init(def);

    return def;
}

struct definition *parse(void)
{
    static struct definition *def;

    /*
     * Clear memory allocated for previous result. Parse is called until
     * no more input can be consumed.
     */
    if (def) {
        cfg_clear(def);
    }

    /*
     * Parse a declaration, which can include definitions that will fill
     * up the buffer. Tentative declarations will only affect the symbol
     * table.
     */
    while (!deque_len(&definitions) && peek().token != END) {
        declaration(NULL, NULL);
    }

    /*
     * The next definition is taken from queue. Free memory in case we
     * reach end of input.
     */
    if (!deque_len(&definitions)) {
        assert(peek().token == END);
        cleanup();
        def = NULL;
    } else {
        def = deque_pop_front(&definitions);
    }

    return def;
}
