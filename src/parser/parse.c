#include "declaration.h"
#include "parse.h"
#include "symtab.h"

#include <assert.h>

/* Parser consumes whole declaration statements, which can include
 * multiple definitions. For example 'int foo = 1, bar = 2;'. These
 * are buffered and returned one by one on calls to parse().
 */
static struct list
    definitions;

/* A list of blocks kept for housekeeping when parsing declarations
 * that do not have a full definition object associated. For example,
 * the following constant expression would be evaluated by a dummy
 * block holding the immediate value:
 *
 *  enum { A = 1 };
 *
 */
static struct list expr_blocks;

/* Holds blocks that are allocated and free to use (not bound to any
 * definition).
 */
static struct list blocks;

static void free_block(void *elem)
{
    struct block *block = (struct block *) elem;
    array_clear(&block->code);
    free(block);
}

void cfg_block_release(void *elem)
{
    struct block *block = (struct block *) elem;
    struct var value = {0};

    array_empty(&block->code);
    block->label = NULL;
    block->expr = value;
    block->has_return_value = 0;
    block->jump[0] = block->jump[1] = NULL;
    block->color = WHITE;
    list_push(&blocks, elem);
}

struct block *cfg_block_init(void)
{
    struct definition *def;
    struct block *block;

    if (list_len(&blocks)) {
        block = list_pop(&blocks);
    } else {
        block = calloc(1, sizeof(*block));
    }

    if (list_len(&definitions)) {
        block->label = sym_create_label();

        /* Block is owned by last added definition, also when they are
         * not functions. */
        def = list_get(&definitions, list_len(&definitions) - 1);
        list_push_back(&def->nodes, block);
    } else {
        list_push_back(&expr_blocks, block);
    }

    return block;
}

struct definition *create_definition(const struct symbol *sym)
{
    struct definition *def;
    assert(sym->symtype == SYM_DEFINITION);

    def = calloc(1, sizeof(*def));
    def->symbol = sym;

    /* A bit tricky: need to add definition to list before creating
     * block, otherwise block will have wrong owner. */
    def = list_push_back(&definitions, def);
    def->body = cfg_block_init();

    return def;
}

static void free_definition(struct definition *def)
{
    list_clear(&def->params, NULL);
    list_clear(&def->locals, NULL);
    list_clear(&def->nodes, &cfg_block_release);
    free(def);
}

struct var create_var(const struct typetree *type)
{
    struct definition *def = current_func();
    struct symbol *temp = sym_create_tmp(type);
    struct var res = var_direct(temp);

    list_push_back(&def->locals, temp);
    res.lvalue = 1;
    return res;
}

struct definition *current_func(void)
{
    int i;
    struct definition *def;

    for (i = list_len(&definitions); i > 0; --i) {
        def = (struct definition *) list_get(&definitions, i - 1);
        assert(def);
        if (is_function(&def->symbol->type))
            return def;
    }

    assert(0);
    return NULL;
}

struct definition *parse(void)
{
    static struct definition *def;

    /* Clear memory allocated for previous result. Parse is called until
     * no more input can be consumed. */
    if (def) {
        free_definition(def);
    }

    /* Parse a declaration, which can include definitions that will fill
     * up the buffer. Tentative declarations will only affect the symbol
     * table. */
    while (!list_len(&definitions) && peek().token != END) {
        declaration(NULL);
    }

    /* The next definition is taken from queue. Free memory in case we
     * reach end of input. */
    def = list_pop(&definitions);
    if (peek().token == END && !def) {
        assert(!list_len(&definitions));
        list_clear(&definitions, NULL);
        list_clear(&expr_blocks, &free_block);
        list_clear(&blocks, &free_block);
    }

    return def;
}
