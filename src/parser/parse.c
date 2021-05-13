#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "declaration.h"
#include "expression.h"
#include "initializer.h"
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
 * Function declarations must be parsed with possibility to add symbols
 * to scope, and generate new temporary variables for VLA parameters.
 *
 * Example constructs:
 *
 *     int foo(int n, int a[][n + 1]);
 *
 *     int bar(void) {
 *        int foo(int n, int a[][n + 1]);
 *        return 0;
 *     }
 *
 * Only when the parser gets to a '{' (or old style parameters), the
 * prototype declaration is converted to a "real" declaration.
 *
 * Pure prototype declarations are recycled, invalidating symbol
 * references, and replacing VLA sizes with generic '*' length. The
 * declarations above are equivalent (and converted) to the following:
 *
 *     int foo(int n, int a[][*]);
 *
 */
static array_of(struct definition *) prototypes;

/*
 * Functions declared inline must be treated as if they were actually
 * inlined. There should be no symbol generated for inline functions if
 * they are never called. Therefore keep all inline definitions in a
 * separate list, and postpone compilation of these until the end of the
 * translation unit.
 */
static array_of(struct definition *) inline_definitions;

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

static void recycle_block(struct block *block)
{
    struct expression expr = {0};

    array_empty(&block->code);
    block->label = NULL;
    block->expr = expr;
    block->has_return_value = 0;
    block->jump[0] = block->jump[1] = NULL;
    block->color = WHITE;
    array_push_back(&blocks, block);
}

static void cfg_empty(struct definition *def)
{
    int i;
    struct symbol *sym;
    struct asm_statement *st;

    for (i = 0; i < array_len(&def->locals); ++i) {
        sym = array_get(&def->locals, i);
        if (is_temporary(sym)) {
            sym_discard(sym);
        }
    }

    for (i = 0; i < array_len(&def->labels); ++i) {
        sym = array_get(&def->labels, i);
        sym_discard(sym);
    }

    for (i = 0; i < array_len(&def->nodes); ++i) {
        recycle_block(array_get(&def->nodes, i));
    }

    for (i = 0; i < array_len(&def->asm_statements); ++i) {
        st = &array_get(&def->asm_statements, i);
        array_clear(&st->operands);
        array_clear(&st->clobbers);
        array_clear(&st->targets);
    }

    array_empty(&def->params);
    array_empty(&def->locals);
    array_empty(&def->labels);
    array_empty(&def->nodes);
    array_empty(&def->asm_statements);
}

INTERNAL struct block *cfg_block_init(struct definition *def)
{
    struct block *block;

    if (array_len(&blocks)) {
        block = array_pop_back(&blocks);
    } else {
        block = calloc(1, sizeof(*block));
    }

    if (def) {
        block->label = create_label(def);
        array_push_back(&def->nodes, block);
    } else {
        array_push_back(&expressions, block);
    }

    return block;
}

INTERNAL struct symbol *create_label(struct definition *def)
{
    struct symbol *label = sym_create_label();
    array_push_back(&def->labels, label);
    return label;
}

INTERNAL struct definition *cfg_init(void)
{
    struct definition *def;

    if (!array_len(&prototypes)) {
        def = calloc(1, sizeof(*def));
        def->body = cfg_block_init(def);
    } else {
        def = array_pop_back(&prototypes);
        cfg_empty(def);
        assert(!def->symbol);
        def->body = cfg_block_init(def);
    }

    return def;
}

INTERNAL void cfg_discard(struct definition *def)
{
    def->symbol = NULL;
    cfg_empty(def);
    array_push_back(&prototypes, def);
}

INTERNAL void cfg_define(struct definition *def, const struct symbol *sym)
{
    assert(sym->symtype == SYM_DEFINITION);
    assert(!def->symbol);

    def->symbol = sym;
    deque_push_back(&definitions, def);
}

static struct definition *pop_inline_function(void)
{
    int i;
    struct definition *def;

    for (i = 0; i < array_len(&inline_definitions); ++i) {
        def = array_get(&inline_definitions, i);
        assert(is_function(def->symbol->type));
        assert(def->symbol->inlined);
        if (def->symbol->referenced) {
            array_erase(&inline_definitions, i);
            return def;
        }
    }

    /* If no function is referenced, none can be. */
    for (i = 0; i < array_len(&inline_definitions); ++i) {
        def = array_get(&inline_definitions, i);
        cfg_discard(def);
    }

    array_empty(&inline_definitions);
    return NULL;
}

INTERNAL struct definition *parse(void)
{
    int i;
    struct block *block;
    static struct definition *def;

    /*
     * Recycle memory allocated for previous result. Parse is called
     * until no more input can be consumed.
     */
    if (def) {
        cfg_discard(def);
    }

    while (1) {
        /*
         * Parse a declaration, which can include definitions that will
         * fill up the buffer. Tentative declarations will only affect
         * the symbol table.
         */
        while (!deque_len(&definitions) && peek() != END) {
            declaration(NULL, NULL);
        }

        def = NULL;
        if (!deque_len(&definitions)) {
            break; /* no more input */
        } else {
            def = deque_pop_front(&definitions);
            if (def->symbol->inlined) {
                array_push_back(&inline_definitions, def);
            } else {
                return def;
            }
        }
    }

    assert(peek() == END);
    assert(!deque_len(&definitions));
    def = pop_inline_function();
    if (!def) {
        for (i = 0; i < array_len(&expressions); ++i) {
            block = array_get(&expressions, i);
            recycle_block(block);
        }

        array_empty(&expressions);
    }

    return def;
}

INTERNAL void parse_finalize(void)
{
    int i;
    struct definition *def;
    struct block *block;

    for (i = 0; i < array_len(&expressions); ++i) {
        block = array_get(&expressions, i);
        array_clear(&block->code);
        free(block);
    }

    for (i = 0; i < array_len(&prototypes); ++i) {
        def = array_get(&prototypes, i);
        cfg_empty(def);
        array_clear(&def->params);
        array_clear(&def->locals);
        array_clear(&def->labels);
        array_clear(&def->nodes);
        array_clear(&def->asm_statements);
        free(def);
    }

    for (i = 0; i < array_len(&blocks); ++i) {
        block = array_get(&blocks, i);
        array_clear(&block->code);
        free(block);
    }

    deque_destroy(&definitions);
    array_clear(&expressions);
    array_clear(&prototypes);
    array_clear(&inline_definitions);
    array_clear(&blocks);

    initializer_finalize();
    expression_parse_finalize();
    symtab_finalize();
}
