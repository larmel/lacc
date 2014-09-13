#include "lcc.h"

#include <stdlib.h>
#include <string.h>

/* the actual symbol table */
static symbol_t *symtab;
static int symtab_size;
static int symtab_capacity;

static symbol_t *
mksymbol(const char *name)
{
    if (symtab_size == symtab_capacity) {
        symtab_capacity += 64;
        symtab = realloc(symtab, sizeof(symbol_t) * symtab_capacity);
    }
    symtab[symtab_size].name = strdup(name);
    symtab[symtab_size].type = NULL;
    return &symtab[symtab_size++];
}

/* stack structure to keep track of lexical scope */
static struct lexical_scope {
    symbol_t **symlist; /* points to symtab */
    size_t size;
    size_t cap;
} *scopes = NULL;

static int depth = -1;
static int scope_cap;


symbol_t *
sym_lookup(const char *name)
{
    symbol_t *sym;
    int i, d;
    for (d = depth; d >= 0; --d) {
        for (i = 0; i < scopes[d].size; ++i) {
            sym = scopes[d].symlist[i];
            printf("Checking symbol '%s'\n", sym->name);
            if (!strcmp(name, sym->name)) {
                return sym;
            }
        }
    }
    return NULL;
}

symbol_t *
sym_add(const char *name)
{
    printf("Adding symbol '%s'\n", name);
    printf("Current scope depth is %d\n", depth);
    struct lexical_scope *scope = &scopes[depth];
    symbol_t *symbol = sym_lookup(name);
    if (symbol != NULL) {
        error("Duplicate definition of symbol '%s'", name);
        exit(0);
    }
    symbol = mksymbol(name);
    if (scope->size == scope->cap) {
        scope->cap += 16;
        scope->symlist = realloc(scope->symlist, sizeof(symbol_t*) * scope->cap);
    }
    scope->symlist[scope->size] = symbol;
    scope->size++;
    puts("Done adding symbol");
}

void push_scope()
{
    depth++;
    if (depth == scope_cap) {
        scope_cap += 16;
        scopes = realloc(scopes, sizeof(struct lexical_scope) * scope_cap);
        memset(&scopes[depth], 0x0, sizeof(struct lexical_scope) * 16);
    }
}

void pop_scope()
{
    if (depth >= 0) {
        free(scopes[depth].symlist);
        memset(&scopes[depth], 0x0, sizeof(struct lexical_scope));
        depth--;
    }
    if (depth == -1) {
        free(scopes);
        scopes = NULL;
    }
}

void dump_symtab() {
    int i;
    for (i = 0; i < symtab_size; ++i) {
        printf("%s\n", symtab[i].name);
    }
}
