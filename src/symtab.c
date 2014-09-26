#include "lcc.h"

#include <stdlib.h>
#include <string.h>

/* the actual symbol table */
static symbol_t *symtab;
static int symtab_size;
static int symtab_capacity;

/* stack structure to keep track of lexical scope */
static struct lexical_scope {
    symbol_t **symlist; /* points to symtab */
    size_t size;
    size_t cap;
} *scopes = NULL;

static int depth = -1;
static int scope_cap;

static symbol_t *
mksymbol(const char *name, typetree_t *type)
{
    if (symtab_size == symtab_capacity) {
        symtab_capacity += 64;
        symtab = realloc(symtab, sizeof(symbol_t) * symtab_capacity);
    }
    symtab[symtab_size].name = strdup(name);
    symtab[symtab_size].type = type;
    symtab[symtab_size].depth = depth;
    return &symtab[symtab_size++];
}

symbol_t *
sym_lookup(const char *name)
{
    symbol_t *sym;
    int i, d;
    for (d = depth; d >= 0; --d) {
        for (i = 0; i < scopes[d].size; ++i) {
            sym = scopes[d].symlist[i];
            if (!strcmp(name, sym->name)) {
                return sym;
            }
        }
    }
    return NULL;
}

void
sym_add(const char *name, typetree_t *type)
{
    struct lexical_scope *scope = &scopes[depth];
    symbol_t *symbol = sym_lookup(name);
    if (symbol != NULL && symbol->depth == depth) {
        error("Duplicate definition of symbol '%s'", name);
        exit(0);
    }
    symbol = mksymbol(name, type);
    if (scope->size == scope->cap) {
        scope->cap += 16;
        scope->symlist = realloc(scope->symlist, sizeof(symbol_t*) * scope->cap);
    }
    scope->symlist[scope->size] = symbol;
    scope->size++;
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

static void
print_type(typetree_t *tree)
{
    int i;
    if (tree == NULL) return;
    switch (tree->type) {
        case BASIC:
            switch (tree->data.basic.qualifier) {
                case CONST_Q:
                    printf("const ");
                    break;
                case VOLATILE_Q:
                    printf("volatile ");
                    break;
                default: break;
            }
            switch (tree->data.basic.type) {
                case CHAR_T:
                    printf("char");
                    break;
                case INT64_T:
                    printf("int64");
                    break;
                case DOUBLE_T:
                    printf("double");
                    break;
                case VOID_T:
                    printf("void");
                    break;
                default: break;
            }
            break;
        case POINTER:
            if (tree->data.ptr.qualifier != NONE_Q) {
                if (tree->data.ptr.qualifier & CONST_Q) printf("const ");
                if (tree->data.ptr.qualifier & VOLATILE_Q) printf("volatile ");
            }
            printf("* ");
            print_type(tree->data.ptr.to);
            break;
        case FUNCTION:
            printf("(");
            for (i = 0; i < tree->data.func.n_args; ++i) {
                print_type(tree->data.func.args[i]);
                if (i < tree->data.func.n_args - 1)
                    printf(", ");
            }
            printf(") -> ");
            print_type(tree->data.func.ret);
            break;
        default: break;
    }
}

void
dump_symtab()
{
    int i;
    for (i = 0; i < symtab_size; ++i) {
        printf("%*s", symtab[i].depth * 2, "");
        printf("%s :: ", symtab[i].name);
        print_type(symtab[i].type);
        puts("");
    }
}
