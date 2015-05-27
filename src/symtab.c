#include "error.h"
#include "type.h"
#include "symbol.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Let every namespace have a list of scopes, to optimize lookup for a
 * particular scope depth. Store indices into namespace list of symbols. */
typedef struct scope {
    int *idx;
    int size;
    int cap;
} scope_t;

void push_scope(namespace_t *ns) {
    assert(ns);

    ns->depth++;
    if (!ns->scope) {
        ns->depth = 0;
    }
    ns->scope = realloc(ns->scope, sizeof(scope_t) * (ns->depth + 1));
    memset(&ns->scope[ns->depth], 0x0, sizeof(scope_t));
}

void pop_scope(namespace_t *ns) {
    if (ns->depth >= 0) {
        if (ns->scope[ns->depth].idx) {
            free(ns->scope[ns->depth].idx);    
        }
        memset(&ns->scope[ns->depth], 0x0, sizeof(scope_t));
        ns->depth--;
    }
    if (ns->depth == -1) {
        if (ns->scope) {
            free(ns->scope);
        }
        ns->scope = NULL;
    }
}

/* Create and add symbol to symbol table, but not to any scope. Symbol address
 * needs to be stable, so they are stored as a realloc'able list of pointers. */
static int
create_symbol(namespace_t *ns, symbol_t sym)
{
    sym.depth = ns->depth;
    if (ns->size == ns->capacity) {
        ns->capacity += 64;
        ns->symbol = realloc(ns->symbol, sizeof(symbol_t*) * ns->capacity);
    }

    ns->symbol[ns->size] = calloc(1, sizeof(symbol_t));
    *(ns->symbol[ns->size]) = sym;

    return ns->size++;
}

/* Create a temporary identifier name. Use a fixed prefix '.' to all temporary 
 * variables, which will never collide with real symbols. */
static char *
prefixed_temporary_name(char prefix)
{
    static int tmpn;
    static char tmpname[16];

    snprintf(tmpname, 12, ".%c%d", prefix, tmpn++);

    return tmpname;
}

/* Add symbol to current scope, making it possible to look up. Name must be non-
 * NULL, i.e. immediate values do not belong to any scope. */
static void
register_in_scope(namespace_t *ns, int i)
{
    scope_t *scope;

    assert(i < ns->size);
    scope = &ns->scope[ns->depth];
    if (scope->size == scope->cap) {
        scope->cap += 16;
        scope->idx = realloc(scope->idx, scope->cap * sizeof(int *));
    }
    scope->idx[scope->size] = i;
    scope->size++;
}

/* Retrieve a symbol based on identifier name, or NULL of not registered or
 * visible from current scope. */
symbol_t *
sym_lookup(namespace_t *ns, const char *name)
{
    int i, d;

    assert(ns);
    for (d = ns->depth; d >= 0; --d) {
        for (i = 0; i < ns->scope[d].size; ++i) {
            int idx = ns->scope[d].idx[i];
            if (!strcmp(name, ns->symbol[idx]->name)) {
                return ns->symbol[idx];
            }
        }
    }

    return NULL;
}

void print_symbol(symbol_t *sym)
{
    printf("\t[type: %s",
        sym->symtype == SYM_DEFINITION ? "definition" :
        sym->symtype == SYM_TENTATIVE ? "tentative" :
        sym->symtype == SYM_DECLARATION ? "declaration" :
        sym->symtype == SYM_TYPEDEF ? "typedef" : "enum"
    );
    printf(", link: %s]\n",
        sym->linkage == LINK_INTERN ? "intern" :
        sym->linkage == LINK_EXTERN ? "extern" : "none"
    );
    printf("\t%s", sym->name);
    if (sym->n) {
        printf(".%d", sym->n);
    }
    printf(" :: %s\n", typetostr(sym->type));
}

/* Register symbol to current scope. */
symbol_t *
sym_add(namespace_t *ns, symbol_t sym)
{
    int idx;
    symbol_t *symbol;
    extern int VERBOSE;

    symbol = sym_lookup(ns, sym.name);

    if (symbol) {

        /* Resolve extern declaration. */
        if (sym.linkage == LINK_EXTERN && sym.symtype == SYM_DECLARATION &&
            (symbol->symtype == SYM_TENTATIVE || symbol->symtype == SYM_DEFINITION))
        {
            if (!symbol->type->size) {
                symbol->type = type_complete(symbol->type, sym.type);
            }
            return symbol;
        }

        if (symbol->depth == ns->depth && ns->depth == 0) {
            if (symbol->linkage == sym.linkage &&
                ((symbol->symtype == SYM_TENTATIVE && sym.symtype == SYM_DEFINITION) || 
                (symbol->symtype == SYM_DEFINITION && sym.symtype == SYM_TENTATIVE)))
            {
                if (!symbol->type->size) {
                    symbol->type = type_complete(symbol->type, sym.type);
                }
                symbol->symtype = SYM_DEFINITION;
            }
            else if (symbol->linkage == sym.linkage &&
                (symbol->symtype == SYM_DECLARATION && sym.symtype == SYM_TENTATIVE))
            {
                if (!symbol->type->size) {
                    symbol->type = type_complete(symbol->type, sym.type);
                }
                symbol->symtype = SYM_TENTATIVE;
            }
            else if (symbol->symtype != sym.symtype || symbol->linkage != sym.linkage)
            {
                error("Declaration of symbol '%s' does not match previous declaration.", sym.name);
                exit(1);
            }
            else
            {
                if (!symbol->type->size) {
                    symbol->type = type_complete(symbol->type, sym.type);
                }
            }
            return symbol;
        }
        else if (symbol->depth == ns->depth && ns->depth)
        {
            error("Duplicate definition of symbol '%s'", sym.name);
            exit(1);
        }

    }

    /* might not be needed. */
    sym.name = strdup(sym.name);

    /* Scoped static variable must get unique name to not collide with
     * other external declarations. */
    if (sym.linkage == LINK_INTERN && ns->depth)
    {
        static int svc;
        sym.n = ++svc;
    }

    idx = create_symbol(ns, sym);
    register_in_scope(ns, idx);

    symbol = ns->symbol[idx];

    if (VERBOSE) {
        print_symbol(symbol);
    }

    return symbol;
}

/* Add temporary (autogenerated name) symbol to current scope. */
symbol_t *
sym_temp(namespace_t *ns, const typetree_t *type)
{
    int idx;

    symbol_t sym = {0};
    sym.name = strdup( prefixed_temporary_name('t') );
    sym.type = type;

    idx = create_symbol(ns, sym);
    register_in_scope(ns, idx);

    return ns->symbol[idx];
}

/* Add temporary symbol refering to some static value. */
const symbol_t *
sym_temp_static(namespace_t *ns, const typetree_t *type)
{
    int idx;

    symbol_t sym = {0};
    sym.name = strdup( prefixed_temporary_name('d') );
    sym.type = type;

    idx = create_symbol(ns, sym);
    register_in_scope(ns, idx);

    return ns->symbol[idx];
}

/* Register compiler internal builtin symbols, that are assumed to exists by
 * standard library headers. Use dummy types for now. */
void
register_builtin_types(namespace_t *ns)
{
    symbol_t sym = {
        SYM_TYPEDEF,
        LINK_NONE,
        "__builtin_va_list"
    };
    sym.type = type_init_object();
    sym_add(ns, sym);
}

/* Output tentative definitions with external scope. Not assigned a value in
 * this translation unit, and has special representation in GNU assembler. */
void output_definitions(FILE *stream)
{
    extern namespace_t ns_ident;

    int i, found;
    symbol_t *sym;

    for (i = found = 0; i < ns_ident.size; ++i) {
        sym = ns_ident.symbol[i];
        if (sym->symtype == SYM_TENTATIVE && sym->linkage == LINK_EXTERN &&
            sym->type->type != FUNCTION) {
            if (!found) {
                fprintf(stream, "\t.data\n");
                found = 1;
            }

            fprintf(stream, "\t.comm %s, %d, %d\n",
                sym->name, sym->type->size,
                (sym->type->size < 32) ? sym->type->size : 32);
        }
    }
}

void
dump_symtab(namespace_t *ns)
{
    int i;
    char *tstr;

    if (ns->size) {
        printf("namespace %s:\n", ns->name);
    }
    for (i = 0; i < ns->size; ++i) {
        symtype_t st = ns->symbol[i]->symtype;

        printf("%*s", ns->symbol[i]->depth * 2, "");
        if (ns->symbol[i]->linkage != LINK_NONE) {
            printf("%s ",
                (ns->symbol[i]->linkage == LINK_INTERN) ? "static" : "global");
        }
        printf("%s ",
            (st == SYM_TENTATIVE) ? "tentative" : 
            (st == SYM_DEFINITION) ? "definition" :
            (st == SYM_DECLARATION) ? "declaration" :
            (st == SYM_TYPEDEF) ? "typedef" : "enum");

        printf("%s :: ", ns->symbol[i]->name);
        tstr = typetostr(ns->symbol[i]->type);
        printf("%s", tstr);
        free(tstr);

        printf(", size=%d", ns->symbol[i]->type->size);
        if (ns->symbol[i]->stack_offset < 0) {
            printf(" (stack_offset: %d)", ns->symbol[i]->stack_offset);
        }
        if (ns->symbol[i]->symtype == SYM_ENUM) {
            printf(", value=%d", ns->symbol[i]->enum_value);
        }
        printf("\n");
    }
}
