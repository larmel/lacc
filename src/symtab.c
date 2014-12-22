#include "lcc.h"
#include "symbol.h"

#include <stdlib.h>
#include <string.h>

/* the actual symbol table */
static symbol_t **symtab;
static int symtab_size;
static int symtab_capacity;

/* stack structure to keep track of lexical scope */
static struct lexical_scope {
    const symbol_t **symlist; /* points to symtab */
    size_t size;
    size_t cap;
} *scopes = NULL;

/* Nesting level of lexical scope, incremented for each { } block.
 * 0: Global scope
 * 1: Function arguments
 * n: Automatic variables
 */
static int depth = -1;
static int scope_cap;

/* Track offset of local variables on stack. */
int var_stack_offset;

void push_scope()
{
    depth++;
    if (depth == scope_cap) {
        scope_cap += 16;
        scopes = realloc(scopes, sizeof(struct lexical_scope) * scope_cap);
        memset(&scopes[depth], 0x0, sizeof(struct lexical_scope) * 16);
    }
    /* Reset for function params and body. */
    if (depth == 1)
        var_stack_offset = 8; /* stack pointer */
    if (depth == 2)
        var_stack_offset = 0;
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

static symbol_t *
mksymbol(const char *name, const typetree_t *type, int offset)
{
    if (symtab_size == symtab_capacity) {
        symtab_capacity += 64;
        symtab = realloc(symtab, sizeof(symbol_t*) * symtab_capacity);
    }
    /* symbol address needs to be stable, so never realloc this */
    symtab[symtab_size] = malloc(sizeof(symbol_t));
    symtab[symtab_size]->name = strdup(name);
    symtab[symtab_size]->type = type;
    symtab[symtab_size]->depth = depth;
    symtab[symtab_size]->stack_offset = offset;
    return symtab[symtab_size++];
}

static void
addsymbol(const symbol_t *symbol)
{
    struct lexical_scope *scope = &scopes[depth];
    if (scope->size == scope->cap) {
        scope->cap += 16;
        scope->symlist = realloc(scope->symlist, sizeof(symbol_t*) * scope->cap);
    }
    scope->symlist[scope->size] = symbol;
    scope->size++;
}

const symbol_t *
sym_lookup(const char *name)
{
    const symbol_t *sym;
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

const symbol_t *
sym_add(const char *name, const typetree_t *type)
{
    const symbol_t *symbol = sym_lookup(name);
    int offset = 0;
    if (symbol != NULL && symbol->depth == depth) {
        error("Duplicate definition of symbol '%s'", name);
        exit(0);
    }
    if (depth == 1) {
        var_stack_offset += type_varsize(type);
        offset = var_stack_offset;
    } else if (depth > 1) {
        var_stack_offset -= type_varsize(type);
        offset = var_stack_offset;
    }
    symbol = (const symbol_t *) mksymbol(name, type, offset);
    addsymbol(symbol);
    return symbol;
}

const symbol_t *
sym_mktemp(const typetree_t *type)
{
    static int tmpn;

    char tmpname[16];
    do {
        snprintf(tmpname, 12, "t%d", tmpn++);
    } while (sym_lookup(tmpname) != NULL);

    return sym_add(tmpname, type);
}

typetree_t *
type_init(enum tree_type type)
{
    typetree_t *tree = calloc(1, sizeof(typetree_t));
    tree->type = type;
    switch (type) {
        case CHAR_T:
            tree->size = 1;
            break;
        default:
            tree->size = 8;
    }
    return tree;
}

/* Create immediate variable based on textual value. Ex, number constant "5"
 * will get its own symbol entry, as if a variable was named "5". */
const symbol_t *
sym_mkimmediate(enum tree_type type, const char *value)
{
    const symbol_t *existing = sym_lookup(value);
    if (existing == NULL) {
        typetree_t *tree = type_init(type);
        symbol_t *symbol = mksymbol(value, tree, 0);
        symbol->is_immediate = 1;
        switch (type) {
            case CHAR_T:
                symbol->immediate.charval = *value;
                break;
            case INT64_T: /* should probably not do conversion here, no type checking */
                symbol->immediate.longval = strtol(value, NULL, 0);
                break;
            default:
                error("Unexpected immediate type");
                exit(0);
        }
        addsymbol(symbol);
        existing = symbol;
    }
    return existing;
}

const symbol_t *
sym_mkimmediate_long(long value)
{
    char str[16];
    sprintf(str, "%ld", value);
    return sym_mkimmediate(INT64_T, str);
}

int
type_equal(const typetree_t *a, const typetree_t *b)
{
    if (a == NULL && b == NULL) return 1;
    if (a == NULL || b == NULL) return 0;
    if (a->type == b->type) return 1;
    /* todo */
    return 0;
}

static void print_type(const typetree_t *);

/* Resulting type of a <op> b */
const typetree_t *
type_combine(const typetree_t *a, const typetree_t *b)
{
    if (type_equal(a, b))
        return a;

    /* Arrays decay into pointer */
    if (a->type == ARRAY) {
        typetree_t *ptr = type_init(POINTER);
        ptr->next = a->next;
        a = ptr;
    }

    if (a->type == POINTER && b->type == INT64_T)
        return a;

    error("Cannot combine types, aborting");
    print_type(a);
    puts("");
    print_type(b);
    puts("");
    exit(0);
    return NULL;
}

const typetree_t *
type_deref(const typetree_t *t)
{
    if (t->type == POINTER || t->type == ARRAY) {
        return t->next;
    }
    error("Cannot dereference non-pointer type, aborting");
    exit(0);
    return NULL;
}

/* Base size of type, i.e. sizeof(pointer) for complex types, or size of basic
 * type for int, double etc. */
size_t
type_varsize(const typetree_t *type)
{
    switch (type->type) {
        case CHAR_T:
            return 1;
        default:
            return 4; /* 32 bit */
    }
}

static void
print_type(const typetree_t *tree)
{
    int i;
    if (tree == NULL) return;
    switch (tree->type) {
        case CHAR_T:
            printf("char");
            break;
        case INT64_T:
            printf("int");
            break;
        case DOUBLE_T:
            printf("double");
            break;
        case VOID_T:
            printf("void");
            break;
        case POINTER:
            if (tree->flags) {
                if (tree->flags & CONST_Q) printf("const ");
                if (tree->flags & VOLATILE_Q) printf("volatile ");
            }
            printf("* ");
            print_type(tree->next);
            break;
        case FUNCTION:
            printf("(");
            for (i = 0; i < tree->n_args; ++i) {
                print_type(tree->args[i]);
                if (i < tree->n_args - 1)
                    printf(", ");
            }
            printf(") -> ");
            print_type(tree->next);
            break;
        case ARRAY:
            if (tree->length > 0)
                printf("[%u] ", tree->length);
            else 
                printf("[] ");
            print_type(tree->next);
            break;
        default: break;
    }
}

void
dump_symtab()
{
    int i;
    for (i = 0; i < symtab_size; ++i) {
        printf("%*s", symtab[i]->depth * 2, "");
        printf("%s :: ", symtab[i]->name);
        print_type(symtab[i]->type);
        if (symtab[i]->is_immediate) {
            switch (symtab[i]->type->type) {
                case INT64_T:
                    printf(" = %d", (int)symtab[i]->immediate.longval);
                    break;
                default:
                    printf(" = immediate");
            }
        }
        if (symtab[i]->stack_offset > 0) {
            printf(" (param: %d)", symtab[i]->stack_offset);
        }
        if (symtab[i]->stack_offset < 0) {
            printf(" (automatic: %d)", symtab[i]->stack_offset);
        }
        puts("");
    }
}
