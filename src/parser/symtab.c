#if _XOPEN_SOURCE < 500
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 500 /* snprintf */
#endif
#include <lacc/cli.h>
#include <lacc/string.h>
#include <lacc/symbol.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct namespace
    ns_ident = {"identifiers"},
    ns_label = {"labels"},
    ns_tag = {"tags"};

/* Initialize hash table with initial size heuristic based on scope depth.
 * As a special case, depth 1 containing function arguments is assumed to
 * contain fewer symbols.
 */
static size_t hash_cap[] = {256, 16, 128, 64, 32, 16};
static size_t hash_cap_default = 8;

void push_scope(struct namespace *ns)
{
    struct scope *scope;

    ns->current_depth = (ns->scope) ? ns->current_depth + 1 : 0;
    ns->scope = realloc(ns->scope, sizeof(*ns->scope)*(ns->current_depth + 1));

    scope = &ns->scope[ns->current_depth];
    scope->hash_length = hash_cap_default;
    if (ns->current_depth < sizeof(hash_cap) / sizeof(hash_cap[0]))
        scope->hash_length = hash_cap[ns->current_depth];

    scope->hash_tab = calloc(scope->hash_length, sizeof(*scope->hash_tab));
}

static void ref_list_free(struct sym_ref *ref)
{
    if (ref->next)
        ref_list_free(ref->next);
    free(ref);
}

void pop_scope(struct namespace *ns)
{
    size_t i;
    struct scope *scope;

    assert(ns->current_depth >= 0);
    scope = &ns->scope[ns->current_depth];
    for (i = 0; i < scope->hash_length; ++i)
        if (scope->hash_tab[i].next)
            ref_list_free(scope->hash_tab[i].next);

    free(scope->hash_tab);
    ns->current_depth--;

    /* Popping last scope frees the whole symbol table. This only happens once,
     * after reaching the end of the translation unit. */
    if (ns->current_depth == -1) {
        free(ns->scope);
        if (ns->symbol) {
            for (i = 0; i < ns->length; ++i)
                free(ns->symbol[i]);
            free(ns->symbol);
        }
    }
}

/* Create and add symbol to symbol table, but not to any scope. Symbol address
 * needs to be stable, so they are stored as a realloc-safe list of pointers.
 */
static size_t create_symbol(struct namespace *ns, struct symbol arg)
{
    struct symbol *sym;

    arg.depth = ns->current_depth;
    if (ns->length == ns->capacity) {
        ns->capacity = (ns->capacity) ? ns->capacity * 2 : 128;
        ns->symbol = realloc(ns->symbol, sizeof(*ns->symbol) * ns->capacity);
    }

    sym = calloc(1, sizeof(*sym));
    *sym = arg;
    ns->symbol[ns->length] = sym;

    return ns->length++;
}

/* Add symbol to current scope hash table, making it possible to look up.
 *
 * Here we don't need to care about collisions; adding a symbol to scope will
 * always create a new entry in the hash table.
 */
static struct symbol *register_in_scope(struct namespace *ns, size_t index)
{
    struct scope *scope;
    struct symbol *sym;
    struct sym_ref *ref;
    size_t pos;
    unsigned long hash;

    assert(index < ns->length);

    scope = &ns->scope[ns->current_depth];
    sym = ns->symbol[index];
    hash = djb2_hash(sym->name);
    pos = hash % scope->hash_length;
    ref = &scope->hash_tab[pos];

    /* If direct slot is not available, allocate a new sym_ref structure and
     * hook it up last in the chain. */
    if (ref->index) {
        while (ref->next)
            ref = ref->next;
        ref->next = calloc(1, sizeof(*ref));
        ref = ref->next;
    }

    assert(!ref->index);
    assert(!ref->next);

    ref->index = index + 1;
    ref->hash = hash;
    return sym;
}

struct symbol *sym_lookup(struct namespace *ns, const char *name)
{
    struct scope *scope;
    struct symbol *sym;
    struct sym_ref *ref;
    size_t pos;
    int depth;
    unsigned long hash;

    depth = ns->current_depth;
    hash = djb2_hash(name);

    do {
        scope = &ns->scope[depth];
        pos = hash % scope->hash_length;
        ref = &scope->hash_tab[pos];

        /* Move ref until both hash value and symbol name matches, or we reach
         * end of list. */
        while (ref && ref->index) {
            if (ref->hash == hash) {
                sym = ns->symbol[ref->index - 1];
                if (!strcmp(name, sym->name))
                    return sym;
            }
            ref = ref->next;
        }
    } while (depth--);

    return NULL;
}

const char *sym_name(const struct symbol *sym)
{
    static char name[128];

    if (!sym->n)
        return sym->name;

    if (sym->name[0] == '.') {
        if (sym->name[1] == 't')
            snprintf(name, 127, "%s%d", sym->name, sym->n);
        else {
            /* String constants. */
            assert(sym->name[1] == 'L');
            return sym->name;
        }
    } else {
        snprintf(name, 127, "%s.%d", sym->name, sym->n);
    }

    return name;
}

/* Symbols can be declared multiple times, with incomplete or complete types.
 * Only functions and arrays can exist as incomplete declarations. Other symbols
 * can be re-declared, but must have identical type each time.
 *
 * For functions, the last parameter list is applied for as long as the symbol
 * is still tentative.
 */
static void apply_type(struct symbol *sym, const struct typetree *type)
{
    int conflict = 1;

    if (type_equal(&sym->type, type)
        && !(is_function(&sym->type) && sym->symtype != SYM_DEFINITION))
        return;

    switch (sym->type.type) {
    case T_FUNCTION:
        if (is_function(type) && type_equal(sym->type.next, type->next)) {
            conflict =
                sym->type.member_list &&
                nmembers(&sym->type) != nmembers(type);
            if (!conflict)
                sym->type.member_list = type->member_list;
        }
        break;
    case T_ARRAY:
        if (is_array(type) && type_equal(sym->type.next, type->next)) {
            conflict = 0;
            if (!sym->type.size) {
                assert(type->size);
                sym->type.size = type->size;
            }
        }
    default:
        break;
    }

    if (conflict) {
        error("Incompatible declaration of %s :: %t, cannot apply type '%t'.",
            sym->name, &sym->type, type);
        exit(1);
    }
}

struct symbol *sym_add(
    struct namespace *ns,
    const char *name,
    const struct typetree *type,
    enum symtype symtype,
    enum linkage linkage)
{
    struct symbol
        *sym = NULL,
        arg = {0};

    if ((sym = sym_lookup(ns, name))) {
        if (linkage == LINK_EXTERN && symtype == SYM_DECLARATION
            && (sym->symtype == SYM_TENTATIVE
                || sym->symtype == SYM_DEFINITION))
        {
            apply_type(sym, type);
            return sym;
        }
        if (sym->depth == ns->current_depth && !ns->current_depth) {
            if (sym->linkage == linkage
                && ((sym->symtype == SYM_TENTATIVE
                        && symtype == SYM_DEFINITION)
                    || (sym->symtype == SYM_DEFINITION
                        && symtype == SYM_TENTATIVE)))
            {
                apply_type(sym, type);
                sym->symtype = SYM_DEFINITION;
            } else if (
                sym->linkage == linkage
                && sym->symtype == SYM_DECLARATION
                && symtype == SYM_TENTATIVE)
            {
                apply_type(sym, type);
                sym->symtype = SYM_TENTATIVE;
            } else if (sym->symtype != symtype || sym->linkage != linkage) {
                error("Declaration of '%s' does not match prior declaration.",
                    name);
                exit(1);
            } else {
                apply_type(sym, type);
            }
            return sym;
        } else if (sym->depth == ns->current_depth && ns->current_depth) {
            error("Duplicate definition of symbol '%s'", name);
            exit(1);
        }
    }

    arg.name = name;
    arg.type = *type;
    arg.symtype = symtype;
    arg.linkage = linkage;

    /* Scoped static variable must get unique name in order to not collide with
     * other external declarations. */
    if (linkage == LINK_INTERN && ns->current_depth) {
        static int counter;
        arg.n = ++counter;
    }

    sym = register_in_scope(ns, create_symbol(ns, arg));
    verbose(
        "\t[type: %s, link: %s]\n"
        "\t%s :: %t",
        (sym->symtype == SYM_DEFINITION ? "definition" :
            sym->symtype == SYM_TENTATIVE ? "tentative" :
            sym->symtype == SYM_DECLARATION ? "declaration" :
            sym->symtype == SYM_TYPEDEF ? "typedef" : "enum"),
        (sym->linkage == LINK_INTERN ? "intern" :
            sym->linkage == LINK_EXTERN ? "extern" : "none"),
        sym_name(sym),
        &sym->type);

    return sym;
}

struct symbol *sym_temp(struct namespace *ns, const struct typetree *type)
{
    /* Count number of temporary variables, giving each new one a unique name
     * by setting the counter instead of creating a string. */
    static int n;

    struct symbol sym = {0};

    sym.symtype = SYM_DEFINITION;
    sym.linkage = LINK_NONE;
    sym.name = ".t";
    sym.n = n++;
    sym.type = *type;
    return register_in_scope(ns, create_symbol(ns, sym));
}

void register_builtin_types(struct namespace *ns)
{
    struct typetree *type = type_init(T_STRUCT);
    const struct typetree *none = &basic_type__void;

    type_add_member(type, "gp_offset", &basic_type__unsigned_int);
    type_add_member(type, "fp_offset", &basic_type__unsigned_int);
    type_add_member(type, "overflow_arg_area", type_init(T_POINTER, none));
    type_add_member(type, "reg_save_area", type_init(T_POINTER, none));
    type = type_init(T_ARRAY, type, 1);

    /* Define va_list as described in System V ABI. */
    sym_add(ns, "__builtin_va_list", type, SYM_TYPEDEF, LINK_NONE);

    /* Add symbols with dummy types just to reserve them, and make them resolve
     * during parsing. These are implemented as compiler intrinsics. */
    sym_add(ns, "__builtin_va_start", none, SYM_DECLARATION, LINK_NONE);
    sym_add(ns, "__builtin_va_arg", none, SYM_DECLARATION, LINK_NONE);
}

struct symbol_list get_tentative_definitions(const struct namespace *ns)
{
    struct symbol_list list = {0};
    struct symbol *sym;
    int i;

    for (i = 0; i < ns->length; ++i) {
        sym = ns->symbol[i];
        if (sym->symtype == SYM_TENTATIVE) {
            list.length += 1;
            list.symbol =
                realloc(list.symbol, list.length * sizeof(*list.symbol));
            list.symbol[list.length - 1] = sym;
        }
    }

    list.capacity = list.length;
    return list;
}

void output_symbols(FILE *stream, struct namespace *ns)
{
    size_t i;
    enum symtype st;
    char *tstr;

    if (ns->length)
        verbose("namespace %s:", ns->name);

    for (i = 0; i < ns->length; ++i) {
        st = ns->symbol[i]->symtype;
        fprintf(stream, "%*s", ns->symbol[i]->depth * 2, "");
        if (ns->symbol[i]->linkage != LINK_NONE) {
            fprintf(stream, "%s ",
                (ns->symbol[i]->linkage == LINK_INTERN) ? "static" : "global");
        }

        fprintf(stream, "%s ",
            (st == SYM_TENTATIVE) ? "tentative" : 
            (st == SYM_DEFINITION) ? "definition" :
            (st == SYM_DECLARATION) ? "declaration" :
            (st == SYM_TYPEDEF) ? "typedef" : "enum");

        fprintf(stream, "%s :: ", sym_name(ns->symbol[i]));
        tstr = typetostr(&ns->symbol[i]->type);
        fprintf(stream, "%s", tstr);
        free(tstr);

        fprintf(stream, ", size=%d", size_of(&ns->symbol[i]->type));
        if (ns->symbol[i]->stack_offset)
            fprintf(stream, " (stack_offset: %d)", ns->symbol[i]->stack_offset);

        if (ns->symbol[i]->symtype == SYM_ENUM_VALUE)
            fprintf(stream, ", value=%d", ns->symbol[i]->enum_value);

        fprintf(stream, "\n");
    }
}
