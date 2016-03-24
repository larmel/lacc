#if _XOPEN_SOURCE < 500
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 500 /* snprintf */
#endif
#include "symtab.h"
#include "type.h"
#include <lacc/cli.h>
#include <lacc/hash.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct namespace
    ns_ident = {"identifiers"},
    ns_label = {"labels"},
    ns_tag = {"tags"};

const struct symbol
    *decl_memcpy = NULL;

static struct string sym_hash_key(void *ref)
{
    return str_init(((const struct symbol *) ref)->name);
}

void push_scope(struct namespace *ns)
{
    /* Initialize hash table with initial size heuristic based on scope
     * depth. As a special case, depth 1 containing function arguments
     * is assumed to contain fewer symbols.
     */
    static unsigned hash_cap[] = {256, 16, 128, 64, 32, 16};
    static unsigned hash_cap_default = 8;

    struct hash_table *scope;
    unsigned cap;

    ns->current_depth = (ns->scope) ? ns->current_depth + 1 : 0;
    ns->scope = realloc(ns->scope, sizeof(*scope) * (ns->current_depth + 1));

    cap = hash_cap_default;
    if (ns->current_depth < sizeof(hash_cap) / sizeof(hash_cap[0]))
        cap = hash_cap[ns->current_depth];

    scope = &ns->scope[ns->current_depth];
    hash_init(scope, cap, &sym_hash_key, NULL, NULL);
}

void pop_scope(struct namespace *ns)
{
    struct hash_table *scope;
    assert(ns->current_depth >= 0);

    scope = &ns->scope[ns->current_depth--];
    hash_destroy(scope);

    /* Popping last scope frees the whole symbol table, including the
     * symbols themselves. This only happens once, after reaching the
     * end of the translation unit. */
    if (ns->current_depth == -1) {
        free(ns->scope);
        list_clear(&ns->symbol_list, &free);
    }
}

struct symbol *sym_lookup(struct namespace *ns, const char *name)
{
    int depth;
    struct hash_table *scope;
    struct string key;
    struct symbol *sym;
    assert(ns->current_depth >= 0);

    key = str_init(name);
    depth = ns->current_depth;
    do {
        scope = &ns->scope[depth];
        sym = hash_lookup(scope, key);
        if (sym) {
            sym->referenced += 1;
            return sym;
        }
    } while (depth--);

    return NULL;
}

const char *sym_name(const struct symbol *sym)
{
    static char name[128];

    if (!sym->n)
        return sym->name;

    /* Temporary variables and string literals are named '.t' and '.LC',
     * respectively. For those, append the numeral without anything in
     * between. For other variables, which are disambiguated statics,
     * insert a period between the name and the number. */
    if (sym->name[0] == '.')
        snprintf(name, sizeof(name), "%s%d", sym->name, sym->n);
    else
        snprintf(name, sizeof(name), "%s.%d", sym->name, sym->n);

    return name;
}

/* Symbols can be declared multiple times, with incomplete or complete
 * types. Only functions and arrays can exist as incomplete. Other
 * symbols can be re-declared, but must have identical type each time.
 *
 * For functions, the last parameter list is applied for as long as the
 * symbol is still tentative.
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
    struct symbol *sym;
    assert(symtype != SYM_LABEL);

    /* Look up and try to complete existing tentative definition. */
    if (symtype != SYM_STRING_VALUE && (sym = sym_lookup(ns, name))) {
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
            error("Duplicate definition of symbol '%s'.", name);
            exit(1);
        }
    }

    /* Create new symbol. */
    sym = calloc(1, sizeof(*sym));
    sym->depth = ns->current_depth;
    sym->name = name;
    sym->type = *type;
    sym->symtype = symtype;
    sym->linkage = linkage;

    /* Scoped static variable must get unique name in order to not
     * collide with other external declarations. */
    if (linkage == LINK_INTERN && (sym->depth || symtype == SYM_STRING_VALUE)) {
        static int n;
        sym->n = ++n;
    }

    /* Add to normal identifier namespace, and make it searchable
     * through current scope. */
    list_push_back(&ns->symbol_list, sym);
    hash_insert(&ns->scope[ns->current_depth], (void *) sym);

    verbose(
        "\t[type: %s, link: %s]\n"
        "\t%s :: %t",
        (sym->symtype == SYM_DEFINITION ? "definition" :
            sym->symtype == SYM_TENTATIVE ? "tentative" :
            sym->symtype == SYM_DECLARATION ? "declaration" :
            sym->symtype == SYM_TYPEDEF ? "typedef" :
            sym->symtype == SYM_ENUM_VALUE ? "enum" : "string"),
        (sym->linkage == LINK_INTERN ? "intern" :
            sym->linkage == LINK_EXTERN ? "extern" : "none"),
        sym_name(sym),
        &sym->type);

    return sym;
}

struct symbol *sym_create_tmp(const struct typetree *type)
{
    /* Count number of temporary variables, giving each new one a unique
     * name by setting the counter instead of creating a string. */
    static int n;

    struct symbol *sym = calloc(1, sizeof(*sym));
    sym->symtype = SYM_DEFINITION;
    sym->linkage = LINK_NONE;
    sym->name = ".t";
    sym->n = ++n;
    sym->type = *type;

    /* Add temporary to normal identifier namespace, but do not make it
     * searchable through any scope. */
    list_push_back(&ns_ident.symbol_list, sym);
    return sym;
}

struct symbol *sym_create_label(void)
{
    struct symbol *sym = calloc(1, sizeof(*sym));
    sym->type = basic_type__void;
    sym->symtype = SYM_LABEL;
    sym->linkage = LINK_INTERN;
    sym->name = ".L";
    sym->n = list_len(&ns_label.symbol_list) + 1;

    /* Construct symbol in label namespace, but do not add it to any
     * scope. No need or use for searching in labels. */
    list_push_back(&ns_label.symbol_list, sym);
    return sym;
}

void register_builtin_types(struct namespace *ns)
{
    struct typetree *type;
    const struct typetree
        *none = &basic_type__void,
        *voidptr = type_init(T_POINTER, &basic_type__void),
        *constvoidptr = type_init(T_POINTER, &basic_type__const_void);

    type = type_init(T_STRUCT);
    type_add_member(type, "gp_offset", &basic_type__unsigned_int);
    type_add_member(type, "fp_offset", &basic_type__unsigned_int);
    type_add_member(type, "overflow_arg_area", voidptr);
    type_add_member(type, "reg_save_area", voidptr);
    type = type_init(T_ARRAY, type, 1);

    /* Define va_list as described in System V ABI. */
    sym_add(ns, "__builtin_va_list", type, SYM_TYPEDEF, LINK_NONE);

    /* Add symbols with dummy types just to reserve them, and make them
     * resolve during parsing. These are implemented as compiler
     * intrinsics. */
    sym_add(ns, "__builtin_va_start", none, SYM_DECLARATION, LINK_NONE);
    sym_add(ns, "__builtin_va_arg", none, SYM_DECLARATION, LINK_NONE);

    type = type_init(T_FUNCTION);
    type->next = voidptr;
    type_add_member(type, "dest", voidptr);
    type_add_member(type, "src", constvoidptr);
    type_add_member(type, "n", &basic_type__unsigned_long);
    decl_memcpy = sym_add(ns, "memcpy", type, SYM_DECLARATION, LINK_EXTERN);
}

const struct symbol *yield_declaration(struct namespace *ns)
{
    const struct symbol *sym;

    while (ns->cursor < list_len(&ns->symbol_list)) {
        sym = (const struct symbol *) list_get(&ns->symbol_list, ns->cursor);
        ns->cursor++;
        if (sym->symtype == SYM_TENTATIVE ||
            sym->symtype == SYM_STRING_VALUE ||
            (sym->symtype == SYM_DECLARATION &&
                sym->linkage == LINK_EXTERN &&
                (sym->referenced || sym == decl_memcpy)))
        {
            return sym;
        }
    }

    return NULL;
}

void output_symbols(FILE *stream, struct namespace *ns)
{
    unsigned i;
    struct symbol *sym;
    char *tstr;

    for (i = 0; i < list_len(&ns->symbol_list); ++i) {
        if (!i) {
            verbose("namespace %s:", ns->name);
        }
        sym = (struct symbol *) list_get(&ns->symbol_list, i);
        fprintf(stream, "%*s", sym->depth * 2, "");
        if (sym->linkage != LINK_NONE) {
            fprintf(stream, "%s ",
                (sym->linkage == LINK_INTERN) ? "static" : "global");
        }

        fprintf(stream, "%s ",
            (sym->symtype == SYM_TENTATIVE) ? "tentative" : 
            (sym->symtype == SYM_DEFINITION) ? "definition" :
            (sym->symtype == SYM_DECLARATION) ? "declaration" :
            (sym->symtype == SYM_TYPEDEF) ? "typedef" :
            (sym->symtype == SYM_ENUM_VALUE) ? "enum" :
            (sym->symtype == SYM_STRING_VALUE) ? "string" : "label");

        fprintf(stream, "%s :: ", sym_name(sym));
        tstr = typetostr(&sym->type);
        fprintf(stream, "%s", tstr);
        free(tstr);

        fprintf(stream, ", size=%d", size_of(&sym->type));
        if (sym->stack_offset)
            fprintf(stream, " (stack_offset: %d)", sym->stack_offset);

        if (sym->symtype == SYM_ENUM_VALUE)
            fprintf(stream, ", value=%d", sym->enum_value);

        fprintf(stream, "\n");
    }
}
