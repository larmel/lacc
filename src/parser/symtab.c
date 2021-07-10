#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "symtab.h"
#include "typetree.h"
#include <lacc/context.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

INTERNAL struct namespace
    ns_ident = {0},
    ns_label = {0},
    ns_tag = {0};

/* Name prefixes assigned to compiler generated symbols. */
static String
    prefix_temporary = SHORT_STRING_INIT(".t"),
    prefix_unnamed = SHORT_STRING_INIT(".u"),
    prefix_constant = SHORT_STRING_INIT(".C"),
    prefix_string = SHORT_STRING_INIT(".LC"),
    prefix_label = SHORT_STRING_INIT(".L");

/*
 * Maintain list of symbols allocated for temporaries and labels, which
 * can be reused between function definitions.
 *
 * Calling sym_discard will push symbols back into this list.
 */
static SymbolArray temporaries;

static struct symbol *alloc_sym(void)
{
    struct symbol *sym;

    if (array_len(&temporaries)) {
        sym = array_pop_back(&temporaries);
        memset(sym, 0, sizeof(*sym));
    } else {
        sym = calloc(1, sizeof(*sym));
    }

    return sym;
}

/*
 * Cache string types, to avoid creating new type for every string
 * literal.
 */
struct string_type_handle {
    size_t length;
    Type type;
};

static array_of(struct string_type_handle) string_types;

static Type get_string_type(size_t len)
{
    int i;
    struct string_type_handle handle;

    for (i = 0; i < array_len(&string_types); ++i) {
        handle = array_get(&string_types, i);
        if (handle.length == len) {
            return handle.type;
        }
    }

    handle.type = type_create_array(basic_type__char, len);
    handle.length = len;
    array_push_back(&string_types, handle);
    return handle.type;
}

/*
 * Keep track of all function declarations globally, in order to coerce
 * forward declarations made in inner scope.
 *
 * int foo(void) {
 *     int bar(int);
 *     return bar(42);
 * }
 *
 * int bar(int a) {
 *     return a * a;
 * }
 *
 * In the above example, both references to bar must resolve to the same
 * symbol, even though the first declaration is not in scope for the
 * actual definition.
 */
static struct hash_table functions;

/*
 * Deallocate all memory owned by namespace.
 */
static void clear_namespace(struct namespace *ns)
{
    int i;
    struct symbol *sym;

    ns->cursor = 0;

    assert(array_len(&ns->scope.counts) == 0);
    assert(array_len(&ns->scope.names) == 0);
    assert(array_len(&ns->scope.symbols) == 0);
    array_clear(&ns->scope.counts);
    array_clear(&ns->scope.names);
    array_clear(&ns->scope.symbols);

    for (i = 0; i < array_len(&ns->symbols); ++i) {
        sym = array_get(&ns->symbols, i);
        free(sym);
    }

    array_clear(&ns->symbols);
    hash_destroy(&ns->globals);
}

INTERNAL void symtab_clear(void)
{
    int i;
    struct symbol *sym;

    for (i = 0; i < array_len(&ns_label.symbols); ++i) {
        sym = array_get(&ns_label.symbols, i);
        if (sym->symtype == SYM_TENTATIVE) {
            error("Undefined label '%s'.", sym_name(sym));
        }
    }

    clear_namespace(&ns_tag);
    clear_namespace(&ns_ident);
    clear_namespace(&ns_label);

    array_empty(&string_types);
    hash_clear(&functions, NULL);
}

INTERNAL void symtab_finalize(void)
{
    int i;
    struct symbol *sym;

    for (i = 0; i < array_len(&temporaries); ++i) {
        sym = array_get(&temporaries, i);
        free(sym);
    }

    array_clear(&temporaries);
    array_clear(&string_types);
    hash_destroy(&functions);
}

INTERNAL void push_scope(struct namespace *ns)
{
    array_push_back(&ns->scope.counts, 0);
}

INTERNAL void pop_scope(struct namespace *ns)
{
    int count;

    assert(array_len(&ns->scope.counts) > 0);

    count = array_pop_back(&ns->scope.counts);
    array_len(&ns->scope.symbols) -= count;
    array_len(&ns->scope.names) -= count;
}

INTERNAL int current_scope_depth(struct namespace *ns)
{
    /*
     * Labels are scoped within a function, and to work with existing
     * logic for other scopes we need that to be depth 0.
     */
    if (ns == &ns_label) {
        assert(array_len(&ns->scope.counts) == 1);
        return 0;
    }

    return array_len(&ns->scope.counts);
}

INTERNAL struct symbol *sym_lookup(struct namespace *ns, String name)
{
    int i;
    String n;

    for (i = array_len(&ns->scope.symbols) - 1; i >= 0; --i) {
        n = array_get(&ns->scope.names, i);
        if (str_eq(name, n)) {
            return array_get(&ns->scope.symbols, i);
        }
    }

    return hash_lookup(&ns->globals, name);
}

INTERNAL const char *sym_name(const struct symbol *sym)
{
    static char name[128];
    const char *raw;

    raw = str_raw(sym->name);
    if (!sym->n) {
        return raw;
    }

    if (strlen(raw) > 100) {
        error("Symbol name %s exceeds limit.", raw);
        exit(1);
    }

    /*
     * Temporary variables and string literals are named '.t' and '.LC',
     * respectively. For those, append the numeral without anything in
     * between. For other variables, which are disambiguated statics,
     * insert a period between the name and the number.
     */
    if (str_raw(sym->name)[0] == '.') {
        sprintf(name, "%s%d", raw, sym->n);
    } else {
        sprintf(name, "%s.%d", raw, sym->n);
    }

    return name;
}

/*
 * Symbols can be declared multiple times, with incomplete or complete
 * types. Only functions and arrays can exist as incomplete. Other
 * symbols can be re-declared, but must have identical type each time.
 *
 * For functions, the last parameter list is applied for as long as the
 * symbol is still tentative.
 */
static void sym_apply_type(struct symbol *sym, Type type)
{
    if (is_function(sym->type)) {
        if (!is_function(type)) {
            error("Conflicting types for %s.", sym_name(sym));
            exit(1);
        }

        if (!is_compatible(type_next(sym->type), type_next(type))) {
            error("Conflicting return type for function %s.", sym_name(sym));
            exit(1);
        }

        if (!is_complete(sym->type)) {
            assert(nmembers(sym->type) == 0);
            if (is_complete(type)) {
                sym->type = type;
            }
        } else if (is_complete(type)) {
            if (is_compatible(sym->type, type)) {
                sym->type = type;
            } else {
                error("Conflicting types for function %s.", sym_name(sym));
                exit(1);
            }
        }
    } else if (is_array(sym->type)) {
        if (!is_array(type)) {
            error("Conflicting types for array %s.", sym_name(sym));
            exit(1);
        }

        if (!type_equal_unqualified(type_next(sym->type), type_next(type))) {
            error("Redefinition of array %s with different type.",
                sym_name(sym));
            exit(1);
        }

        if (!is_complete(sym->type)) {
            if (is_complete(type)) {
                set_array_length(sym->type, type_array_len(type));
            }
        } else if (is_complete(type) && !is_compatible(sym->type, type)) {
            error("Conflicting declaration of array %s.", sym_name(sym));
            exit(1);
        }
    } else if (!is_compatible(sym->type, type)) {
        error("Conflicting types for %s.", sym_name(sym), sym->type, type);
        exit(1);
    }
}

/*
 * Update existing symbol from new declaration or tentative definition.
 */
static struct symbol *sym_redeclare(
    struct symbol *sym,
    struct namespace *ns,
    Type type,
    enum symtype symtype,
    enum linkage linkage)
{
    switch (linkage) {
    case LINK_INTERN:
        if (sym->linkage == LINK_EXTERN) {
            error("'%s' was previously defined non-static.", sym_name(sym));
            exit(1);
        }
        sym->linkage = LINK_INTERN;
        break;
    case LINK_EXTERN:
        if (sym->linkage == LINK_INTERN) {
            if (symtype == SYM_DEFINITION || symtype == SYM_TENTATIVE) {
                error("'%s' was previously declared static.", sym_name(sym));
                exit(1);
            }
        }
        break;
    case LINK_NONE:
        if (sym->depth == current_scope_depth(ns) && sym->depth) {
            error("Duplicate definition of '%s'.", sym_name(sym));
            exit(1);
        }
        break;
    }

    switch (symtype) {
    case SYM_TENTATIVE:
        if (sym->symtype == SYM_DECLARATION) {
            sym->symtype = SYM_TENTATIVE;
        }
    case SYM_DECLARATION:
        if (sym->symtype == SYM_DEFINITION) {
            if (!type_equal(sym->type, type)) {
                error("'%s' redeclared with different type.", sym_name(sym));
                exit(1);
            }
        } else {
            sym_apply_type(sym, type);
        }
        break;
    case SYM_DEFINITION:
        sym->symtype = SYM_DEFINITION;
        sym_apply_type(sym, type);
        break;
    case SYM_TAG:
    case SYM_TYPEDEF:
    case SYM_CONSTANT:
        if (sym->symtype != symtype || !type_equal(sym->type, type)) {
            error("Conflicting declaration of '%s'.", sym_name(sym));
            exit(1);
        }
        break;
    case SYM_LABEL:
    case SYM_LITERAL:
    case SYM_BUILTIN:
        assert(0);
        break;
    }

    return sym;
}

INTERNAL void sym_make_visible(struct namespace *ns, struct symbol *sym)
{
    if (array_len(&ns->scope.counts) == 0) {
        hash_insert(&ns->globals, sym->name, sym, NULL);
    } else {
        array_push_back(&ns->scope.names, sym->name);
        array_push_back(&ns->scope.symbols, sym);
        array_back(&ns->scope.counts) += 1;
    }
}

/*
 * Register a new symbol, or redeclare an existing one.
 *
 * All function declarations must agree, regardless of scope.
 *
 * Scoped static variable are given unique names in order to not collide
 * with other external declarations.
 */
INTERNAL struct symbol *sym_add(
    struct namespace *ns,
    String name,
    Type type,
    enum symtype symtype,
    enum linkage linkage)
{
    static int n;

    int depth;
    struct symbol *sym;
    assert(symtype != SYM_LABEL);
    assert(symtype != SYM_TAG || ns == &ns_tag);

    sym = sym_lookup(ns, name);
    depth = current_scope_depth(ns);
    if (!sym && symtype != SYM_TYPEDEF && is_function(type)) {
        assert(ns == &ns_ident);
        sym = hash_lookup(&functions, name);
        if (sym) {
            sym_apply_type(sym, type);
            sym_make_visible(ns, sym);
            if (depth < sym->depth) {
                sym->depth = depth;
            }
            return sym;
        }
    } else if (sym && (!depth || linkage == LINK_EXTERN) && !sym->depth) {
        return sym_redeclare(sym, ns, type, symtype, linkage);
    }

    sym = alloc_sym();
    sym->depth = depth;
    sym->name = name;
    sym->type = type;
    sym->symtype = symtype;
    sym->linkage = linkage;
    if (linkage == LINK_INTERN && sym->depth) {
        sym->n = ++n;
    }

    if (sym->symtype == SYM_TAG || sym->symtype == SYM_TYPEDEF) {
        type_set_tag(type, sym);
    }

    array_push_back(&ns->symbols, sym);
    sym_make_visible(ns, sym);
    if (symtype != SYM_TYPEDEF && is_function(sym->type)) {
        hash_insert(&functions, sym->name, sym, NULL);
    }

    if (context.verbose) {
        verbose(
            "\t[type: %s, link: %s]\n"
            "\t%s :: %t",
            (sym->symtype == SYM_DEFINITION ? "definition" :
                sym->symtype == SYM_TENTATIVE ? "tentative" :
                sym->symtype == SYM_DECLARATION ? "declaration" :
                sym->symtype == SYM_TYPEDEF ? "typedef" :
                sym->symtype == SYM_TAG ? "tag" :
                sym->symtype == SYM_CONSTANT ? "number" :
                sym->symtype == SYM_LITERAL ? "string" : "builtin"),
            (sym->linkage == LINK_INTERN ? "intern" :
                sym->linkage == LINK_EXTERN ? "extern" : "none"),
            sym_name(sym),
            sym->type);
    }

    return sym;
}

INTERNAL struct symbol *sym_create_temporary(Type type)
{
    static int n;
    struct symbol *sym;

    sym = alloc_sym();
    sym->symtype = SYM_DEFINITION;
    sym->linkage = LINK_NONE;
    sym->name = prefix_temporary;
    sym->type = type;
    sym->n = ++n;
    return sym;
}

INTERNAL struct symbol *sym_create_unnamed(Type type)
{
    static int n;
    struct symbol *sym;

    sym = alloc_sym();
    if (current_scope_depth(&ns_ident) == 0) {
        sym->linkage = LINK_INTERN;
    } else {
        sym->linkage = LINK_NONE;
    }

    sym->symtype = SYM_DEFINITION;
    sym->name = prefix_unnamed;
    sym->type = type;
    sym->n = ++n;
    return sym;
}

INTERNAL struct symbol *sym_create_label(void)
{
    static int n;
    struct symbol *sym;

    sym = alloc_sym();
    sym->type = basic_type__void;
    sym->symtype = SYM_LABEL;
    sym->linkage = LINK_INTERN;
    sym->name = prefix_label;
    sym->n = ++n;
    return sym;
}

INTERNAL struct symbol *sym_create_constant(Type type, union value val)
{
    static int n;
    struct symbol *sym;

    sym = alloc_sym();
    sym->type = type;
    sym->value.constant = val;
    sym->symtype = SYM_CONSTANT;
    sym->linkage = LINK_INTERN;
    sym->name = prefix_constant;
    sym->n = ++n;
    array_push_back(&ns_ident.symbols, sym);
    return sym;
}

/*
 * Store string value directly on symbol, memory ownership is in string
 * table from previously called str_register. The symbol now exists as
 * if declared static char .LC[] = "...".
 */
INTERNAL struct symbol *sym_create_string(String str)
{
    static int n;
    struct symbol *sym;
    size_t len;

    len = str_len(str);
    sym = alloc_sym();
    sym->type = get_string_type(len + 1);
    sym->value.string = str;
    sym->symtype = SYM_LITERAL;
    sym->linkage = LINK_INTERN;
    sym->name = prefix_string;
    sym->n = ++n;
    array_push_back(&ns_ident.symbols, sym);
    return sym;
}

INTERNAL struct symbol *sym_create_builtin(
    String name,
    struct block *(*handler)(struct definition *, struct block *))
{
    struct symbol *sym;

    sym = sym_add(&ns_ident, name, basic_type__void, SYM_BUILTIN, LINK_NONE);
    sym->value.handler = handler;
    return sym;
}

INTERNAL void sym_discard(struct symbol *sym)
{
    array_push_back(&temporaries, sym);
}

INTERNAL int is_temporary(const struct symbol *sym)
{
    return str_eq(prefix_temporary, sym->name);
}

INTERNAL const struct symbol *yield_declaration(struct namespace *ns)
{
    const struct symbol *sym;

    while (ns->cursor < array_len(&ns->symbols)) {
        sym = array_get(&ns->symbols, ns->cursor);
        ns->cursor++;
        switch (sym->symtype) {
        case SYM_LITERAL:
        case SYM_CONSTANT:
        case SYM_DECLARATION:
            if (sym->referenced)
                return sym;
            break;
        case SYM_TENTATIVE:
            return sym;
        default:
            break;
        }
    }

    return NULL;
}

static void print_symbol(FILE *stream, const struct symbol *sym)
{
    fprintf(stream, "%*s", sym->depth * 2, "");
    if (sym->linkage != LINK_NONE) {
        fprintf(stream, "%s ",
            (sym->linkage == LINK_INTERN) ? "static" : "global");
    }

    switch (sym->symtype) {
    case SYM_TENTATIVE:
        fprintf(stream, "tentative ");
        break;
    case SYM_DEFINITION:
        fprintf(stream, "definition ");
        break;
    case SYM_DECLARATION:
        fprintf(stream, "declaration ");
        break;
    case SYM_TYPEDEF:
        fprintf(stream, "typedef ");
        break;
    case SYM_TAG:
        if (is_struct(sym->type)) {
            fprintf(stream, "struct ");
        } else if (is_union(sym->type)) {
            fprintf(stream, "union ");
        } else {
            assert(type_equal(basic_type__int, sym->type));
            fprintf(stream, "enum ");
        }
        break;
    case SYM_CONSTANT:
        fprintf(stream, "number ");
        break;
    case SYM_LITERAL:
        fprintf(stream, "string ");
        break;
    case SYM_LABEL:
        fprintf(stream, "label ");
        break;
    case SYM_BUILTIN:
        fprintf(stream, "builtin ");
        break;
    }

    fprintf(stream, "%s :: ", sym_name(sym));
    fprinttype(stream, sym->type, sym);
    if (size_of(sym->type)) {
        fprintf(stream, ", size=%lu", size_of(sym->type));
    }

    if (sym->stack_offset) {
        fprintf(stream, ", (stack_offset: %d)", sym->stack_offset);
    }
    if (is_vla(sym->type)) {
        fprintf(stream, ", (vla_address: %s)",
            sym_name(sym->value.vla_address));
    }

    if (sym->symtype == SYM_CONSTANT) {
        if (is_signed(sym->type)) {
            fprintf(stream, ", value=%ld", sym->value.constant.i);
        } else if (is_unsigned(sym->type)) {
            fprintf(stream, ", value=%lu", sym->value.constant.u);
        } else if (is_float(sym->type)) {
            fprintf(stream, ", value=%ff", sym->value.constant.f);
        } else if (is_double(sym->type)) {
            fprintf(stream, ", value=%f", sym->value.constant.d);
        } else {
            assert(is_long_double(sym->type));
            fprintf(stream, ", value=%Lf", get_long_double(sym->value.constant));
        }
    }
}

INTERNAL void output_symbols(FILE *stream, struct namespace *ns)
{
    int i;
    const struct symbol *sym;

    fprintf(stream, "namespace %s:\n", ns == &ns_ident ? "identifiers"
        : ns == &ns_label ? "labels" : "tags");

    for (i = 0; i < array_len(&ns->symbols); ++i) {
        sym = array_get(&ns->symbols, i);
        print_symbol(stream, sym);
        fprintf(stream, "\n");
    }
}
