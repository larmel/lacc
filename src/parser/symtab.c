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
    ns_ident = {"identifiers"},
    ns_label = {"labels"},
    ns_tag = {"tags"};

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
static array_of(struct symbol *) temporaries;

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

/* Save memcpy reference for backend. */
INTERNAL const struct symbol *decl_memcpy = NULL;

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

static void symtab_reset_buffers(void)
{
    ns_tag.cursor = 0;
    ns_ident.cursor = 0;
    ns_label.cursor = 0;
    decl_memcpy = NULL;
    array_clear(&string_types);
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
    static struct scope empty;
    struct scope *scope;

    if (array_len(&ns->scope) < ns->max_scope_depth) {
        assert(array_len(&ns->scope) < ns->scope.capacity);
        array_len(&ns->scope) += 1;
        scope = &array_get(&ns->scope, array_len(&ns->scope) - 1);
        if (scope->state == SCOPE_INITIALIZED) {
            scope->state = SCOPE_DIRTY;
        }
    } else {
        ns->max_scope_depth += 1;
        array_push_back(&ns->scope, empty);
        scope = &array_get(&ns->scope, array_len(&ns->scope) - 1);
        scope->state = SCOPE_CREATED;
    }
}

INTERNAL void pop_scope(struct namespace *ns)
{
    int i;
    struct symbol *sym;
    struct scope *scope;

    /*
     * Popping last scope frees the whole symbol table, including the
     * symbols themselves. For label scope, which is per function, make
     * sure there are no tentative definitions.
     */
    assert(array_len(&ns->scope) > 0);
    if (array_len(&ns->scope) == 1) {
        for (i = 0; i < ns->max_scope_depth; ++i) {
            scope = &array_get(&ns->scope, i);
            if (scope->state != SCOPE_CREATED) {
                hash_destroy(&scope->table);
            }
        }

        ns->max_scope_depth = 0;
        array_clear(&ns->scope);
        for (i = 0; i < array_len(&ns->symbol); ++i) {
            sym = array_get(&ns->symbol, i);
            if (ns == &ns_label && sym->symtype == SYM_TENTATIVE) {
                error("Undefined label '%s'.", sym_name(sym));
            }
            free(sym);
        }

        array_clear(&ns->symbol);
        if (ns == &ns_ident) {
            symtab_reset_buffers();
        }
    } else {
        array_len(&ns->scope) -= 1;
    }
}

INTERNAL unsigned current_scope_depth(struct namespace *ns)
{
    unsigned depth = array_len(&ns->scope);
    assert(depth);
    return depth - 1;
}

INTERNAL struct symbol *sym_lookup(struct namespace *ns, String name)
{
    int i;
    struct scope *scope;
    struct symbol *sym;

    for (i = array_len(&ns->scope) - 1; i >= 0; --i) {
        scope = &array_get(&ns->scope, i);
        if (scope->state == SCOPE_INITIALIZED) {
            sym = hash_lookup(&scope->table, name);
            if (sym) {
                return sym;
            }
        }
    }

    return NULL;
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
    if (is_function(sym->type)
        && is_function(type)
        && type_equal(type_next(sym->type), type_next(type)))
    {
        if (!is_complete(sym->type)
            || (is_complete(type) && is_compatible(sym->type, type)))
        {
            sym->type = type;
        } else if (!is_complete(type)) {
            return;
        }
    } else if (is_array(sym->type) && is_array(type)) {
        if (type_equal(type_next(sym->type), type_next(type))) {
            if (!is_complete(sym->type) && is_complete(type)) {
                set_array_length(sym->type, type_array_len(type));
            } else if (!is_complete(type)) {
                return;
            }
        }
    }

    if (!type_equal(sym->type, type)) {
        error("'%s' declared with different types.", sym_name(sym));
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
        assert(0);
        break;
    }

    return sym;
}

INTERNAL void sym_make_visible(struct namespace *ns, struct symbol *sym)
{
    struct scope *scope;

    scope = &array_get(&ns->scope, array_len(&ns->scope) - 1);
    if (scope->state == SCOPE_DIRTY) {
        hash_clear(&scope->table, NULL);
    }

    hash_insert(&scope->table, sym->name, sym, NULL);
    scope->state = SCOPE_INITIALIZED;
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
    static String smemcpy = SHORT_STRING_INIT("memcpy");

    unsigned depth;
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
    } else if (sym
        && (!depth || linkage == LINK_EXTERN)
        && !sym->depth)
    {
        return sym_redeclare(sym, ns, type, symtype, linkage);
    }

    sym = alloc_sym();
    sym->depth = depth;
    sym->name = name;
    sym->type = type;
    sym->symtype = symtype;
    sym->linkage = linkage;
    if (!decl_memcpy && str_eq(smemcpy, sym->name)) {
        decl_memcpy = sym;
    }

    if (linkage == LINK_INTERN && sym->depth) {
        sym->n = ++n;
    }

    if (sym->symtype == SYM_TAG || sym->symtype == SYM_TYPEDEF) {
        type_set_tag(type, sym);
    }

    array_push_back(&ns->symbol, sym);
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
                sym->symtype == SYM_CONSTANT ? "number" : "string"),
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
    array_push_back(&ns_ident.symbol, sym);
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
    array_push_back(&ns_ident.symbol, sym);
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

    while (ns->cursor < array_len(&ns->symbol)) {
        sym = array_get(&ns->symbol, ns->cursor);
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
            fprintf(stream, ", value=%Lf", sym->value.constant.ld);
        }
    }
}

INTERNAL void output_symbols(FILE *stream, struct namespace *ns)
{
    int i;
    const struct symbol *sym;

    for (i = 0; i < array_len(&ns->symbol); ++i) {
        if (!i) {
            fprintf(stream, "namespace %s:\n", ns->name);
        }

        sym = array_get(&ns->symbol, i);
        print_symbol(stream, sym);
        fprintf(stream, "\n");
    }
}
