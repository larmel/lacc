#if _XOPEN_SOURCE < 500
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 500 /* snprintf */
#endif
#include "error.h"
#include "symbol.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void push_scope(struct namespace *ns) {
    ns->current_depth++;
    if (!ns->scope) {
        ns->current_depth = 0;
    }

    ns->scope =
        realloc(ns->scope, sizeof(*ns->scope) * (ns->current_depth + 1));
    memset(&ns->scope[ns->current_depth], 0x0, sizeof(struct scope));
}

void pop_scope(struct namespace *ns) {
    if (ns->current_depth >= 0) {
        if (ns->scope[ns->current_depth].idx) {
            free(ns->scope[ns->current_depth].idx);    
        }
        memset(&ns->scope[ns->current_depth], 0x0, sizeof(struct scope));
        ns->current_depth--;
    }

    if (ns->current_depth == -1) {
        int i;

        if (ns->scope) {
            free(ns->scope);
            ns->scope = NULL;
        }
        if (ns->symbol) {
            for (i = 0; i < ns->size; ++i) {
                free(ns->symbol[i]);
            }
            free(ns->symbol);
            ns->symbol = NULL;
            ns->size = 0;
            ns->cap = 0;
        }
    }
}

/* Create and add symbol to symbol table, but not to any scope. Symbol address
 * needs to be stable, so they are stored as a realloc-safe list of pointers.
 */
static int create_symbol(struct namespace *ns, struct symbol arg)
{
    struct symbol *sym = calloc(1, sizeof(*sym));

    arg.depth = ns->current_depth;
    if (ns->size == ns->cap) {
        ns->cap += 64;
        ns->symbol = realloc(ns->symbol, sizeof(*ns->symbol) * ns->cap);
    }

    *sym = arg;
    ns->symbol[ns->size] = sym;
    return ns->size++;
}

/* Add symbol to current scope, making it possible to look up. Name must be non-
 * NULL, i.e. immediate values do not belong to any scope.
 */
static struct symbol *register_in_scope(struct namespace *ns, int i)
{
    struct scope *scope;

    assert(i < ns->size);
    scope = &ns->scope[ns->current_depth];
    if (scope->size == scope->cap) {
        scope->cap += 16;
        scope->idx = realloc(scope->idx, scope->cap * sizeof(int *));
    }
    scope->idx[scope->size] = i;
    scope->size++;

    return ns->symbol[i];
}

/* Create a temporary identifier name. Use a fixed prefix '.' to all temporary 
 * variables, which will never collide with real symbols.
 */
static char *unique_identifier_name(void)
{
    static int n;
    char *c = malloc(16);
    snprintf(c, 16, ".t%d", n++);
    return c;
}

const char *sym_name(const struct symbol *sym)
{
    if (sym->n) {
        static char name[128];
        snprintf(name, 127, "%s.%d", sym->name, sym->n);
        return name;
    }

    return sym->name;
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
        && !(sym->type.type == FUNCTION && sym->symtype != SYM_DEFINITION))
        return;

    switch (sym->type.type) {
    case FUNCTION:
        if (sym->type.type == type->type
            && sym->type.flags == type->flags
            && type_equal(sym->type.next, type->next))
        {
            conflict = 0;
            if (!sym->type.n || sym->type.n == type->n) {
                sym->type.n = type->n;
                sym->type.member = type->member;
            } else
                conflict = 1;
        }
        break;
    case ARRAY:
        if (sym->type.type == type->type
            && type_equal(sym->type.next, type->next))
        {
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

struct symbol *sym_lookup(struct namespace *ns, const char *name)
{
    int i, d;
    for (d = ns->current_depth; d >= 0; --d) {
        for (i = 0; i < ns->scope[d].size; ++i) {
            int idx = ns->scope[d].idx[i];
            if (!strcmp(name, ns->symbol[idx]->name)) {
                return ns->symbol[idx];
            }
        }
    }
    return NULL;
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
    struct symbol sym = {0};

    sym.symtype = SYM_DEFINITION;
    sym.linkage = LINK_NONE;
    sym.name = unique_identifier_name();
    sym.type = *type;
    return register_in_scope(ns, create_symbol(ns, sym));
}

void register_builtin_types(struct namespace *ns)
{
    struct typetree
        *type = type_init_object(),
        *none = type_init_void();

    type_add_member(type, type_init_unsigned(4), "gp_offset");
    type_add_member(type, type_init_unsigned(4), "fp_offset");
    type_add_member(type, type_init_pointer(none), "overflow_arg_area");
    type_add_member(type, type_init_pointer(none), "reg_save_area");
    type_align_struct_members(type);
    type = type_init_array(type, 1);

    /* Define va_list as described in System V ABI. */
    sym_add(ns, "__builtin_va_list", type, SYM_TYPEDEF, LINK_NONE);

    /* Add symbols with dummy types just to reserve them, and make them resolve
     * during parsing. These are implemented as compiler intrinsics. */
    sym_add(ns, "__builtin_va_start", none, SYM_DECLARATION, LINK_NONE);
    sym_add(ns, "__builtin_va_arg", none, SYM_DECLARATION, LINK_NONE);
}

static int sym_asm_alignment(const struct symbol *sym)
{
    int w = size_of(&sym->type);
    if (w >= 16) return 16;
    if (w >= 8) return 8;
    return 4;
}

void assemble_tentative_definitions(FILE *stream)
{
    int i;
    struct symbol *sym;

    for (i = 0; i < ns_ident.size; ++i) {
        sym = ns_ident.symbol[i];
        if (sym->symtype == SYM_TENTATIVE && is_object(&sym->type)) {
            if (sym->linkage == LINK_INTERN) {
                fprintf(stream, "\t.local %s\n", sym_name(sym));
            }
            fprintf(stream, "\t.comm %s, %d, %d\n",
                sym_name(sym), size_of(&sym->type), sym_asm_alignment(sym));
        }
    }
}

void output_symbols(FILE *stream, struct namespace *ns)
{
    int i;
    char *tstr;

    if (ns->size) {
        verbose("namespace %s:", ns->name);
    }
    for (i = 0; i < ns->size; ++i) {
        enum symtype st = ns->symbol[i]->symtype;

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

        fprintf(stream, "%s :: ", ns->symbol[i]->name);
        tstr = typetostr(&ns->symbol[i]->type);
        fprintf(stream, "%s", tstr);
        free(tstr);

        fprintf(stream, ", size=%d", size_of(&ns->symbol[i]->type));
        if (ns->symbol[i]->stack_offset) {
            fprintf(stream, " (stack_offset: %d)", ns->symbol[i]->stack_offset);
        }
        if (ns->symbol[i]->symtype == SYM_ENUM_VALUE) {
            fprintf(stream, ", value=%d", ns->symbol[i]->enum_value);
        }
        fprintf(stream, "\n");
    }
}
