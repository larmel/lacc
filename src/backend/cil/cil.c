#include "cil.h"

#include <assert.h>
#include <stdarg.h>

static FILE *stream;
static const char *libc_assembly_name = "libc.so.6";
static const char *assembly_name = "foo";

static int print(const char *format, ...)
{
    int n;
    va_list args;

    va_start(args, format);
    n = vfprintf(stream, format, args);
    va_end(args);
    return n;
}

static const char *cilname(const struct symbol *sym)
{
    const char *name = sym_name(sym);
    return name + (name[0] == '.');
}

static const char *ciltype(const struct typetree *type)
{
    switch (type->type) {
    case T_UNSIGNED:
        return (type->size == 1) ? "unsigned int8"
            : (type->size == 2) ? "unsigned int16"
            : (type->size == 4) ? "unsigned int32" : "unsigned int64";
    case T_SIGNED:
        return (type->size == 1) ? "int8"
            : (type->size == 2) ? "int16"
            : (type->size == 4) ? "int32" : "int64";
    case T_REAL:
        return (type->size == 4) ? "float32" : "float64";
    case T_POINTER:
        if (type_equal(&basic_type__char, type->next))
            return "string";
        return "native int";
    case T_VOID:
        return "void";
    case T_FUNCTION:
    case T_ARRAY:
    case T_STRUCT:
    case T_UNION:
        break;
    }

    return "int32";
}

static void print_method_signature(const struct typetree *type)
{
    int i;
    const struct member *m;

    assert(is_function(type));
    print("(");
    for (i = 0; i < nmembers(type); ++i) {
        if (i) print(", ");
        m = get_member(type, i);
        print("%s", ciltype(m->type));
        if (m->name.len)
            print(" %s", str_raw(m->name));
    }
    print(")");
}

static void push_variable(struct var var)
{
    switch (var.kind) {
    case DIRECT:
        if (var.symbol->linkage == LINK_NONE)
            print("\tldloc.%d\n", var.symbol->index);
        break;
    case IMMEDIATE:
        if (type_equal(&basic_type__int, var.type))
            print("\tldc.i4\t%d\n", var.imm.i);
        else if (var.symbol->symtype == SYM_STRING_VALUE) 
            print("\tldstr\t\"%s\"\n", str_raw(var.symbol->string_value));
        break;
    default:
        break;
    }
}

static void compile_expression(struct expression expr)
{
    switch (expr.op) {
    case IR_OP_CAST:
        if (is_identity(expr))
            push_variable(expr.l);
        break;
    case IR_OP_CALL:
        if (expr.l.kind == ADDRESS) {
            assert(is_function(&expr.l.symbol->type));
            print("\tcall %s %s",
                ciltype(expr.l.symbol->type.next), cilname(expr.l.symbol));
            print_method_signature(&expr.l.symbol->type);
            print("\n");
        }
        break;
    case IR_OP_ADD:
        push_variable(expr.l);
        push_variable(expr.r);
        print("\tadd\n");
        break;
    default:
        break;
    }
}

static void compile_block(struct block *block)
{
    int i;
    struct statement s;

    if (block->color == BLACK)
        return;

    block->color = BLACK;
    print("%s:", cilname(block->label));
    for (i = 0; i < array_len(&block->code); ++i) {
        s = array_get(&block->code, i);
        switch (s.st) {
        case IR_PARAM:
            compile_expression(s.expr);
            break;
        case IR_EXPR:
            compile_expression(s.expr);
            if (is_object(s.expr.type)) {
                print("\tpop\n");
            }
            break;
        case IR_ASSIGN:
            compile_expression(s.expr);
            if (s.t.kind == DIRECT)
                print("\tstloc.%d\n", s.t.symbol->index);
        default:
            break;
        }
    }

    if (block->jump[0] && block->jump[1]) {
        compile_block(block->jump[0]);
        compile_block(block->jump[1]);
    } else if (block->jump[0]) {
        compile_block(block->jump[0]);
    } else {
        if (block->has_return_value)
            compile_expression(block->expr);
        print("\tret\n");
    }
}

static void compile_function_body(struct definition *def)
{
    int i;
    int maxstack = 0;
    struct symbol *sym;

    for (i = 0; i < array_len(&def->locals); ++i) {
        sym = array_get(&def->locals, i);
        sym->stack_offset = maxstack;
        sym->index = i;
        maxstack += size_of(&sym->type);
    }

    print("\t.maxstack %d\n", maxstack);
    if (i) {
        print("\t.locals init (\n");
        for (i = 0; i < array_len(&def->locals); ++i) {
            sym = array_get(&def->locals, i);
            if (i) print(",\n");
            print("\t\t[%d] %s %s",
                sym->index, ciltype(&sym->type), cilname(sym));
        }
        print(")\n");
    }

    compile_block(def->body);
}

void cil_set_output(FILE *output)
{
    stream = output;
    print(".assembly %s { }\n", assembly_name);
}

void cil_compile_definition(struct definition *def)
{
    const struct symbol *sym = def->symbol;
    const char *name;

    name = sym_name(sym);
    if (is_function(&sym->type)) {
        print(".method static %s %s", ciltype(sym->type.next), name);
        print_method_signature(&sym->type);
        print("\n");
        print("{\n");
        if (!strcmp("main", name)) {
            print("\t.entrypoint\n");
        }

        compile_function_body(def);
        print("}\n");
    }
}

void cil_declare_symbol(const struct symbol *sym)
{
    if (sym->linkage == LINK_EXTERN && is_function(&sym->type)) {
        if (!strcmp("memcpy", sym_name(sym)))
            return;
        print(".method static pinvokeimpl(\"%s\" cdecl)\n",
            libc_assembly_name);
        print("\t%s %s ", ciltype(&sym->type), sym_name(sym));
        print_method_signature(&sym->type);
        print(" { }\n");
    }
}

void cil_flush(void)
{

}
