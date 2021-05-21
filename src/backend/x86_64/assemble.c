#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "abi.h"
#include "assemble.h"
#include <lacc/context.h>

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>

#define MAX_OPERAND_TEXT_LENGTH 256

static const struct symbol *current_symbol;

static FILE *asm_output;

static void out(const char *s, ...)
{
    va_list args;

    va_start(args, s);
    vfprintf(asm_output, s, args);
    va_end(args);
}

static enum section {
    SECTION_NONE,
    SECTION_TEXT,
    SECTION_DATA,
    SECTION_RODATA
} current_section = SECTION_NONE;

static void set_section(enum section section)
{
    if (section != current_section) switch (section) {
    case SECTION_TEXT:
        out("\t.text\n");
        break;
    case SECTION_DATA:
        out("\t.data\n");
        break;
    case SECTION_RODATA:
        out("\t.section\t.rodata\n");
        break;
    default: break;
    }

    current_section = section;
}

static const char *reg_name[] = {
    "%al",   "%ax",   "%eax",  "%rax",
    "%cl",   "%cx",   "%ecx",  "%rcx",
    "%dl",   "%dx",   "%edx",  "%rdx",
    "%bl",   "%bx",   "%ebx",  "%rbx",
    "%spl",  "%sp",   "%esp",  "%rsp",
    "%bpl",  "%bp",   "%ebp",  "%rbp",
    "%sil",  "%si",   "%esi",  "%rsi",
    "%dil",  "%di",   "%edi",  "%rdi",
    "%r8b",  "%r8w",  "%r8d",  "%r8",
    "%r9b",  "%r9w",  "%r9d",  "%r9",
    "%r10b", "%r10w", "%r10d", "%r10",
    "%r11b", "%r11w", "%r11d", "%r11",
    "%r12b", "%r12w", "%r12d", "%r12",
    "%r13b", "%r13w", "%r13d", "%r13",
    "%r14b", "%r14w", "%r14d", "%r14",
    "%r15b", "%r15w", "%r15d", "%r15"
};

static const char *xmm_name[] = {
    "%xmm0",  "%xmm1",  "%xmm2",  "%xmm3",
    "%xmm4",  "%xmm5",  "%xmm6",  "%xmm7",
    "%xmm8",  "%xmm9",  "%xmm10", "%xmm11",
    "%xmm12", "%xmm13", "%xmm14", "%xmm15"
};

static const char *x87_name[] = {
    "%st(0)", "%st(1)", "%st(2)", "%st(3)",
    "%st(4)", "%st(5)", "%st(6)", "%st(7)"
};

static const char *regname(struct registr reg)
{
    int i, j;

    if (reg.r == IP) {
        assert(reg.width == 8);
        return "%rip";
    } else if (reg.r < XMM0) {
        i = 4 * (reg.r - 1);
        j = reg.width - 1;

        if (j == 3) j = 2;
        if (j == 7) j = 3;

        return reg_name[i + j];
    } else if (reg.r < ST0) {
        return xmm_name[reg.r - XMM0];
    } else {
        i = x87_stack_pos(reg.r);
        return x87_name[i];
    }
}

static const char *asm_address(struct address addr)
{
    static char buf[MAX_OPERAND_TEXT_LENGTH];

    struct registr reg = {0};
    int w = 0;

    reg.width = 8;
    if (addr.sym) {
        w += sprintf(buf + w, "%s", sym_name(addr.sym));
        switch (addr.type) {
        case ADDR_GLOBAL_OFFSET:
            assert(addr.displacement == 0);
            w += sprintf(buf + w, "@GOTPCREL");
            break;
        case ADDR_PLT:
            assert(addr.displacement == 0);
            w += sprintf(buf + w, "@PLT");
            break;
        default:
            if (addr.displacement != 0) {
                w += sprintf(buf + w, "%s%d",
                    (addr.displacement > 0) ? "+" : "", addr.displacement);
            }
            break;
        }
    } else if (addr.displacement != 0) {
        w += sprintf(buf, "%d", addr.displacement);
    }

    if (addr.base) {
        reg.r = addr.base;
        w += sprintf(buf + w, "(%s", regname(reg));
        if (addr.index) {
            reg.r = addr.index;
            w += sprintf(buf + w, ",%s,%d", regname(reg), addr.scale);
        } else assert(!addr.scale);
        sprintf(buf + w, ")");
    } else if (addr.index) {
        reg.r = addr.index;
        sprintf(buf + w, "(,%s,%d)", regname(reg), addr.scale);
    } else assert(!addr.scale);

    return buf;
}

static const char *immediate(struct immediate imm)
{
    static char buf[MAX_OPERAND_TEXT_LENGTH];

    if (imm.type == IMM_INT) {
        if (imm.width < 8) {
            sprintf(buf, "$%d",
                (imm.width == 1) ? imm.d.byte :
                (imm.width == 2) ? imm.d.word : imm.d.dword);
        } else {
            sprintf(buf, "$%ld", imm.d.qword);
        }

        return buf;
    }

    assert(imm.type == IMM_ADDR);
    assert(imm.d.addr.sym);
    assert(imm.d.addr.sym->symtype != SYM_LITERAL);

    return asm_address(imm.d.addr);
}

INTERNAL void asm_init(FILE *output, const char *file)
{
    asm_output = output;
    if (file) {
        out("\t.file\t\"%s\"\n", file);
    }
}

INTERNAL int asm_symbol(const struct symbol *sym)
{
    const char *name;
    size_t size;

    /*
     * Labels stay in the same function context, otherwise flush to
     * write any end of function metadata.
     */
    if (sym->symtype != SYM_LABEL) {
        asm_flush();
        current_symbol = sym;
    }

    name = sym_name(sym);
    size = size_of(sym->type);
    switch (sym->symtype) {
    case SYM_TENTATIVE:
        assert(is_object(sym->type));
        if (!context.no_common) {
            if (sym->linkage == LINK_INTERN)
                out("\t.local\t%s\n", name);
            out("\t.comm\t%s,%lu,%lu\n", name, size, type_alignment(sym->type));
            break;
        }
    case SYM_DEFINITION:
        if (is_function(sym->type)) {
            set_section(SECTION_TEXT);
            if (sym->linkage == LINK_EXTERN)
                out("\t.globl\t%s\n", name);
            out("\t.type\t%s, @function\n", name);
            out("%s:\n", name);
        } else {
            set_section(SECTION_DATA);
            if (sym->linkage == LINK_EXTERN)
                out("\t.globl\t%s\n", name);
            out("\t.align\t%d\n", sym_alignment(sym));
            out("\t.type\t%s, @object\n", name);
            out("\t.size\t%s, %lu\n", name, size);
            out("%s:\n", name);
            if (sym->symtype == SYM_TENTATIVE) {
                out("\t.zero %lu\n", size);
            }
        }
        break;
    case SYM_LITERAL:
        set_section(SECTION_RODATA);
        out("\t.align\t%d\n", sym_alignment(sym));
        out("\t.type\t%s, @object\n", name);
        out("\t.size\t%s, %lu\n", name, size);
        out("%s:\n", name);
        out("\t.string\t");
        fprintstr(asm_output, sym->value.string);
        out("\n");
        break;
    case SYM_CONSTANT:
        set_section(SECTION_RODATA);
        out("\t.align\t%d\n", sym_alignment(sym));
        out("%s:\n", name);
        if (is_float(sym->type)) {
            out("\t.long\t%lu\n", sym->value.constant.u & 0xFFFFFFFFu);
        } else if (is_double(sym->type)) {
            out("\t.quad\t%ld\n", sym->value.constant.i);
        } else {
            union {
                long double ld;
                long i[2];
            } conv = {0};
            assert(is_long_double(sym->type));
            conv.ld = get_long_double(sym->value.constant);
            out("\t.quad\t%ld\n", conv.i[0]);
            out("\t.quad\t%ld\n", conv.i[1] & 0xFFFF);
        }
        break;
    case SYM_LABEL:
        out("%s:\n", name);
        break;
    default:
        break;
    }

    return 0;
}

INTERNAL int asm_text(struct instruction instr)
{
    char buf[11] = {0};

    out("\t");
    switch (instr.prefix) {
    case PREFIX_REP: out("rep "); break;
    case PREFIX_REPNE: out("repne "); break;
    default: break;
    }

    get_mnemonic(instr, buf);
    out("%s", buf);
    switch (instr.optype) {
    case OPT_REG:
        if (instr.opcode == INSTR_CALL) {
            out("\t*%s", regname(instr.source.reg));
            break;
        }
    case OPT_REG_REG:
    case OPT_REG_MEM:
        out("\t%s", regname(instr.source.reg));
        break;
    case OPT_IMM:
    case OPT_IMM_REG:
    case OPT_IMM_MEM:
        out("\t%s", immediate(instr.source.imm));
        break;
    case OPT_MEM:
    case OPT_MEM_REG:
        out("\t%s", asm_address(instr.source.mem.addr));
        break;
    default:
        break;
    }

    switch (instr.optype) {
    case OPT_REG_REG:
    case OPT_MEM_REG:
    case OPT_IMM_REG:
        out(", %s", regname(instr.dest.reg));
        break;
    case OPT_REG_MEM:
    case OPT_IMM_MEM:
        out(", %s", asm_address(instr.dest.mem.addr));
        break;
    default:
        break;
    }

    out("\n");
    return 0;
}

INTERNAL int asm_data(struct immediate data)
{
    int i;

    switch (data.type) {
    case IMM_INT:
        switch (data.width) {
        case 1:
            out("\t.byte\t%d\n", data.d.byte);
            break;
        case 2:
            out("\t.short\t%d\n", data.d.word);
            break;
        case 4:
            out("\t.int\t%d\n", data.d.dword);
            break;
        case 8:
            out("\t.quad\t%ld\n", data.d.qword);
            break;
        default:
            assert(data.width > 0 && data.width < 8);
            for (i = 0; i < data.width; ++i) {
                out("\t.byte\t%d\n", (char) (data.d.qword >> (i * 8)));
            }
            break;
        }
        break;
    case IMM_ADDR:
        assert(data.d.addr.sym);
        if (data.d.addr.displacement) {
            out("\t.quad\t%s%s%d\n", sym_name(data.d.addr.sym),
                data.d.addr.displacement < 0 ? "" : "+",
                data.d.addr.displacement);
        } else
            out("\t.quad\t%s\n", sym_name(data.d.addr.sym));
        break;
    case IMM_STRING:
        if (data.width == str_len(data.d.string)) {
            out("\t.ascii\t");
        } else {
            assert(data.width == str_len(data.d.string) + 1);
            out("\t.string\t");
        }
        fprintstr(asm_output, data.d.string);
        out("\n");
        break;
    }
    return 0;
}

INTERNAL int asm_flush(void)
{
    const char *name;

    if (current_symbol
        && is_function(current_symbol->type)
        && current_symbol->symtype == SYM_DEFINITION)
    {
        name = sym_name(current_symbol);
        out("\t.size\t%s, .-%s\n", name, name);
    }

    current_symbol = NULL;
    current_section = SECTION_NONE;
    return 0;
}
