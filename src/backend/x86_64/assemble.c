#if _XOPEN_SOURCE < 500
#  undef _XOPEN_SOURCE
#  define _XOPEN_SOURCE 500 /* snprintf */
#endif
#include "abi.h"
#include "assemble.h"

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>

#define SUFFIX(w) ((w) == 1 ? 'b' : (w) == 2 ? 'w' : (w) == 4 ? 'l' : 'q')

#define I0(instr)           out("\t%s\n", instr)
#define I1(instr, op)       out("\t%s\t%s\n", instr, op)
#define I2(instr, a, b)     out("\t%s\t%s, %s\n", instr, a, b)
#define S1(instr, w, op)    out("\t%s%c\t%s\n", instr, SUFFIX(w), op)
#define S2(instr, w, a, b)  out("\t%s%c\t%s, %s\n", instr, SUFFIX(w), a, b)

#define MAX_OPERAND_TEXT_LENGTH 256

/* Clients must set this field first.
 */
FILE *asm_output = NULL;

static const struct symbol *current_symbol;

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
    "%r15b", "%r15w", "%r15d", "%r15",
    "%ip",   "%ipw",  "%eip",  "%rip"
};

static const char *xmm_name[] = {
    "%xmm0", "%xmm1", "%xmm2", "%xmm3", "%xmm4", "%xmm5", "%xmm6", "%xmm7",
    "%xmm8", "%xmm9", "%xmm10", "%xmm11", "%xmm12", "%xmm13", "%xmm14", "%xmm15"
};

static void out(const char *s, ...)
{
    va_list args;

    va_start(args, s);
    vfprintf(asm_output, s, args);
    va_end(args);
}

static const char *mnemonic(struct registr reg)
{
    int i, j;

    if (reg.r < XMM0) {
        i = 4 * ((int) reg.r - 1);
        j = reg.w - 1;

        if (j == 3) j = 2;
        if (j == 7) j = 3;

        return reg_name[i + j];
    } else {
        i = (int) reg.r - XMM0;
        assert(reg.w == 16);
        assert(i >= 0 && i < 16);
        return xmm_name[i];
    }
}

static const char *address(struct address addr)
{
    static char buf[MAX_OPERAND_TEXT_LENGTH];

    struct registr reg = {0, 8};
    int w = 0,
        s = sizeof(buf);

    if (addr.sym) {
        if (addr.disp != 0) {
            w += snprintf(buf + w, s - w, "%s%s%d",
                sym_name(addr.sym),
                (addr.disp > 0) ? "+" : "",
                addr.disp);
        } else {
            w += snprintf(buf + w, s - w, "%s", sym_name(addr.sym));
        }
    } else if (addr.disp != 0) {
        w += snprintf(buf + w, s - w, "%d", addr.disp);
    }

    reg.r = addr.base;
    w += snprintf(buf + w, s - w, "(%s", mnemonic(reg));
    if (addr.offset) {
        reg.r = addr.offset;
        w += snprintf(buf + w, s - w, ",%s,%d", mnemonic(reg), addr.mult);
    }
    w += snprintf(buf + w, s - w, ")");

    return buf;
}

static const char *immediate(struct immediate imm, int *size)
{
    static char buf[MAX_OPERAND_TEXT_LENGTH];

    int w = 0,
        s = sizeof(buf);

    *size = 8;
    switch (imm.type) {
    case IMM_INT:
        *size = imm.w;
        if (imm.w < 8)
            w += snprintf(buf + w, s - w, "$%d",
                (imm.w == 1) ? imm.d.byte :
                (imm.w == 2) ? imm.d.word : imm.d.dword);
        else
            w += snprintf(buf + w, s - w, "$%ld", imm.d.qword);
        break;
    case IMM_ADDR:
        assert(imm.d.addr.sym);
        if (imm.d.addr.sym->symtype == SYM_STRING_VALUE) {
            if (imm.d.addr.disp != 0)
                w += snprintf(buf + w, s - w, "$%s%s%d",
                    sym_name(imm.d.addr.sym),
                    (imm.d.addr.disp > 0) ? "+" : "",
                    imm.d.addr.disp);
            else
                w += snprintf(buf + w, s - w, "$%s", sym_name(imm.d.addr.sym));
        } else {
            w += snprintf(buf + w, s - w, "%s", sym_name(imm.d.addr.sym));
        }
        break;
    case IMM_STRING:
        assert(0);
        break;
    }

    return buf;
}

static void output_escaped_string(const char *str)
{
    char c;

    while ((c = *str++) != '\0') {
        if (isprint(c) && c != '"' && c != '\\') {
            putc(c, asm_output);
            continue;
        }

        switch (c) {
        case '\b': fprintf(asm_output, "\\b");  break;
        case '\t': fprintf(asm_output, "\\t");  break;
        case '\n': fprintf(asm_output, "\\n");  break;
        case '\f': fprintf(asm_output, "\\f");  break;
        case '\r': fprintf(asm_output, "\\r");  break;
        case '\\': fprintf(asm_output, "\\\\"); break;
        case '"':  fprintf(asm_output, "\\\""); break;
        default:
            fprintf(asm_output, "\\0%02o", c);
            break;
        }
    }
}

int asm_symbol(const struct symbol *sym)
{
    /* Labels stay in the same function context, otherwise flush to write any
     * end of function metadata. */
    if (sym->symtype != SYM_LABEL) {
        asm_flush();
        current_symbol = sym;
    }

    if (sym->symtype == SYM_TENTATIVE) {
        if (is_object(&sym->type)) {
            if (sym->linkage == LINK_INTERN)
                out("\t.local %s\n", sym_name(sym));
            out("\t.comm %s,%d,%d\n",
                sym_name(sym), size_of(&sym->type), type_alignment(&sym->type));
        } else {
            /* Tentative function, nothing to output. */
            assert(is_function(&sym->type));
        }
    } else if (is_function(&sym->type)) {
        I0(".text");
        if (sym->linkage == LINK_EXTERN)
            I1(".globl", sym->name);
        I2(".type", sym->name, "@function");
        out("%s:\n", sym->name);
    } else if (sym->symtype == SYM_STRING_VALUE) {
        I0(".data");
        out("\t.align\t%d\n", sym_alignment(sym));
        out("\t.type\t%s, @object\n", sym_name(sym));
        out("\t.size\t%s, %d\n", sym_name(sym), size_of(&sym->type));
        out("%s:\n", sym_name(sym));
        out("\t.string\t\"");
        output_escaped_string(sym->string_value);
        out("\"\n");
    } else if (sym->symtype == SYM_LABEL) {
        out("%s:\n", sym_name(sym));
    } else {
        I0(".data");
        if (sym->linkage == LINK_EXTERN)
            I1(".globl", sym->name);
        out("\t.align\t%d\n", sym_alignment(sym));
        out("\t.type\t%s, @object\n", sym_name(sym));
        out("\t.size\t%s, %d\n", sym_name(sym), size_of(&sym->type));
        out("%s:\n", sym_name(sym));
    }

    return 0;
}

int asm_text(struct instruction instr)
{
    int ws = 0,
        wd = 0;
    const char
        *source = NULL,
        *destin = NULL;

    switch (instr.optype) {
    case OPT_REG:
    case OPT_REG_REG:
    case OPT_REG_MEM:
        ws = instr.source.reg.w;
        source = mnemonic(instr.source.reg);
        break;
    case OPT_IMM:
    case OPT_IMM_REG:
    case OPT_IMM_MEM:
        source = immediate(instr.source.imm, &ws);
        break;
    case OPT_MEM:
    case OPT_MEM_REG:
        ws = instr.source.mem.w;
        source = address(instr.source.mem.addr);
        break;
    default:
        break;
    }

    switch (instr.optype) {
    case OPT_REG_REG:
    case OPT_MEM_REG:
    case OPT_IMM_REG:
        wd = instr.dest.reg.w;
        destin = mnemonic(instr.dest.reg);
        break;
    case OPT_REG_MEM:
    case OPT_IMM_MEM:
        wd = instr.dest.mem.w;
        destin = address(instr.dest.mem.addr);
        break;
    default:
        break;
    }

    switch (instr.opcode) {
    case INSTR_ADD:      S2("add", wd, source, destin); break;
    case INSTR_SUB:      S2("sub", wd, source, destin); break;
    case INSTR_NOT:      S1("not", ws, source); break;
    case INSTR_MUL:      S1("mul", ws, source); break;
    case INSTR_DIV:      S1("div", ws, source); break;
    case INSTR_XOR:      S2("xor", wd, source, destin); break;
    case INSTR_AND:      S2("and", wd, source, destin); break;
    case INSTR_OR:       S2("or", wd, source, destin); break;
    case INSTR_SHL:      S2("shl", wd, source, destin); break;
    case INSTR_SHR:      S2("shr", wd, source, destin); break;
    case INSTR_SAR:      S2("sar", wd, source, destin); break;
    case INSTR_MOV:      S2("mov", wd, source, destin); break;
    case INSTR_MOVZX:
        assert(ws == 1 || ws == 2);
        assert(ws < wd);
        S2((ws == 1) ? "movzb" : "movzw", wd, source, destin);
        break;
    case INSTR_MOVSX:
        assert(ws == 1 || ws == 2 || ws == 4);
        assert(ws < wd);
        S2((ws == 1) ? "movsb" : (ws == 2) ? "movsw" : "movsl",
            wd, source, destin);
        break;
    case INSTR_MOVAPS:
        I2("movaps", source, destin);
        break;
    case INSTR_SETZ:     I1("setz", source); break;
    case INSTR_SETA:     I1("seta", source); break;
    case INSTR_SETG:     I1("setg", source); break;
    case INSTR_SETAE:    I1("setae", source); break;
    case INSTR_SETGE:    I1("setge", source); break;
    case INSTR_TEST:     S2("test", wd, source, destin); break;
    case INSTR_CMP:      S2("cmp", wd, source, destin); break;
    case INSTR_LEA:      S2("lea", wd, source, destin); break;
    case INSTR_PUSH:     S1("push", ws, source); break;
    case INSTR_JMP:      I1("jmp", source); break;
    case INSTR_JZ:       I1("jz", source); break;
    case INSTR_JA:       I1("ja", source); break;
    case INSTR_JG:       I1("jg", source); break;
    case INSTR_JAE:      I1("jae", source); break;
    case INSTR_JGE:      I1("jge", source); break;
    case INSTR_CALL:
        if (instr.optype == OPT_REG)
            out("\tcall\t*%s\n", source);
        else
            I1("call", source);
        break;
    case INSTR_LEAVE:    I0("leave"); break;
    case INSTR_RET:      I0("ret"); break;
    case INSTR_REP_MOVS: I0("rep movsq"); break;
    }

    return 0;
}

int asm_data(struct immediate data)
{
    switch (data.type) {
    case IMM_INT:
        if (data.w == 1)
            out("\t.byte\t%d\n", data.d.byte);
        else if (data.w == 2)
            out("\t.short\t%d\n", data.d.word);
        else if (data.w == 4)
            out("\t.int\t%d\n", data.d.dword);
        else
            out("\t.quad\t%ld\n", data.d.qword);
        break;
    case IMM_ADDR:
        assert(data.d.addr.sym);
        if (data.d.addr.disp) {
            out("\t.quad\t%s%c%d\n", sym_name(data.d.addr.sym),
                data.d.addr.disp < 0 ? '-' : '+',
                data.d.addr.disp);
        } else
            out("\t.quad\t%s\n", sym_name(data.d.addr.sym));
        break;
    case IMM_STRING:
        out("\t.string\t\"");
        output_escaped_string(data.d.string);
        out("\"\n");
        break;
    }
    return 0;
}

int asm_flush(void)
{
    if (current_symbol) {
        if (is_function(&current_symbol->type) &&
                current_symbol->symtype != SYM_TENTATIVE)
            out("\t.size\t%s, .-%s\n",
                current_symbol->name, current_symbol->name);
        current_symbol = NULL;
    }
    return 0;
}
