#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "assembler.h"
#include "compile.h"
#include "x86_64/encoding.h"
#include <lacc/context.h>

#include <assert.h>
#include <ctype.h>

enum asm_token_type {
    ASM_END,
    ASM_REG,            /* %%rax */
    ASM_IDENT,          /* mov, .file */
    ASM_OP,             /* %0, %[foo] */
    ASM_LABEL,          /* %l2 */
    ASM_CONSTANT,       /* $42 */
    ASM_NUMBER,         /* 42 */
    ASM_MINUS = '-',
    ASM_COMMA = ',',
    ASM_LPAREN = '(',
    ASM_RPAREN = ')',
    ASM_LBRACKET = '[',
    ASM_RBRACKET = ']'
};

struct asm_token {
    enum asm_token_type type;
    const char *str;
    size_t len;
    long val;
};

static array_of(struct asm_operand) operands;
static array_of(struct block *) targets;

static void skip_whitespace(const char *line, const char **endptr)
{
    while (isspace(*line)) {
        line++;
    }

    *endptr = line;
}

static struct asm_token asmtok(const char *line, const char **endptr)
{
    const char *ptr, *raw;
    int i, len;
    struct asm_operand *op;
    struct asm_token t = {0};

    skip_whitespace(line, &line);
    ptr = line;

    if (isalpha(*ptr) || *ptr == '.' || *ptr == '_') {
        t.type = ASM_IDENT;
        do ptr++;
        while (isalnum(*ptr) || *ptr == '.' || *ptr == '_');
    } else if (isdigit(*ptr)
        || ((*ptr == '-' || *ptr == '+') && isdigit(ptr[1])))
    {
        t.type = ASM_NUMBER;
        t.val = strtol(ptr, (char **) &ptr, 10);
    } else switch (*ptr) {
    case '[':
    case ']':
    case '(':
    case ')':
    case ',':
    case '-':
        t.type = *ptr++;
        break;
    case '$':
        t.type = ASM_CONSTANT;
        ptr++;
        while (isdigit(*ptr)) {
            ptr++;
        }
        break;
    case '*':
        if (ptr[1] == '%' && ptr[2] == '%') {
            t.type = ASM_REG;
            ptr += 3;
            line = ptr;
            while (isalnum(*ptr)) {
                ptr++;
            }
        } else goto fail;
        break;
    case '%':
        if (ptr[1] == '%') {
            t.type = ASM_REG;
            ptr += 2;
            line = ptr;
            while (isalnum(*ptr)) {
                ptr++;
            }
        } else if (isdigit(ptr[1])) {
            t.type = ASM_OP;
            ptr += 1;
            line = ptr;
            t.val = strtol(ptr, (char **) &ptr, 10);
            if (t.val < 0 || t.val >= array_len(&operands)) {
                error("Invalid operand reference, %ld is out of range.", t.val);
                exit(1);
            }
        } else if (ptr[1] == '[') {
            ptr += 2;
            for (i = 0; ptr[i] && ptr[i] != ']'; i++)
                ;
            if (!ptr[i]) {
                error("Invalid symbolic reference, missing trailing ']'.");
                exit(1);
            }
            len = i;
            for (i = 0; i < array_len(&operands); ++i) {
                op = &array_get(&operands, i);
                raw = str_raw(op->alias);
                if (len == str_len(op->alias) && !strncmp(raw, ptr, len)) {
                    t.type = ASM_OP;
                    ptr += len + 2;
                    line = ptr;
                    t.val = i;
                    break;
                }
            }
            if (i == array_len(&operands)) {
                error("Symbolic reference does not match any operand.");
                exit(1);
            }
        } else if (ptr[1] == 'l') {
            t.type = ASM_LABEL;
            ptr += 2;
            t.val = strtol(ptr, (char **) &ptr, 10);
            t.val -= array_len(&operands);
            if (t.val < 0 || t.val >= array_len(&targets)) {
                error("Invalid label reference.");
                exit(1);
            }
        } else goto fail;
        break;
    default: fail:
        error("Unexpected token %s.", line);
        exit(1);
        break;
    }

    t.str = line;
    t.len = ptr - line;
    *endptr = ptr;
    return t;
}

static enum reg parse_asm_int_reg(const char *str, size_t len, int *w)
{
    static struct {
        const char *name;
        enum reg r;
        int w;
    } regs[] = {
        {"al", AX, 1}, {"ax", AX, 2}, {"eax", AX, 4}, {"rax", AX, 8},
        {"bl", BX, 1}, {"bx", BX, 2}, {"ebx", BX, 4}, {"rbx", BX, 8},
        {"cl", CX, 1}, {"cx", CX, 2}, {"ecx", CX, 4}, {"rcx", CX, 8},
        {"dl", DX, 1}, {"dx", DX, 2}, {"edx", DX, 4}, {"rdx", DX, 8},
        {"spl", SP, 1}, {"sp", SP, 2}, {"esp", SP, 4}, {"rsp", SP, 8},
        {"bpl", BP, 1}, {"bp", BP, 2}, {"ebp", BP, 4}, {"rbp", BP, 8},
        {"sil", SI, 1}, {"si", SI, 2}, {"esi", SI, 4}, {"rsi", SI, 8},
        {"dil", DI, 1}, {"di", DI, 2}, {"edi", DI, 4}, {"rdi", DI, 8},
        {"r8b", R8, 1}, {"r8w", R8, 2}, {"r8d", R8, 4}, {"r8", R8, 8},
        {"r9b", R9, 1}, {"r9w", R9, 2}, {"r9d", R9, 4}, {"r9", R9, 8},
        {"r10b", R10, 1}, {"r10w", R10, 2}, {"r10d", R10, 4}, {"r10", R10, 8},
        {"r11b", R11, 1}, {"r11w", R11, 2}, {"r11d", R11, 4}, {"r11", R11, 8},
        {"r12b", R12, 1}, {"r12w", R12, 2}, {"r12d", R12, 4}, {"r12", R12, 8},
        {"r13b", R13, 1}, {"r13w", R13, 2}, {"r13d", R13, 4}, {"r13", R13, 8},
        {"r14b", R14, 1}, {"r14w", R14, 2}, {"r14d", R14, 4}, {"r14", R14, 8},
        {"r15b", R15, 1}, {"r15w", R15, 2}, {"r15d", R15, 4}, {"r15", R15, 8},
    };

    int i;

    for (i = 0; i < 4*16; ++i) {
        if (len != strlen(regs[i].name)) continue;
        if (!strncmp(regs[i].name, str, len)) {
            *w = regs[i].w;
            return regs[i].r;
        }
    }

    error("Invalid assembly register %s.", str);
    exit(1);
}

static struct registr parse__asm__register(const char *str, size_t len)
{
    long d;
    char *endptr;
    struct registr reg = {0};

    if (!strncmp("xmm", str, 3)) {
        d = strtol(str + 3, &endptr, 10);
        if (endptr == str + len && d >= 0 && d <= 15) {
            reg.width = 0; /* either 4 or 8 */
            reg.r = XMM0 + d;
        } else {
            error("Invalid SSE register.");
            exit(1);
        }
    } else {
        reg.r = parse_asm_int_reg(str, len, &reg.width);
    }

    return reg;
}

INTERNAL enum reg get_clobbered_register(String clobber)
{
    const char *str;
    size_t len;
    struct registr reg;

    str = str_raw(clobber);
    len = str_len(clobber);
    if (str[0] == '%') {
        str++;
        len--;
    }

    reg = parse__asm__register(str, len);
    return reg.r;
}

/*
 * Parse part of address inside parenthesis. Consumes the closing
 * parenthesis.
 *
 *     -8(%rbp, %rax, 4)
 *       (,%rax,4)
 *       (%0)
 */
static int parse_asm_address(
    const char *line,
    const char **endptr,
    struct address *addr)
{
    int w;
    union operand op;
    enum instr_optype opt;
    struct asm_operand asmop;
    struct asm_token t;

    /* Base */
    t = asmtok(line, &line);
    if (t.type == ASM_REG) {
        addr->base = parse_asm_int_reg(t.str, t.len, &w);
        t = asmtok(line, &line);
        if (t.type == ')') {
            *endptr = line;
            return 1;
        }
    } else if (t.type == ASM_OP) {
        asmop = array_get(&operands, t.val);
        opt = allocation(asmop.variable, &op);
        if (opt != OPT_REG) {
            error("Operand %ld must be register allocated.", t.val);
            exit(1);
        }
        addr->base = op.reg.r;
    } else if (t.type != ',') {
        *endptr = line;
        return 0;
    }

    /* Index and scale. */
    t = asmtok(line, &line);
    if (t.type == ASM_REG) {
        addr->index = parse_asm_int_reg(t.str, t.len, &w);
        t = asmtok(line, &line);
        if (t.type != ',') {
            *endptr = line;
            return 1;
        }

        addr->scale = strtol(line, (char **) &line, 10);
        t = asmtok(line, &line);
    }

    *endptr = line;
    return t.type == ')';
}

static enum instr_optype parse__asm__operand(
    const char *line,
    const char **endptr,
    union operand *op)
{
    long l;
    enum instr_optype opt;
    struct asm_operand asmop;
    struct block *target;
    struct asm_token t;

    opt = OPT_NONE;
    switch ((t = asmtok(line, &line)).type) {
    case ASM_NUMBER:
        op->mem.addr.displacement = t.val;
        t = asmtok(line, &line);
        if (t.type != '(') {
            error("Expected '(' after displacement in address operand.");
            exit(1);
        }
    case '(':
        opt = OPT_MEM;
        if (!parse_asm_address(line, &line, &op->mem.addr)) {
            error("Invalid memory address operand.");
            exit(1);
        }
        break;
    case ASM_CONSTANT:
        assert(*t.str == '$');
        l = strtol(t.str + 1, NULL, 0);
        op->imm.type = IMM_INT;
        op->imm.d.qword = l;
        opt = OPT_IMM;
        break;
    case ASM_REG:
        op->reg = parse__asm__register(t.str, t.len);
        opt = OPT_REG;
        break;
    case ASM_OP:
        asmop = array_get(&operands, t.val);
        opt = allocation(asmop.variable, op);
        break;
    case ASM_LABEL:
        target = array_get(&targets, t.val);
        opt = OPT_IMM;
        op->imm.type = IMM_ADDR;
        op->imm.d.addr.type = ADDR_NORMAL;
        op->imm.d.addr.sym = target->label;
        op->imm.width = 8;
        break;
    default:
        error("Invalid assembly operand %s.", line);
        exit(1);
        break;
    }

    *endptr = line;
    return opt;
}

static struct instruction parse__asm__instruction(
    const char *line,
    const char **endptr)
{
    struct instruction instr = {0};
    enum instr_optype opt1, opt2;
    struct asm_token t;

    t = asmtok(line, &line);
    skip_whitespace(line, &line);

    if (*line == '\n') {
        instr.optype = OPT_NONE;
    } else {
        instr.optype = parse__asm__operand(line, &line, &instr.source);
        skip_whitespace(line, &line);
        if (*line == ',') {
            opt1 = instr.optype;
            opt2 = parse__asm__operand(line + 1, &line, &instr.dest);
            if (opt1 == OPT_REG && opt2 == OPT_REG) {
                instr.optype = OPT_REG_REG;
            } else if (opt1 == OPT_REG && opt2 == OPT_MEM) {
                instr.optype = OPT_REG_MEM;
            } else if (opt1 == OPT_MEM && opt2 == OPT_REG) {
                instr.optype = OPT_MEM_REG;
            } else if (opt1 == OPT_IMM && opt2 == OPT_REG) {
                instr.optype = OPT_IMM_REG;
            } else if (opt1 == OPT_IMM && opt2 == OPT_MEM) {
                instr.optype = OPT_IMM_MEM;
            } else {
                error("Invalid combination of operands.");
                exit(1);
            }
        }
    }

    if (!mnemonic_match_operands(t.str, t.len, &instr)) {
        error("Unrecognized instruction %s", t.str);
        exit(1);
    }

    *endptr = line;
    return instr;
}

INTERNAL size_t read_line(
    const char *line,
    size_t len,
    char *ptr,
    int *linecount);

INTERNAL int assemble_inline(
    struct asm_statement st,
    int (*emit)(struct instruction))
{
    struct instruction instr;
    struct asm_operand op;
    struct block *target;
    const char *str, *ptr;
    size_t len, read;
    char *buf;
    int c, i;

    for (i = 0; i < array_len(&st.operands); ++i) {
        op = array_get(&st.operands, i);
        array_push_back(&operands, op);
    }

    for (i = 0; i < array_len(&st.targets); ++i) {
        target = array_get(&st.targets, i);
        array_push_back(&targets, target);
    }

    len = str_len(st.template);
    str = str_raw(st.template);
    buf = calloc(len + 2, sizeof(*buf));

    while ((read = read_line(str, len, buf + 1, &c)) != 0) {
        ptr = buf + 1;
        str += read;
        len -= read;
        skip_whitespace(ptr, &ptr);
        if (*ptr) {
            instr = parse__asm__instruction(ptr, &ptr);
            emit(instr);
            skip_whitespace(ptr, &ptr);
            if (*ptr) {
                error("Stray token at end of assembly instruction.");
                exit(1);
            }
        }
    }

    array_clear(&operands);
    array_clear(&targets);
    free(buf);
    return 0;
}
