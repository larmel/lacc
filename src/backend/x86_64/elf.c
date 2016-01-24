#include "abi.h"
#include "elf.h"

#include <assert.h>

#define SHNUM 10        /* Number of section headers */

#define SHID_ZERO 0
#define SHID_SHSTRTAB 1
#define SHID_STRTAB 2
#define SHID_SYMTAB 3
#define SHID_RELA_TEXT 4
#define SHID_RELA_DATA 5
#define SHID_BSS 6
#define SHID_DATA 7
#define SHID_RODATA 8
#define SHID_TEXT 9

#define SHDR_CHAIN_OFFSET(a, b) \
    shdr[b].sh_offset = shdr[a].sh_offset + shdr[a].sh_size

#define symtab_index_of(s) ((s)->stack_offset)
#define symtab_lookup(s) (&sbuf[SHID_SYMTAB].sym[(s)->stack_offset])

FILE *object_file_output;

static Elf64_Ehdr header = {
    {
        '\x7f', 'E', 'L', 'F',  /* El_MAG */
        ELFCLASS64,             /* El_CLASS */
        ELFDATA2LSB,            /* El_DATA */
        EV_CURRENT,             /* El_VERSION */
        ELFOSABI_SYSV,          /* El_OSABI */
        0,                      /* El_ABIVERSION */
        0,                      /* El_PAD */
        sizeof(header.e_ident)  /* El_NIDENT */
    },
    ET_REL,             /* Relocatable file type */
    0x3E,               /* Machine type x86_64 */
    1,                  /* Version */
    0x0,                /* Entry point address */
    0x0,                /* Program header offset */
    sizeof(header),     /* Section header offset */
    0x0,                /* Flags */
    sizeof(header),
    0x0,                /* Program header size */
    0,                  /* Number of program header entries */
    sizeof(Elf64_Shdr), /* e_shentsize */
    SHNUM,              /* e_shnum, number of section headers */
    SHID_SHSTRTAB       /* e_shstrndx, index of shstrtab */
};

static char *shname[] = {
    NULL,
    ".shstrtab",
    ".strtab",
    ".symtab",
    ".rela.text",
    ".rela.data",
    ".bss",
    ".data",
    ".rodata",
    ".text"
};

static Elf64_Shdr shdr[] = {
    {0},                /* First section header must contain all-zeroes */
    { /* .shstrtab */
        0,              /* sh_name, index into shstrtab */
        SHT_STRTAB,     /* sh_type */
        0,              /* sh_flags */
        0x0,            /* sh_addr */
        sizeof(Elf64_Ehdr) + SHNUM * sizeof(Elf64_Shdr),
        0,              /* sh_size (TODO) */
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        1,              /* sh_addralign */
        0               /* sh_entsize */
    },
    { /* .strtab */
        0,              /* sh_name, index into shstrtab */
        SHT_STRTAB,     /* sh_type */
        0,              /* sh_flags */
        0x0,            /* sh_addr */
        0,              /* sh_offset (TODO) */
        0,              /* sh_size (TODO) */
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        1,              /* sh_addralign */
        0               /* sh_entsize */
    },
    { /* .symtab */
        0,              /* sh_name, index into shstrtab */
        SHT_SYMTAB,     /* sh_type */
        0,              /* sh_flags */
        0x0,            /* sh_addr */
        0,              /* sh_offset (TODO) */
        0,              /* sh_size (TODO) */
        SHID_STRTAB,    /* sh_link, section number of strtab */
        0,              /* sh_info, index of first non-local symbol (x) */
        4,              /* sh_addralign */
        sizeof(Elf64_Sym)
    },
    { /* .rela.text */
        0,              /* sh_name, index into shstrtab */
        SHT_RELA,       /* sh_type */
        0x0,
        0x0,            /* Virtual address */
        0x0,            /* Offset in file (TODO!) */
        0,              /* Size of section (TODO!) */
        SHID_SYMTAB,    /* sh_link, symbol table referenced by relocations */
        SHID_TEXT,      /* sh_info, section which relocations apply */
        8,              /* sh_addralign */
        sizeof(Elf64_Rela)
    },
    { /* .rela.data */
        0,              /* sh_name, index into shstrtab */
        SHT_RELA,       /* sh_type */
        0x0,
        0x0,            /* Virtual address */
        0x0,            /* Offset in file (TODO!) */
        0,              /* Size of section (TODO!) */
        SHID_SYMTAB,    /* sh_link, symbol table referenced by relocations */
        SHID_DATA,      /* sh_info, section which relocations apply */
        8,              /* sh_addralign */
        sizeof(Elf64_Rela)
    },
    { /* .bss */
        0,              /* sh_name, index into shstrtab */
        SHT_NOBITS,     /* Section type */
        SHF_WRITE | SHF_ALLOC,
        0x0,            /* Virtual address */
        0x0,            /* Offset in file (TODO!) */
        0,              /* Size of section (TODO!) */
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        4,              /* sh_addralign */
        0               /* sh_entsize */
    },
    { /* .data */
        0,              /* sh_name, index into shstrtab */
        SHT_PROGBITS,   /* Section type */
        SHF_WRITE | SHF_ALLOC,
        0x0,            /* Virtual address */
        0x0,            /* Offset in file (TODO!) */
        0,              /* Size of section (TODO!) */
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        4,              /* sh_addralign */
        0               /* sh_entsize */
    },
    { /* .rodata */
        0,              /* sh_name, index into shstrtab */
        SHT_PROGBITS,   /* sh_type */
        SHF_ALLOC,
        0x0,            /* Virtual address */
        0x0,            /* Offset in file (TODO!) */
        0,              /* Size of section (TODO!) */
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        16,             /* sh_addralign */
        0               /* sh_entsize */
    },
    { /* .text */
        0,              /* sh_name, index into shstrtab */
        SHT_PROGBITS,   /* sh_type */
        SHF_EXECINSTR | SHF_ALLOC,
        0x0,            /* Virtual address */
        0x0,            /* Offset in file (TODO!) */
        0,              /* Size of section (TODO!) */
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        16,             /* sh_addralign */
        0               /* sh_entsize */
    }
};

/* Data associated with each section.
 */
static union {
    unsigned char *data;
    Elf64_Sym *sym;
    Elf64_Rela *rela;
} sbuf[SHNUM];

/* Pending relocations, waiting for sym->stack_offset to be resolved to index
 * into .symtab.
 */
static struct pending_relocation {
    const struct symbol *symbol;
    enum rel_type type;
    int section;                /* section id of .rela.X */
    int offset;                 /* offset into .text */
    int addend;                 /* offset into symbol ? */
} *prl;

static int
    n_rela_data,
    n_rela_text;

/* Keep track of function being assembled, updating st_size after each
 * instruction.
 */
static Elf64_Sym *current_function_entry;

/* List of pending global symbols, not yet added to .symtab. All globals have
 * to come after LOCAL symbols, according to spec. Also, ld will segfault(!)
 * otherwise.
 */
static struct {
    struct symbol *sym;
    Elf64_Sym entry;
} *globals;
static int n_globals;

/* Text section contains offsets to labels, also in text. Forward references
 * cannot be resolved immediately, as translation is single pass. Store offsets
 * into .text, paired with symbol (label) which offsets should be calculated.
 */
static struct pending_displacement {
    const struct symbol *label;
    int text_offset;
} *toff;
static int n_toff;

/* Write bytes to section. If ptr is NULL, fill with zeros.
 */
static int elf_section_write(int shid, const void *ptr, size_t n)
{
    size_t offset;
    unsigned char **data;

    assert(0 < shid && shid < SHNUM);
    assert(
        shdr[shid].sh_type == SHT_STRTAB ||
        shdr[shid].sh_type == SHT_PROGBITS ||
        shdr[shid].sh_type == SHT_NOBITS);

    offset = shdr[shid].sh_size;
    if (shdr[shid].sh_type != SHT_NOBITS) {
        data = &sbuf[shid].data;
        *data = realloc(*data, offset + n);
        if (ptr)
            memcpy(*data + offset, ptr, n);
        else
            memset(*data + offset, 0, n);
    }

    shdr[shid].sh_size += n;
    return offset;
}

/* Align data section to specified number of bytes. Following calls to
 * elf_section_write start at this alignment. Padding is filled with zero.
 */
static int elf_section_align(int shid, int align)
{
    size_t offset;
    assert(0 < shid && shid < SHNUM);
    assert(
        shdr[shid].sh_type == SHT_STRTAB ||
        shdr[shid].sh_type == SHT_PROGBITS ||
        shdr[shid].sh_type == SHT_NOBITS);

    offset = shdr[shid].sh_size;
    if (offset % align != 0)
        elf_section_write(shid, NULL, align - (offset % align));
    return offset;
}

/* Add entry to .symtab, returning index.
 */
static int elf_symtab_add(Elf64_Sym entry)
{
    static Elf64_Sym default_symbols[] = {
        {0},
        {0, (STB_LOCAL << 4) | STT_SECTION, 0, SHID_DATA, 0, 0},
        {0, (STB_LOCAL << 4) | STT_SECTION, 0, SHID_TEXT, 0, 0}
    };

    Elf64_Sym **symtab;
    int i;

    symtab = &sbuf[SHID_SYMTAB].sym;
    if (!*symtab) {
        *symtab = calloc(1, sizeof(default_symbols));
        *symtab = memcpy(*symtab, default_symbols, sizeof(default_symbols));
        shdr[SHID_SYMTAB].sh_size = sizeof(default_symbols);
    }

    i = shdr[SHID_SYMTAB].sh_size / sizeof(Elf64_Sym);
    shdr[SHID_SYMTAB].sh_size += sizeof(Elf64_Sym);
    *symtab = realloc(*symtab, shdr[SHID_SYMTAB].sh_size);
    (*symtab)[i] = entry;

    /* All STB_LOCAL must come before STB_GLOBAL. Index of the first non-local
     * symbol is stored in section header field. */
    if (entry.st_info >> 4 == STB_GLOBAL && !shdr[SHID_SYMTAB].sh_info) {
        shdr[SHID_SYMTAB].sh_info = i;
    } else
        assert((entry.st_info >> 4) == ((*symtab)[i - 1].st_info >> 4));

    return i;
}

/* Associate symbol with symtab entry. Internal symbols are added to table right
 * away, but global symbols have to be buffered and flushed at the end. (Mis-)
 * use member for storing index into ELF symbol table. Stack offset is otherwise
 * only used for local variables, which will not live in this symbol table.
 */
static void elf_symtab_assoc(struct symbol *sym, Elf64_Sym entry)
{
    if (sym->linkage == LINK_INTERN) {
        sym->stack_offset = elf_symtab_add(entry);
        if (is_function(&sym->type))
            current_function_entry = &sbuf[SHID_SYMTAB].sym[sym->stack_offset];
    } else {
        assert((entry.st_info >> 4) == STB_GLOBAL);
        globals = realloc(globals, (n_globals + 1) * sizeof(*globals));
        globals[n_globals].sym = sym;
        globals[n_globals].entry = entry;
        if (is_function(&sym->type))
            current_function_entry = &globals[n_globals].entry;
        n_globals += 1;
    }
}

/* Write global symtab entries to table.
 */
static void flush_symtab_globals(void)
{
    int i;
    for (i = 0; i < n_globals; ++i)
        globals[i].sym->stack_offset = elf_symtab_add(globals[i].entry);
}

/* Add string to strtab section, returning its offset into the section for use
 * in references.
 */
static int elf_strtab_add(int shid, const char *str)
{
    int pos;

    assert(0 < shid && shid < SHNUM);
    assert(shdr[shid].sh_type == SHT_STRTAB);

    if (!shdr[shid].sh_size)
        elf_section_write(shid, NULL, 1);

    pos = shdr[shid].sh_size;
    elf_section_write(shid, str, strlen(str) + 1);

    return pos;
}

static void elf_add_reloc(struct pending_relocation entry)
{
    if (entry.section == SHID_RELA_TEXT)
        n_rela_text++;
    else {
        assert(entry.section == SHID_RELA_DATA);
        n_rela_data++;
    }
    prl = realloc(prl, (n_rela_text + n_rela_data) * sizeof(*prl));
    prl[n_rela_text + n_rela_data - 1] = entry;
}

void elf_add_reloc_text(
    const struct symbol *symbol,
    enum rel_type type,
    int offset,
    int addend)
{
    struct pending_relocation r = {0};
    r.symbol = symbol;
    r.type = type;
    r.section = SHID_RELA_TEXT;
    r.offset = shdr[SHID_TEXT].sh_size + offset;
    r.addend = addend;
    elf_add_reloc(r);
}

static void elf_add_reloc_data(
    const struct symbol *symbol,
    enum rel_type type,
    int addend)
{
    struct pending_relocation r = {0};
    r.symbol = symbol;
    r.type = type;
    r.section = SHID_RELA_DATA;
    r.offset = shdr[SHID_DATA].sh_size;
    r.addend = addend;
    elf_add_reloc(r);
}

/* Construct relocation entries from pending relocations. Invoked with flush(),
 * after all data and code is processed. It is important that this is called
 * once all symbols have been written to symtab, as it relies on stack_offset
 * pointing to symtab entry index.
 */
static void flush_relocations(void)
{
    Elf64_Rela *entry, *data_entry, *text_entry;
    int i;

    sbuf[SHID_RELA_TEXT].rela = calloc(n_rela_text, sizeof(Elf64_Rela));
    shdr[SHID_RELA_TEXT].sh_size = n_rela_text * sizeof(Elf64_Rela);
    text_entry = sbuf[SHID_RELA_TEXT].rela;

    sbuf[SHID_RELA_DATA].rela = calloc(n_rela_data, sizeof(Elf64_Rela));
    shdr[SHID_RELA_DATA].sh_size = n_rela_data * sizeof(Elf64_Rela);
    data_entry = sbuf[SHID_RELA_DATA].rela;

    for (i = 0; i < n_rela_text + n_rela_data; ++i) {
        assert(prl[i].type != R_X86_64_NONE);
        if (prl[i].section == SHID_RELA_DATA)
            entry = data_entry++;
        else {
            assert(prl[i].section == SHID_RELA_TEXT);
            entry = text_entry++;
        }

        entry->r_offset = prl[i].offset;
        entry->r_addend = prl[i].addend;
        entry->r_info =
            ELF64_R_INFO(symtab_index_of(prl[i].symbol), prl[i].type);

        /* Subtract 4 to account for the size occupied by the relocation
         * slot itself, it takes up 4 bytes in the instruction. */
        if (prl[i].type == R_X86_64_PC32)
            entry->r_addend -= 4;
    }
}

/* Must be called before writing text segment. Overwrite locations with offsets
 * now found in stack_offset member of label symbols.
 */
static void flush_text_displacements(void)
{
    int i, *ptr;
    const struct pending_displacement *entry;

    if (!toff)
        return;

    assert(n_toff);
    for (i = 0; i < n_toff; ++i) {
        entry = &toff[i];
        assert(entry->label->stack_offset);

        ptr = (int *) (sbuf[SHID_TEXT].data + entry->text_offset);
        *ptr += entry->label->stack_offset - entry->text_offset;
    }
    free(toff);
    toff = NULL;
    n_toff = 0;
}

int elf_text_displacement(const struct symbol *label, int instr_offset)
{
    assert(label->symtype == SYM_LABEL);

    if (label->stack_offset) {
        return label->stack_offset - shdr[SHID_TEXT].sh_size - instr_offset;
    }

    toff = realloc(toff, (n_toff + 1) * sizeof(*toff));
    toff[n_toff].label = label;
    toff[n_toff].text_offset = shdr[SHID_TEXT].sh_size + instr_offset;
    n_toff += 1;
    return 0;
}

int elf_symbol(const struct symbol *sym)
{
    Elf64_Sym entry = {0};
    assert(sym->linkage != LINK_NONE);
    assert(!sym->stack_offset);

    if (sym->symtype == SYM_LABEL) {
        ((struct symbol *) sym)->stack_offset = shdr[SHID_TEXT].sh_size;
        return 0;
    }

    entry.st_name = elf_strtab_add(SHID_STRTAB, sym_name(sym));
    entry.st_info = (sym->linkage == LINK_INTERN)
        ? STB_LOCAL << 4 : STB_GLOBAL << 4;

    if (is_function(&sym->type)) {
        entry.st_info |= STT_FUNC;
        if (sym->symtype == SYM_DEFINITION) {
            entry.st_shndx = SHID_TEXT;
            entry.st_value = shdr[SHID_TEXT].sh_size;
        }
        /* st_size is updated while assembling instructions. */
    } else if (sym->symtype == SYM_DEFINITION) {
        elf_section_align(SHID_DATA, sym_alignment(sym));
        entry.st_shndx = SHID_DATA;
        entry.st_size = size_of(&sym->type);
        entry.st_value = shdr[SHID_DATA].sh_size;
        entry.st_info |= STT_OBJECT;
    } else if (sym->symtype == SYM_STRING_VALUE) {
        elf_section_align(SHID_RODATA, sym_alignment(sym));
        entry.st_shndx = SHID_RODATA;
        entry.st_size = size_of(&sym->type);
        entry.st_value = shdr[SHID_RODATA].sh_size;
        entry.st_info |= STT_OBJECT;

        /* String value symbols contain the actual string value; write to
         * .rodata immediately. */
        elf_section_write(SHID_RODATA, sym->string_value, entry.st_size);
    } else if (sym->linkage == LINK_INTERN) {
        elf_section_align(SHID_BSS, sym_alignment(sym));
        entry.st_shndx = SHID_BSS;
        entry.st_size = size_of(&sym->type);
        entry.st_value = shdr[SHID_BSS].sh_size;
        entry.st_info |= STT_OBJECT;
        shdr[SHID_BSS].sh_size += entry.st_size;
    } else if (sym->symtype == SYM_TENTATIVE) {
        assert(sym->linkage == LINK_EXTERN);
        entry.st_shndx = SHN_COMMON;
    }

    elf_symtab_assoc((struct symbol *) sym, entry);
    return 0;
}

int elf_text(struct instruction instr)
{
    struct code c = encode(instr);
    assert(current_function_entry);

    if (c.val[0] != 0x90) {
        elf_section_write(SHID_TEXT, &c.val, c.len);
        current_function_entry->st_size += c.len;
    }

    return 0;
}

int elf_data(struct immediate imm)
{
    const void *ptr = NULL;
    size_t w = imm.w;

    switch (imm.type) {
    case IMM_INT:
        if (imm.w == 1)
            ptr = &imm.d.byte;
        else if (imm.w == 2)
            ptr = &imm.d.word;
        else if (imm.w == 4)
            ptr = &imm.d.dword;
        else
            ptr = &imm.d.qword;
        break;
    case IMM_ADDR:
        assert(imm.d.addr.sym);
        assert(imm.w == 8);
        elf_add_reloc_data(imm.d.addr.sym, R_X86_64_64, imm.d.addr.disp);
        break;
    case IMM_STRING:
        assert(w == strlen(imm.d.string) + 1);
        ptr = imm.d.string;
        break;
    }

    return elf_section_write(SHID_DATA, ptr, w);
}

int elf_flush(void)
{
    int i;

    for (i = 1; i < SHNUM; ++i)
        shdr[i].sh_name = elf_strtab_add(SHID_SHSTRTAB, shname[i]);

    /* Write remaining data to section buffers. */
    flush_symtab_globals();
    flush_relocations();
    flush_text_displacements();

    /* Add padding to force proper alignment. */
    elf_section_align(SHID_SHSTRTAB, 0x10);
    elf_section_align(SHID_STRTAB, 0x10);
    elf_section_align(SHID_STRTAB, 0x10);
    elf_section_align(SHID_DATA, 0x10);
    elf_section_align(SHID_RODATA, 0x10);

    /* Fill in missing offset and size values */
    SHDR_CHAIN_OFFSET(SHID_SHSTRTAB, SHID_STRTAB);
    SHDR_CHAIN_OFFSET(SHID_STRTAB, SHID_SYMTAB);
    SHDR_CHAIN_OFFSET(SHID_SYMTAB, SHID_RELA_TEXT);
    SHDR_CHAIN_OFFSET(SHID_RELA_TEXT, SHID_RELA_DATA);
    SHDR_CHAIN_OFFSET(SHID_RELA_DATA, SHID_BSS);
    SHDR_CHAIN_OFFSET(SHID_RELA_DATA, SHID_DATA);
    SHDR_CHAIN_OFFSET(SHID_DATA, SHID_RODATA);
    SHDR_CHAIN_OFFSET(SHID_RODATA, SHID_TEXT);

    /* Write headers and section data. */
    fwrite(&header, sizeof(header), 1, object_file_output);
    fwrite(&shdr, sizeof(shdr), 1, object_file_output);
    for (i = 1; i < SHNUM; ++i) {
        if (!shdr[i].sh_size)
            continue;

        switch (shdr[i].sh_type) {
        case SHT_PROGBITS:
        case SHT_STRTAB:
            fwrite(sbuf[i].data, shdr[i].sh_size, 1, object_file_output);
            break;
        case SHT_SYMTAB:
            fwrite(sbuf[i].sym, shdr[i].sh_size, 1, object_file_output);
            break;
        case SHT_RELA:
            fwrite(sbuf[i].rela, shdr[i].sh_size, 1, object_file_output);
            break;
        default:
            assert(shdr[i].sh_type == SHT_NOBITS);
            break;
        }
    }

    return 0;
}
