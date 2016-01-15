#include "abi.h"
#include "elf.h"

#include <assert.h>

#define SHNUM 8     /* Number of section headers */

#define SHID_ZERO 0
#define SHID_SHSTRTAB 1
#define SHID_STRTAB 2
#define SHID_SYMTAB 3
#define SHID_RELA_TEXT 4
#define SHID_DATA 5
#define SHID_RODATA 6
#define SHID_TEXT 7

#define SHDR_CHAIN_OFFSET(a, b) \
    shdr[b].sh_offset = shdr[a].sh_offset + shdr[a].sh_size

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

static char shstrtab[] =
    "\0.data\0.text\0.shstrtab\0.symtab\0.strtab\0.rodata\0.rela.text\0"
    "\0\0\0\0\0"; /* Make size % 16 = 0 */

static Elf64_Shdr shdr[] = {
    {0},                /* First section header must contain all-zeroes */
    { /* .shstrtab */
        13,             /* sh_name, index into shstrtab */
        SHT_STRTAB,     /* sh_type */
        0,              /* sh_flags */
        0x0,            /* sh_addr */
        sizeof(Elf64_Ehdr) + SHNUM * sizeof(Elf64_Shdr),
        sizeof(shstrtab),
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        1,              /* sh_addralign */
        0               /* sh_entsize */
    },
    { /* .strtab */
        31,             /* sh_name, index into shstrtab */
        SHT_STRTAB,     /* sh_type */
        0,              /* sh_flags */
        0x0,            /* sh_addr */
        sizeof(Elf64_Ehdr) + SHNUM * sizeof(Elf64_Shdr) + sizeof(shstrtab),
        0,              /* sh_size (TODO) */
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        1,              /* sh_addralign */
        0               /* sh_entsize */
    },
    { /* .symtab */
        23,             /* sh_name, index into shstrtab */
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
        47,             /* sh_name, index into shstrtab */
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
    { /* .data */
        1,              /* sh_name, index into shstrtab */
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
        39,             /* sh_name, index into shstrtab */
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
        7,              /* sh_name, index into shstrtab */
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

static unsigned char *data;
static unsigned char *rodata;

/* Write bytes to .data or .rodata section. If ptr is NULL, fill with zeros.
 */
static int elf_data_add(int shid, const char *ptr, size_t n)
{
    size_t offset;
    unsigned char **buf;
    assert(shid == SHID_DATA || shid == SHID_RODATA);

    offset = shdr[shid].sh_size;
    buf = (shid == SHID_DATA) ? &data : &rodata;
    *buf = realloc(*buf, offset + n);
    if (ptr)
        memcpy(*buf + offset, ptr, n);
    else
        memset(*buf + offset, '\0', n);
    shdr[shid].sh_size += n;
    return offset;
}

/* Align .data or .rodata section to specified number of bytes. Following calls
 * to elf_data_add start at this alignment. Padding is filled with zero.
 */
static int elf_data_align(int shid, int align)
{
    size_t offset;
    assert(shid == SHID_DATA || shid == SHID_RODATA);

    offset = shdr[shid].sh_size;
    if (offset % align != 0)
        elf_data_add(shid, NULL, align - (offset % align));
    return offset;
}

static unsigned char *text;

static Elf64_Sym *symtab;

/* Add entry to .symtab, returning index.
 */
static int elf_symtab_add(Elf64_Sym entry)
{
    static Elf64_Sym default_symbols[] = {
        {0},
        {0, (STB_LOCAL << 4) | STT_SECTION, 0, SHID_DATA, 0, 0},
        {0, (STB_LOCAL << 4) | STT_SECTION, 0, SHID_TEXT, 0, 0}
    };

    int i;

    if (!symtab) {
        symtab = calloc(1, sizeof(default_symbols));
        symtab = memcpy(symtab, default_symbols, sizeof(default_symbols));
        shdr[SHID_SYMTAB].sh_size = sizeof(default_symbols);
    }

    i = shdr[SHID_SYMTAB].sh_size / sizeof(Elf64_Sym);
    shdr[SHID_SYMTAB].sh_size += sizeof(Elf64_Sym);
    symtab = realloc(symtab, shdr[SHID_SYMTAB].sh_size);
    symtab[i] = entry;

    /* All STB_LOCAL must come before STB_GLOBAL. Index of the first non-local
     * symbol is stored in section header field. */
    if (entry.st_info >> 4 == STB_GLOBAL && !shdr[SHID_SYMTAB].sh_info) {
        shdr[SHID_SYMTAB].sh_info = i;
    } else
        assert((entry.st_info >> 4) == (symtab[i - 1].st_info >> 4));

    return i;
}

static char *strtab;

/* Add string to .strtab, returning its offset into the section for use in
 * references.
 */
static int elf_strtab_add(const char *str)
{
    /* Current amount of padding in strtab, filled with zeroes. Section size is 
     * padded to make it align to 0x10. Adding new strings first eat off any
     * redundant padding. */
    static size_t padding;

    size_t
        len = strlen(str) + 1, /* including zero byte */
        off = shdr[SHID_STRTAB].sh_size - padding; /* pos of new string */

    /* First byte should be '\0' */
    if (!off)
        off = 1;

    padding = 0x10 - ((off + len) % 0x10);
    shdr[SHID_STRTAB].sh_size = off + len + padding;
    assert(shdr[SHID_STRTAB].sh_size % 0x10 == 0);

    strtab = realloc(strtab, shdr[SHID_STRTAB].sh_size);
    strcpy(strtab + off, str);
    memset(strtab + off + len, '\0', padding);
    strtab[off - 1] = '\0';

    return off;
}

static Elf64_Rela *rela_text;

/* Pending relocations, waiting for sym->stack_offset to be resolved to index
 * into .symtab once written to data section.
 */
static struct pending_relocation {
    const struct symbol *sym;
    enum rel_type type;
    int offset;                 /* offset into .text */
    int addend;                 /* offset into symbol ? */
} *prl;
static int n_relocs;

/* Add pending relocation to symbol on offset from current .text size. Offset
 * is into instruction being assembled.
 * The relocation entry cannot always be immediately constructed, as the symbol
 * might not have been written to any section yet.
 */
void elf_add_relocation(
    const struct symbol *sym,
    enum rel_type type,
    int section_offset,
    int sym_offset)
{
    struct pending_relocation *entry;

    n_relocs += 1;
    prl = realloc(prl, n_relocs * sizeof(*prl));
    entry = &prl[n_relocs - 1];

    entry->sym = sym;
    entry->type = type;
    entry->offset = section_offset + shdr[SHID_TEXT].sh_size;
    entry->addend = sym_offset; /* - size_of(&sym->type);*/
}

#define symtab_index_of(s) ((s)->stack_offset)
#define symtab_lookup(s) (&symtab[(s)->stack_offset])

/* Construct relocation entries from pending relocations. Invoked with flush(),
 * after all data and code is processed. It is important that this is called
 * after all symbols have been written to symtab, as it relies on stack_offset
 * pointing to symtab entry index.
 */
static void flush_relocations(void)
{
    int i;
    Elf64_Rela *entry;
    const struct symbol *sym;
    enum rel_type type;
    assert(!rela_text);

    rela_text = calloc(n_relocs, sizeof(*rela_text));
    shdr[SHID_RELA_TEXT].sh_size = n_relocs * sizeof(Elf64_Rela);

    for (i = 0; i < n_relocs; ++i) {
        entry = &rela_text[i];
        sym = prl[i].sym;
        type = prl[i].type;
        assert(type == R_X86_64_PC32 || type == R_X86_64_32S);

        entry->r_offset = prl[i].offset;
        entry->r_addend = prl[i].addend;
        entry->r_info = ELF64_R_INFO((long) symtab_index_of(sym), (long) type);

        /* Subtract 4 to account for the size occupied by the relocation
         * slot itself, it takes up 4 bytes in the instruction. */
        if (type == R_X86_64_PC32)
            entry->r_addend -= 4;
    }
}

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
            current_function_entry = &symtab[sym->stack_offset];
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

int elf_symbol(const struct symbol *sym)
{
    Elf64_Sym entry = {0};
    assert(sym->linkage != LINK_NONE);
    assert(!sym->stack_offset);

    if (sym->symtype == SYM_LABEL) {
        ((struct symbol *) sym)->stack_offset = shdr[SHID_TEXT].sh_size;
        return 0;
    }

    entry.st_name = elf_strtab_add(sym_name(sym));
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
        elf_data_align(SHID_DATA, sym_alignment(sym));
        entry.st_shndx = SHID_DATA;
        entry.st_size = size_of(&sym->type);
        entry.st_value = shdr[SHID_DATA].sh_size;
        entry.st_info |= STT_OBJECT;
    } else if (sym->symtype == SYM_STRING_VALUE) {
        elf_data_align(SHID_RODATA, sym_alignment(sym));
        entry.st_shndx = SHID_RODATA;
        entry.st_size = size_of(&sym->type);
        entry.st_value = shdr[SHID_RODATA].sh_size;
        entry.st_info |= STT_OBJECT;

        /* String value symbols contain the actual string value; write to
         * .rodata immediately. */
        elf_data_add(SHID_RODATA, sym->string_value, size_of(&sym->type));
    }

    elf_symtab_assoc((struct symbol *) sym, entry);
    return 0;
}

/* Text section contains offsets to labels, also in text. Forward references
 * cannot be resolved immediately, as translation is single pass. Store offsets
 * into .text, paired with symbol (label) which offsets should be calculated.
 */
static struct pending_displacement {
    const struct symbol *label;
    int text_offset;
} *toff;
static int n_toff;

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

        ptr = (int *) (text + entry->text_offset);
        *ptr += entry->label->stack_offset - entry->text_offset;
    }
    free(toff);
    toff = NULL;
    n_toff = 0;
}

int elf_text(struct instruction instr)
{
    struct code c = encode(instr);
    assert(current_function_entry);

    if (c.val[0] == 0x90)
        return 0;

    text = realloc(text, shdr[SHID_TEXT].sh_size + c.len);
    memcpy(text + shdr[SHID_TEXT].sh_size, &c.val, c.len);

    shdr[SHID_TEXT].sh_size += c.len;
    current_function_entry->st_size += c.len;

    return 0;
}

int elf_data(struct immediate imm)
{
    const void *ptr;
    size_t w = imm.w;

    if (imm.type == IMM_INT) {
        if (imm.w == 1)
            ptr = &imm.d.byte;
        else if (imm.w == 2)
            ptr = &imm.d.word;
        else if (imm.w == 4)
            ptr = &imm.d.dword;
        else
            ptr = &imm.d.qword;
    } else {
        assert(imm.type == IMM_STRING);
        assert(w == strlen(imm.d.string) + 1);
        ptr = imm.d.string;
    }

    return elf_data_add(SHID_DATA, ptr, w);
}

int elf_flush(void)
{
    flush_symtab_globals();
    flush_relocations();
    flush_text_displacements();
    elf_data_align(SHID_DATA, 0x10);
    elf_data_align(SHID_RODATA, 0x10);

    fwrite(&header, sizeof(header), 1, object_file_output);

    /* Fill in missing offset and size values */
    SHDR_CHAIN_OFFSET(SHID_STRTAB, SHID_SYMTAB);
    SHDR_CHAIN_OFFSET(SHID_SYMTAB, SHID_RELA_TEXT);
    SHDR_CHAIN_OFFSET(SHID_RELA_TEXT, SHID_DATA);
    SHDR_CHAIN_OFFSET(SHID_DATA, SHID_RODATA);
    SHDR_CHAIN_OFFSET(SHID_RODATA, SHID_TEXT);

    /* Section headers */
    fwrite(&shdr, sizeof(shdr), 1, object_file_output);

    /* Section data */
    fwrite(shstrtab, shdr[SHID_SHSTRTAB].sh_size, 1, object_file_output);
    fwrite(strtab, shdr[SHID_STRTAB].sh_size, 1, object_file_output);
    fwrite(symtab, shdr[SHID_SYMTAB].sh_size, 1, object_file_output);
    fwrite(rela_text, shdr[SHID_RELA_TEXT].sh_size, 1, object_file_output);
    fwrite(data, shdr[SHID_DATA].sh_size, 1, object_file_output);
    fwrite(rodata, shdr[SHID_RODATA].sh_size, 1, object_file_output);
    fwrite(text, shdr[SHID_TEXT].sh_size, 1, object_file_output);

    return 0;
}
