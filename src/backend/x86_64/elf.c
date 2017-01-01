#include "abi.h"
#include "elf.h"
#include <lacc/array.h>

#include <assert.h>

#define SHNUM 10        /* Number of section headers. */

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

static FILE *object_file_output;

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
    ET_REL,             /* Relocatable file type. */
    0x3E,               /* Machine type x86_64. */
    1,                  /* Version. */
    0x0,                /* Entry point address. */
    0x0,                /* Program header offset. */
    sizeof(header),     /* Section header offset. */
    0x0,                /* Flags. */
    sizeof(header),
    0x0,                /* Program header size. */
    0,                  /* Number of program header entries. */
    sizeof(Elf64_Shdr), /* e_shentsize. */
    SHNUM,              /* e_shnum, number of section headers. */
    SHID_SHSTRTAB       /* e_shstrndx, index of shstrtab. */
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
    {0},                /* First section header must be all-zero. */
    { /* .shstrtab */
        0,              /* sh_name, index into shstrtab. */
        SHT_STRTAB,     /* sh_type. */
        0,              /* sh_flags. */
        0x0,            /* sh_addr. */
        sizeof(Elf64_Ehdr) + SHNUM * sizeof(Elf64_Shdr),
        0,              /* sh_size (TODO). */
        SHN_UNDEF,      /* sh_link. */
        0,              /* sh_info. */
        1,              /* sh_addralign. */
        0               /* sh_entsize. */
    },
    { /* .strtab */
        0,              /* sh_name, index into shstrtab. */
        SHT_STRTAB,     /* sh_type. */
        0,              /* sh_flags. */
        0x0,            /* sh_addr. */
        0,              /* sh_offset (TODO). */
        0,              /* sh_size (TODO). */
        SHN_UNDEF,      /* sh_link. */
        0,              /* sh_info. */
        1,              /* sh_addralign. */
        0               /* sh_entsize. */
    },
    { /* .symtab */
        0,              /* sh_name, index into shstrtab. */
        SHT_SYMTAB,     /* sh_type. */
        0,              /* sh_flags. */
        0x0,            /* sh_addr. */
        0,              /* sh_offset (TODO). */
        0,              /* sh_size (TODO). */
        SHID_STRTAB,    /* sh_link, section number of strtab. */
        0,              /* sh_info, index of first non-local symbol. */
        4,              /* sh_addralign. */
        sizeof(Elf64_Sym)
    },
    { /* .rela.text */
        0,              /* sh_name, index into shstrtab. */
        SHT_RELA,       /* sh_type. */
        0x0,
        0x0,            /* Virtual address. */
        0x0,            /* Offset in file (TODO). */
        0,              /* Size of section (TODO). */
        SHID_SYMTAB,    /* sh_link, symbol table referenced by reloc. */
        SHID_TEXT,      /* sh_info, section which relocations apply. */
        8,              /* sh_addralign. */
        sizeof(Elf64_Rela)
    },
    { /* .rela.data */
        0,              /* sh_name, index into shstrtab. */
        SHT_RELA,       /* sh_type. */
        0x0,
        0x0,            /* Virtual address. */
        0x0,            /* Offset in file (TODO). */
        0,              /* Size of section (TODO). */
        SHID_SYMTAB,    /* sh_link, symbol table referenced by reloc. */
        SHID_DATA,      /* sh_info, section which relocations apply. */
        8,              /* sh_addralign. */
        sizeof(Elf64_Rela)
    },
    { /* .bss */
        0,              /* sh_name, index into shstrtab. */
        SHT_NOBITS,     /* Section type. */
        SHF_WRITE | SHF_ALLOC,
        0x0,            /* Virtual address. */
        0x0,            /* Offset in file (TODO). */
        0,              /* Size of section (TODO). */
        SHN_UNDEF,      /* sh_link. */
        0,              /* sh_info. */
        4,              /* sh_addralign. */
        0               /* sh_entsize. */
    },
    { /* .data */
        0,              /* sh_name, index into shstrtab. */
        SHT_PROGBITS,   /* Section type. */
        SHF_WRITE | SHF_ALLOC,
        0x0,            /* Virtual address. */
        0x0,            /* Offset in file (TODO). */
        0,              /* Size of section (TODO). */
        SHN_UNDEF,      /* sh_link. */
        0,              /* sh_info. */
        4,              /* sh_addralign. */
        0               /* sh_entsize. */
    },
    { /* .rodata */
        0,              /* sh_name, index into shstrtab. */
        SHT_PROGBITS,   /* sh_type. */
        SHF_ALLOC,
        0x0,            /* Virtual address. */
        0x0,            /* Offset in file (TODO). */
        0,              /* Size of section (TODO). */
        SHN_UNDEF,      /* sh_link. */
        0,              /* sh_info. */
        16,             /* sh_addralign. */
        0               /* sh_entsize. */
    },
    { /* .text */
        0,              /* sh_name, index into shstrtab. */
        SHT_PROGBITS,   /* sh_type. */
        SHF_EXECINSTR | SHF_ALLOC,
        0x0,            /* Virtual address. */
        0x0,            /* Offset in file (TODO). */
        0,              /* Size of section (TODO). */
        SHN_UNDEF,      /* sh_link. */
        0,              /* sh_info. */
        16,             /* sh_addralign. */
        0               /* sh_entsize. */
    }
};

/* Data associated with each section. */
static union {
    unsigned char *data;
    Elf64_Sym *sym;
    Elf64_Rela *rela;
} sbuf[SHNUM];

/*
 * Pending relocations, waiting for sym->stack_offset to be resolved to
 * index into .symtab.
 */
struct pending_relocation {
    const struct symbol *symbol;
    enum rel_type type;
    int section;                /* section id of .rela.X */
    int offset;                 /* offset into .text */
    int addend;                 /* offset into symbol ? */
};

static array_of(struct pending_relocation) pending_relocation_list;

static int
    n_rela_data,
    n_rela_text;

/*
 * Keep track of function being assembled, updating st_size after each
 * instruction.
 */
static Elf64_Sym *current_function_entry;

/*
 * List of pending global symbols, not yet added to .symtab. All globals
 * have to come after LOCAL symbols, according to spec. Also, ld will
 * segfault(!) otherwise.
 */
struct global {
    struct symbol *sym;
    Elf64_Sym entry;
};

static array_of(struct global) globals;

/*
 * Text section contains offsets to labels, also in text. Forward
 * references cannot be resolved immediately, as translation is single
 * pass. Store offsets into .text, paired with symbol (label) which
 * offsets should be calculated.
 */
struct pending_displacement {
    const struct symbol *label;
    int text_offset;
};

static array_of(struct pending_displacement) pending_displacement_list;

/* Write bytes to section. If ptr is NULL, fill with zeros. */
static int elf_section_write(int shid, const void *data, size_t n)
{
    /*
     * Section data buffer capacity, in bytes. Buffer is kept in sbuf,
     * indexed by section id. 
     */
    static size_t scap[SHNUM];

    size_t offset;
    assert(0 < shid && shid < SHNUM);
    assert(
        shdr[shid].sh_type == SHT_STRTAB ||
        shdr[shid].sh_type == SHT_SYMTAB ||
        shdr[shid].sh_type == SHT_PROGBITS ||
        shdr[shid].sh_type == SHT_NOBITS);

    offset = shdr[shid].sh_size;
    if (shdr[shid].sh_type != SHT_NOBITS) {
        if (offset + n >= scap[shid]) {
            assert(n > 0);
            if (!scap[shid]) {
                assert(!offset);
                assert(!sbuf[shid].data);
                scap[shid] = 10 * n;
                sbuf[shid].data = malloc(scap[shid]);
            } else {
                assert(offset);
                assert(sbuf[shid].data);
                scap[shid] = 2 * scap[shid] + n;
                sbuf[shid].data = realloc(sbuf[shid].data, scap[shid]);
            }
        }
        if (data) {
            memcpy(sbuf[shid].data + offset, data, n);
        } else {
            memset(sbuf[shid].data + offset, 0, n);
        }
    }

    shdr[shid].sh_size += n;
    return offset;
}

/*
 * Align data section to specified number of bytes. Following calls to
 * elf_section_write start at this alignment. Padding is filled with
 * zero.
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

/*
 * Add entry to .symtab, returning index.
 *
 * All STB_LOCAL must come before STB_GLOBAL. Index of the first non-
 * local symbol is stored in section header field.
 */
static int elf_symtab_add(Elf64_Sym entry)
{
    int i;

    i = shdr[SHID_SYMTAB].sh_size / sizeof(Elf64_Sym);
    elf_section_write(SHID_SYMTAB, &entry, sizeof(Elf64_Sym));

    if (entry.st_info >> 4 == STB_GLOBAL && !shdr[SHID_SYMTAB].sh_info) {
        shdr[SHID_SYMTAB].sh_info = i;
    } else {
        assert(i == 0 ||
            (entry.st_info >> 4) == (sbuf[SHID_SYMTAB].sym[i-1].st_info >> 4));
    }

    return i;
}

/*
 * Associate symbol with symtab entry. Internal symbols are added to
 * table right away, but global symbols have to be buffered and flushed
 * at the end. (Mis-)use member for storing index into ELF symbol table.
 * Stack offset is otherwise only used for local variables, which will
 * not live in this symbol table.
 */
static void elf_symtab_assoc(struct symbol *sym, Elf64_Sym entry)
{
    struct global var;
    if (sym->linkage == LINK_INTERN) {
        sym->stack_offset = elf_symtab_add(entry);
        if (is_function(sym->type)) {
            current_function_entry = &sbuf[SHID_SYMTAB].sym[sym->stack_offset];
        }
    } else {
        assert((entry.st_info >> 4) == STB_GLOBAL);
        var.sym = sym;
        var.entry = entry;
        array_push_back(&globals, var);
        if (is_function(sym->type)) {
            /*
             * This seems a bit shady, assumes the array address is kept
             * stable. Same with LINK_INTERN case...
             */
            current_function_entry
                = &array_get(&globals, array_len(&globals) - 1).entry;
        }
    }
}

/* Write global symtab entries to section data. */
static void flush_symtab_globals(void)
{
    int i;
    struct global var;

    for (i = 0; i < array_len(&globals); ++i) {
        var = array_get(&globals, i);
        var.sym->stack_offset = elf_symtab_add(var.entry);
    }

    array_clear(&globals);
}

/*
 * Add string to strtab section, returning its offset into the section
 * for use in references.
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
    array_push_back(&pending_relocation_list, entry);
    if (entry.section == SHID_RELA_TEXT) {
        n_rela_text++;
    } else {
        assert(entry.section == SHID_RELA_DATA);
        n_rela_data++;
    }
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

/*
 * Construct relocation entries from pending relocations. Invoked with
 * flush(), after all data and code is processed. It is important that
 * this is called once all symbols have been written to symtab, as it
 * relies on stack_offset pointing to symtab entry index.
 */
static void flush_relocations(void)
{
    Elf64_Rela
        *entry,
        *data_entry = NULL,
        *text_entry = NULL;
    struct pending_relocation pending;
    int i;

    if (n_rela_text) {
        sbuf[SHID_RELA_TEXT].rela = calloc(n_rela_text, sizeof(Elf64_Rela));
        shdr[SHID_RELA_TEXT].sh_size = n_rela_text * sizeof(Elf64_Rela);
        text_entry = sbuf[SHID_RELA_TEXT].rela;
    }

    if (n_rela_data) {
        sbuf[SHID_RELA_DATA].rela = calloc(n_rela_data, sizeof(Elf64_Rela));
        shdr[SHID_RELA_DATA].sh_size = n_rela_data * sizeof(Elf64_Rela);
        data_entry = sbuf[SHID_RELA_DATA].rela;
    }

    assert(array_len(&pending_relocation_list) == n_rela_text + n_rela_data);
    for (i = 0; i < n_rela_text + n_rela_data; ++i) {
        pending = array_get(&pending_relocation_list, i);
        assert(pending.type != R_X86_64_NONE);
        if (pending.section == SHID_RELA_DATA) {
            assert(n_rela_data);
            entry = data_entry++;
        } else {
            assert(n_rela_text);
            assert(pending.section == SHID_RELA_TEXT);
            entry = text_entry++;
        }

        entry->r_offset = pending.offset;
        entry->r_addend = pending.addend;
        entry->r_info =
            ELF64_R_INFO(symtab_index_of(pending.symbol), pending.type);

        /*
         * Subtract 4 to account for the size occupied by the relocation
         * slot itself, it takes up 4 bytes in the instruction.
         */
        if (pending.type == R_X86_64_PC32) {
            entry->r_addend -= 4;
        }
    }

    array_clear(&pending_relocation_list);
}

/*
 * Must be called before writing text segment. Overwrite locations with
 * offsets now found in stack_offset member of label symbols.
 */
static void flush_text_displacements(void)
{
    int i, *ptr;
    struct pending_displacement entry;

    for (i = 0; i < array_len(&pending_displacement_list); ++i) {
        entry = array_get(&pending_displacement_list, i);
        assert(entry.label->stack_offset);

        ptr = (int *) (sbuf[SHID_TEXT].data + entry.text_offset);
        *ptr += entry.label->stack_offset - entry.text_offset;
    }

    array_clear(&pending_displacement_list);
}

int elf_text_displacement(const struct symbol *label, int instr_offset)
{
    struct pending_displacement entry;
    assert(label->symtype == SYM_LABEL);

    if (label->stack_offset) {
        return label->stack_offset - shdr[SHID_TEXT].sh_size - instr_offset;
    }

    entry.label = label;
    entry.text_offset = shdr[SHID_TEXT].sh_size + instr_offset;
    array_push_back(&pending_displacement_list, entry);
    return 0;
}

/*
 * Initialize object file output. Called once before any other function.
 *
 * Write initial values to .symtab, starting with {0}, followed by a
 * special symbol representing the name of the source file, and section
 * names.
 */
void elf_init(FILE *output, const char *file)
{
    static Elf64_Sym default_symbols[] = {
        {0, (STB_LOCAL << 4) | STT_SECTION, 0, SHID_DATA, 0, 0},
        {0, (STB_LOCAL << 4) | STT_SECTION, 0, SHID_TEXT, 0, 0}
    };

    Elf64_Sym entry = {0};
    object_file_output = output;
    elf_symtab_add(entry);
    if (file) {
        entry.st_name = elf_strtab_add(SHID_STRTAB, file);
        entry.st_info = STB_LOCAL << 4 | STT_FILE;
        entry.st_shndx = SHN_ABS;
        elf_symtab_add(entry);
    }

    elf_section_write(SHID_SYMTAB, &default_symbols, sizeof(default_symbols));
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

    if (is_function(sym->type)) {
        entry.st_info |= STT_FUNC;
        if (sym->symtype == SYM_DEFINITION) {
            entry.st_shndx = SHID_TEXT;
            entry.st_value = shdr[SHID_TEXT].sh_size;
        }
        /* st_size is updated while assembling instructions. */
    } else if (sym->symtype == SYM_DEFINITION) {
        elf_section_align(SHID_DATA, sym_alignment(sym));
        entry.st_shndx = SHID_DATA;
        entry.st_size = size_of(sym->type);
        entry.st_value = shdr[SHID_DATA].sh_size;
        entry.st_info |= STT_OBJECT;
    } else if (
        sym->symtype == SYM_STRING_VALUE ||
        sym->symtype == SYM_CONSTANT)
    {
        elf_section_align(SHID_RODATA, sym_alignment(sym));
        entry.st_shndx = SHID_RODATA;
        entry.st_size = size_of(sym->type);
        entry.st_value = shdr[SHID_RODATA].sh_size;
        entry.st_info |= STT_OBJECT;

        /*
         * Strings and constant symbols carry their actual string value;
         * write to .rodata immediately.
         */
        if (sym->symtype == SYM_STRING_VALUE) {
            elf_section_write(SHID_RODATA,
                str_raw(sym->string_value), entry.st_size);
        } else {
            elf_section_write(SHID_RODATA, &sym->constant_value, entry.st_size);
        }
    } else if (sym->linkage == LINK_INTERN) {
        elf_section_align(SHID_BSS, sym_alignment(sym));
        entry.st_shndx = SHID_BSS;
        entry.st_size = size_of(sym->type);
        entry.st_value = shdr[SHID_BSS].sh_size;
        entry.st_info |= STT_OBJECT;
        shdr[SHID_BSS].sh_size += entry.st_size;
    } else if (sym->symtype == SYM_TENTATIVE) {
        assert(sym->linkage == LINK_EXTERN);
        entry.st_shndx = SHN_COMMON;
        entry.st_size = size_of(sym->type);
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
        assert(w == imm.d.string.len + 1);
        ptr = str_raw(imm.d.string);
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
            free(sbuf[i].data);
            break;
        case SHT_SYMTAB:
            fwrite(sbuf[i].sym, shdr[i].sh_size, 1, object_file_output);
            free(sbuf[i].sym);
            break;
        case SHT_RELA:
            fwrite(sbuf[i].rela, shdr[i].sh_size, 1, object_file_output);
            free(sbuf[i].rela);
            break;
        default:
            assert(shdr[i].sh_type == SHT_NOBITS);
            break;
        }
    }

    return 0;
}
