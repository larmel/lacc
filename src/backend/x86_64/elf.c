#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "abi.h"
#include "elf.h"
#include "dwarf.h"
#include <lacc/array.h>
#include <lacc/context.h>

#include <assert.h>

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
    0,                  /* e_shnum, number of section headers. (TODO) */
    0                   /* e_shstrndx, index of shstrtab. (TODO) */
};

#define SHNUM_MAX 13

/* Section headers. */
static Elf64_Shdr shdr[SHNUM_MAX];
static int shnum;

INTERNAL struct elf_sections section = {0};

static struct symbol section_symbol[SHNUM_MAX];

#define symtab_index_of(s) ((s)->stack_offset)
#define symtab_lookup(s) (&sbuf[section.symtab].sym[(s)->stack_offset])

/* Data associated with each section. */
static union {
    unsigned char *data;
    Elf64_Sym *sym;
    Elf64_Rela *rela;
} sbuf[SHNUM_MAX];

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

/* Store list of relocations for sections of type rela. */
static array_of(struct pending_relocation)
    pending_relocations[SHNUM_MAX];

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
INTERNAL size_t elf_section_write(int shid, const void *data, size_t n)
{
    /*
     * Section data buffer capacity, in bytes. Buffer is kept in sbuf,
     * indexed by section id. 
     */
    static size_t scap[SHNUM_MAX];

    size_t offset;
    assert(0 < shid && shid < shnum);
    assert(
        shdr[shid].sh_type == SHT_STRTAB ||
        shdr[shid].sh_type == SHT_SYMTAB ||
        shdr[shid].sh_type == SHT_PROGBITS ||
        shdr[shid].sh_type == SHT_RELA ||
        shdr[shid].sh_type == SHT_NOBITS);

    offset = shdr[shid].sh_size;
    if (shdr[shid].sh_type != SHT_NOBITS && n > 0) {
        if (offset + n >= scap[shid]) {
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

INTERNAL void *elf_section_buffer(int shid)
{
    return sbuf[shid].data;
}

/*
 * Align data section to specified number of bytes. Following calls to
 * elf_section_write start at this alignment. Padding is filled with
 * zero.
 */
static int elf_section_align(int shid, int align)
{
    size_t offset;
    assert(0 < shid && shid < shnum);
    assert(shdr[shid].sh_type == SHT_STRTAB
        || shdr[shid].sh_type == SHT_SYMTAB
        || shdr[shid].sh_type == SHT_PROGBITS
        || shdr[shid].sh_type == SHT_RELA
        || shdr[shid].sh_type == SHT_NOBITS);

    if (shdr[shid].sh_addralign < align) {
        shdr[shid].sh_addralign = align;
        assert(align <= 16);
    }

    offset = shdr[shid].sh_size;
    if (offset % align != 0) {
        elf_section_write(shid, NULL, align - (offset % align));
    }

    return offset;
}

/*
 * Update offset values into the object file for each section.
 * Insert padding between sections to make offsets respect alignment.
 */
static void elf_chain_offsets(void)
{
    int i, j;
    int padding;

    for (j = 1; j < shnum && shdr[j].sh_type == SHT_NOBITS; ++j)
        ;

    assert(j < shnum);
    assert(shdr[j].sh_type != SHT_NOBITS);
    shdr[j].sh_offset = sizeof(Elf64_Ehdr) + shnum * sizeof(Elf64_Shdr);

    for (i = j + 1; i < shnum; ++i) {
        if (shdr[i].sh_type == SHT_NOBITS)
            continue;

        assert(j > 0);
        assert(shdr[j].sh_type != SHT_NOBITS);
        shdr[i].sh_offset = shdr[j].sh_offset + shdr[j].sh_size;
        if (shdr[i].sh_addralign > 1) {
            padding = shdr[i].sh_offset % shdr[i].sh_addralign;
            shdr[i].sh_offset += padding;
            assert(padding < 16);
        }

        j = i;
    }
}

/*
 * Add string to strtab section, returning its offset into the section
 * for use in references.
 */
static int elf_strtab_add(int shid, const char *str)
{
    int pos;

    assert(0 < shid && shid < shnum);
    assert(shdr[shid].sh_type == SHT_STRTAB);

    if (!shdr[shid].sh_size)
        elf_section_write(shid, NULL, 1);

    pos = shdr[shid].sh_size;
    elf_section_write(shid, str, strlen(str) + 1);

    return pos;
}

/*
 * Add entry to .symtab, returning index.
 *
 * All STB_LOCAL must come before STB_GLOBAL. Index of the first non-
 * local symbol is stored in section header field.
 *
 * The first item in symtab should be all-zero, so handle that here.
 */
static int elf_symtab_add(Elf64_Sym entry)
{
    int i;
    assert(section.symtab);

    i = shdr[section.symtab].sh_size / sizeof(Elf64_Sym);
    if (!i) {
        elf_section_write(section.symtab, NULL, sizeof(Elf64_Sym));
        i += 1;
    }

    elf_section_write(section.symtab, &entry, sizeof(Elf64_Sym));
    if (entry.st_info >> 4 == STB_GLOBAL && !shdr[section.symtab].sh_info) {
        shdr[section.symtab].sh_info = i;
    } else {
        assert(i == 0 ||
            (entry.st_info >> 4) == (sbuf[section.symtab].sym[i-1].st_info >> 4));
    }

    return i;
}

/*
 * Create ELF section, returning id of new section.
 *
 * Ensures that first section is all zero.
 */
INTERNAL int elf_section_init(
    const char *name,
    int type,
    int flags,
    int link,
    int info,
    int addralign,
    int entsize)
{
    Elf64_Sym sym = {0};
    int shid;

    if (!shnum) {
        shnum++;
        memset(shdr, 0, sizeof(Elf64_Shdr));
    }

    shid = shnum++;
    assert(shid < SHNUM_MAX);

    memset(shdr + shid, 0, sizeof(Elf64_Shdr));
    shdr[shid].sh_type = type;
    shdr[shid].sh_flags = flags;
    shdr[shid].sh_link = link;
    shdr[shid].sh_info = info;
    shdr[shid].sh_addralign = addralign;
    shdr[shid].sh_entsize = entsize;
    if (section.shstrtab) {
        shdr[shid].sh_name = elf_strtab_add(section.shstrtab, name);
    } else {
        assert(!strcmp(".shstrtab", name));
        shdr[shid].sh_name = elf_strtab_add(shid, name);
        header.e_shstrndx = shid;
    }

    if (section.symtab) {
        sym.st_info = (STB_LOCAL << 4) | STT_SECTION;
        sym.st_shndx = shid;
        section_symbol[shid].stack_offset = elf_symtab_add(sym);
    }

    header.e_shnum = shnum;
    return shid;
}

/*
 * Retrieve index into symbol table for section as a proper symbol,
 * making it convenient to use like any other when creating relocations.
 */
INTERNAL const struct symbol *elf_section_symbol(int shnum)
{
    assert(shnum > 0);
    assert(shnum <= SHNUM_MAX);

    return &section_symbol[shnum];
}

/*
 * Keep track of current function being emitted, for adjusting size of
 * symbol entry.
 *
 * Since we have separate lists for local and global symbols, keep an
 * extra flag telling which array to look up.
 */
static struct {
    enum {
        CURRENT_FUNC_NONE,
        CURRENT_FUNC_STATIC,
        CURRENT_FUNC_GLOBAL
    } type;
    int index;
} current_function;

static void increment_function_size(size_t bytes)
{
    Elf64_Sym *entry;
    assert(current_function.type != CURRENT_FUNC_NONE);

    if (current_function.type == CURRENT_FUNC_STATIC) {
        entry = &sbuf[section.symtab].sym[current_function.index];
    } else {
        assert(current_function.type == CURRENT_FUNC_GLOBAL);
        entry = &array_get(&globals, current_function.index).entry;
    }

    entry->st_size += bytes;
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
            current_function.type = CURRENT_FUNC_STATIC;
            current_function.index = sym->stack_offset;
        }
    } else {
        assert((entry.st_info >> 4) == STB_GLOBAL);
        var.sym = sym;
        var.entry = entry;
        array_push_back(&globals, var);
        if (is_function(sym->type)) {
            current_function.type = CURRENT_FUNC_GLOBAL;
            current_function.index = array_len(&globals) - 1;
        }
    }
}

/*
 * Write global symtab entries to section data.
 *
 * A special entry for GOT is added in the end, if we are generating
 * position independent code.
 */
static void flush_symtab_globals(void)
{
    int i;
    struct global var;
    Elf64_Sym entry = {0};

    for (i = 0; i < array_len(&globals); ++i) {
        var = array_get(&globals, i);
        var.sym->stack_offset = elf_symtab_add(var.entry);
    }

    if (context.pic) {
        entry.st_name = elf_strtab_add(section.strtab, "_GLOBAL_OFFSET_TABLE_");
        entry.st_info = STB_GLOBAL << 4 | STT_NOTYPE;
        entry.st_shndx = SHN_UNDEF;
        elf_symtab_add(entry);
    }

    array_empty(&globals);
}

INTERNAL void elf_add_relocation(
    int shid,
    const struct symbol *symbol,
    enum rel_type type,
    int offset,
    int addend)
{
    struct pending_relocation entry = {0};
    int target;

    assert(shid);
    assert(shdr[shid].sh_type == SHT_RELA);

    target = shdr[shid].sh_info;
    entry.symbol = symbol;
    entry.type = type;
    entry.offset = shdr[target].sh_size + offset;
    entry.addend = addend;
    array_push_back(&pending_relocations[shid], entry);
}

/*
 * Construct relocation entries from pending relocations. Invoked with
 * flush(), after all data and code is processed. It is important that
 * this is called once all symbols have been written to symtab, as it
 * relies on stack_offset pointing to symtab entry index.
 */
static void flush_relocations(void)
{
    Elf64_Rela *entry;
    struct pending_relocation pending;
    size_t offset;
    int i, j, len, index;

    for (i = 0; i < SHNUM_MAX; ++i) {
        len = array_len(&pending_relocations[i]);
        if (!len) {
            assert(!pending_relocations[i].data);
            continue;
        }

        offset = elf_section_write(i, NULL, len * sizeof(Elf64_Rela));
        entry = sbuf[i].rela + offset;
        for (j = 0; j < len; ++j) {
            pending = array_get(&pending_relocations[i], j);
            assert(pending.type != R_X86_64_NONE);

            index = symtab_index_of(pending.symbol);
            entry[j].r_offset = pending.offset;
            entry[j].r_addend = pending.addend;
            entry[j].r_info = ELF64_R_INFO(index, pending.type);

            /* Account for relocation itself. */
            switch (pending.type) {
            case R_X86_64_PC32:
            case R_X86_64_PLT32:
            case R_X86_64_GOTPCREL:
                entry[j].r_addend -= 4;
                break;
            default:
                break;
            }
        }

        array_clear(&pending_relocations[i]);
    }
}

/*
 * Overwrite locations with offsets now found in stack_offset member of
 * label symbols. Invoked after each function, before the labels are
 * recycled.
 */
INTERNAL void elf_flush_text_displacements(void)
{
    int i, *ptr;
    struct pending_displacement entry;

    for (i = 0; i < array_len(&pending_displacement_list); ++i) {
        entry = array_get(&pending_displacement_list, i);
        assert(entry.label->stack_offset);

        ptr = (int *) (sbuf[section.text].data + entry.text_offset);
        *ptr += entry.label->stack_offset - entry.text_offset;
    }

    array_empty(&pending_displacement_list);
}

INTERNAL int elf_text_displacement(const struct symbol *label, int instr_offset)
{
    struct pending_displacement entry;
    assert(label->symtype == SYM_LABEL);

    if (label->stack_offset) {
        return label->stack_offset - shdr[section.text].sh_size - instr_offset;
    }

    entry.label = label;
    entry.text_offset = shdr[section.text].sh_size + instr_offset;
    array_push_back(&pending_displacement_list, entry);
    return 0;
}

/*
 * Initialize object file output. Called once for every input file.
 *
 * Write initial values to .symtab, starting with {0}, followed by a
 * special symbol representing the name of the source file, and section
 * names.
 */
INTERNAL void elf_init(FILE *output, const char *file)
{
    Elf64_Sym entry = {0};

    shnum = 0;
    memset(&section, 0, sizeof(section));
    memset(&section_symbol, 0, sizeof(section_symbol));
    memset(&current_function, 0, sizeof(current_function));
    object_file_output = output;

    section.shstrtab = elf_section_init(
        ".shstrtab", SHT_STRTAB, 0, SHN_UNDEF, 0, 1, 0);

    section.strtab = elf_section_init(
        ".strtab", SHT_STRTAB, 0, SHN_UNDEF, 0, 1, 0);

    section.symtab = elf_section_init(
        ".symtab", SHT_SYMTAB, 0, section.strtab, 0, 4, sizeof(Elf64_Sym));

    if (file) {
        entry.st_name = elf_strtab_add(section.strtab, file);
        entry.st_info = STB_LOCAL << 4 | STT_FILE;
        entry.st_shndx = SHN_ABS;
        elf_symtab_add(entry);
    }

    section.bss = elf_section_init(
        ".bss", SHT_NOBITS, SHF_WRITE | SHF_ALLOC, SHN_UNDEF, 0, 4, 0);

    section.rodata = elf_section_init(
        ".rodata", SHT_PROGBITS, SHF_ALLOC, SHN_UNDEF, 0, 16, 0);

    section.data = elf_section_init(
        ".data", SHT_PROGBITS, SHF_WRITE | SHF_ALLOC, SHN_UNDEF, 0, 4, 0);

    section.rela_data = elf_section_init(
        ".rela.data", SHT_RELA, 0, section.symtab, section.data, 8,
        sizeof(Elf64_Rela));

    section.text = elf_section_init(
        ".text", SHT_PROGBITS, SHF_EXECINSTR | SHF_ALLOC, SHN_UNDEF, 0, 16, 0);

    section.rela_text = elf_section_init(
        ".rela.text", SHT_RELA, 0, section.symtab, section.text, 8,
        sizeof(Elf64_Rela));

    if (context.debug) {
        dwarf_init(file);
    }
}

INTERNAL int elf_symbol(const struct symbol *sym)
{
    const void *data;
    Elf64_Sym entry = {0};
    union {
        char arr[16];
        long double ld;
    } ldc = {0};

    assert(sym->linkage != LINK_NONE);
    assert(!sym->stack_offset);

    if (sym->symtype == SYM_LABEL) {
        ((struct symbol *) sym)->stack_offset = shdr[section.text].sh_size;
        return 0;
    }

    entry.st_name = elf_strtab_add(section.strtab, sym_name(sym));
    entry.st_info = (sym->linkage == LINK_INTERN)
        ? STB_LOCAL << 4 : STB_GLOBAL << 4;

    if (is_function(sym->type)) {
        entry.st_info |= STT_FUNC;
        if (sym->symtype == SYM_DEFINITION) {
            entry.st_shndx = section.text;
            entry.st_value = shdr[section.text].sh_size;
        }
        /* st_size is updated while assembling instructions. */
    } else if (sym->symtype == SYM_DEFINITION) {
        elf_section_align(section.data, sym_alignment(sym));
        entry.st_shndx = section.data;
        entry.st_size = size_of(sym->type);
        entry.st_value = shdr[section.data].sh_size;
        entry.st_info |= STT_OBJECT;
    } else if (sym->symtype == SYM_LITERAL || sym->symtype == SYM_CONSTANT) {
        elf_section_align(section.rodata, sym_alignment(sym));
        entry.st_shndx = section.rodata;
        entry.st_size = size_of(sym->type);
        entry.st_value = shdr[section.rodata].sh_size;
        entry.st_info |= STT_OBJECT;

        /*
         * Strings and constant symbols carry their actual string value;
         * write to .rodata immediately.
         */
        if (sym->symtype == SYM_LITERAL) {
            data = str_raw(sym->value.string);
        } else if (is_long_double(sym->type)) {
            ldc.ld = get_long_double(sym->value.constant);
            data = &ldc.arr;
            assert(sizeof(ldc.arr) == entry.st_size);
        } else {
            data = &sym->value.constant;
        }

        elf_section_write(section.rodata, data, entry.st_size);
    } else if (sym->linkage == LINK_INTERN
        || (sym->symtype == SYM_TENTATIVE && context.no_common))
    {
        elf_section_align(section.bss, sym_alignment(sym));
        entry.st_shndx = section.bss;
        entry.st_size = size_of(sym->type);
        entry.st_value = shdr[section.bss].sh_size;
        entry.st_info |= STT_OBJECT;
        shdr[section.bss].sh_size += entry.st_size;
    } else if (sym->symtype == SYM_TENTATIVE) {
        assert(sym->linkage == LINK_EXTERN);
        assert(is_object(sym->type));
        entry.st_shndx = SHN_COMMON;
        entry.st_size = size_of(sym->type);
        entry.st_value = sym_alignment(sym);
        entry.st_info |= STT_OBJECT;
    }

    elf_symtab_assoc((struct symbol *) sym, entry);
    return 0;
}

INTERNAL int elf_text(struct instruction instr)
{
    struct code c = encode(instr);

    if (c.val[0] != 0x90) {
        elf_section_write(section.text, &c.val, c.len);
        increment_function_size(c.len);
    }

    return 0;
}

INTERNAL int elf_data(struct immediate imm)
{
    const void *ptr = NULL;
    size_t w = imm.width;

    switch (imm.type) {
    case IMM_INT:
        if (imm.width == 1)
            ptr = &imm.d.byte;
        else if (imm.width == 2)
            ptr = &imm.d.word;
        else if (imm.width == 4)
            ptr = &imm.d.dword;
        else
            ptr = &imm.d.qword;
        break;
    case IMM_ADDR:
        assert(imm.d.addr.sym);
        assert(imm.width == 8);
        elf_add_relocation(section.rela_data,
            imm.d.addr.sym, R_X86_64_64, 0, imm.d.addr.displacement);
        break;
    case IMM_STRING:
        assert(w == str_len(imm.d.string) + 1 || w == str_len(imm.d.string));
        ptr = str_raw(imm.d.string);
        break;
    }

    return elf_section_write(section.data, ptr, w);
}

static void write_data(const void *ptr, size_t size)
{
    char padding[16] = {0};
    size_t b;

    if (!ptr) {
        assert(size <= sizeof(padding));
        ptr = padding;
    }

    b = fwrite(ptr, 1, size, object_file_output);
    if (b != size) {
        fprintf(stderr, "Write failed with %lu out of %lu bytes.\n", b, size);
        exit(1);
    }
}

static void write_sections(void)
{
    int i, j;
    size_t written;

    for (i = 1, j = -1; i < shnum; ++i) {
        if (shdr[i].sh_type == SHT_NOBITS)
            continue;

        if (j != -1) {
            written = shdr[j].sh_offset + shdr[j].sh_size;
            assert(written <= shdr[i].sh_offset);
            if (written < shdr[i].sh_offset) {
                write_data(NULL, shdr[i].sh_offset - written);
            }
        }

        j = i;
        if (shdr[i].sh_size) {
            assert(sbuf[i].data);
            write_data(sbuf[i].data, shdr[i].sh_size);
        }
    }
}

INTERNAL int elf_flush(void)
{
    /* Finalize debug sections. */
    if (context.debug) {
        dwarf_flush();
    }

    /* Write remaining data to section buffers. */
    flush_symtab_globals();
    flush_relocations();
    array_empty(&pending_displacement_list);

    /* Fill in missing offsets in section headers. */
    elf_chain_offsets();

    /* Write headers and section data to file. */
    assert(object_file_output);
    write_data(&header, sizeof(header));
    write_data(shdr, shnum * sizeof(*shdr));
    write_sections();
    return 0;
}

INTERNAL int elf_finalize(void)
{
    int i;

    array_clear(&globals);
    array_clear(&pending_displacement_list);
    for (i = 1; i < SHNUM_MAX; ++i) {
        free(sbuf[i].data);
    }

    return 0;
}
