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
        1,              /* sh_info, index of first non-local symbol (TODO) */
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

/* Write bytes to .data section. If ptr is NULL, fill with zeros.
 */
static int elf_data_add(const char *ptr, size_t n)
{
    size_t offset = shdr[SHID_DATA].sh_size;

    data = realloc(data, offset + n);
    if (ptr)
        memcpy(data + offset, ptr, n);
    else
        memset(data + offset, '\0', n);
    shdr[SHID_DATA].sh_size += n;
    return offset;
}

/* Align data section to specified number of bytes. Following calls to
 * elf_data_add start at this alignment. Padding is filled with zero.
 */
static int elf_data_align(int align)
{
    size_t offset = shdr[SHID_DATA].sh_size;

    if (offset % align != 0)
        elf_data_add(NULL, align - (offset % align));
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
static int rela_text_len;

/* Pending relocations, waiting for sym->stack_offset to be resolved to index
 * into .symtab once written to data section.
 */
static struct pending_relocation {
    const struct symbol *sym;
    int offset;                 /* offset into .text */
    int addend;                 /* offset into symbol ? */
} *prl;

/* Add pending relocation to symbol on offset from current .text size. Offset
 * is into instruction being assembled.
 * The relocation entry cannot always be immediately constructed, as the symbol
 * might not have been written to any section yet.
 */
void elf_add_relocation(
    const struct symbol *sym,
    int section_offset,
    int sym_offset)
{
    struct pending_relocation *entry;

    rela_text_len += 1;
    prl = realloc(prl, rela_text_len * sizeof(*prl));
    entry = &prl[rela_text_len - 1];

    entry->sym = sym;
    entry->offset = section_offset + shdr[SHID_TEXT].sh_size;
    entry->addend = sym_offset - size_of(&sym->type);
}

/* Construct relocation entries from pending relocations. Invoked with flush(),
 * after all data and code is processed.
 */
static void flush_relocations(void)
{
    int i;
    Elf64_Rela *entry;
    assert(!rela_text);

    rela_text = calloc(rela_text_len, sizeof(*rela_text));
    for (i = 0; i < rela_text_len; ++i) {
        entry = &rela_text[i];

        /* Stack offset in this context is used for .symtab entry index. */
        assert(prl[i].sym->stack_offset);

        entry->r_offset = prl[i].offset;
        entry->r_info =
            ELF64_R_INFO((long) prl[i].sym->stack_offset, R_X86_64_PC32);
        entry->r_addend = prl[i].addend;
    }

    shdr[SHID_RELA_TEXT].sh_size = rela_text_len * sizeof(Elf64_Rela);
}

int elf_symbol(const struct symbol *sym)
{
    Elf64_Sym entry = {0};

    /* Ignore these for now... */
    if (sym->symtype == SYM_LABEL || sym->symtype == SYM_STRING_VALUE) {
        return 0;
    }

    entry.st_name = elf_strtab_add(sym->name);

    if (is_function(&sym->type)) {
        switch (sym->symtype) {
        case SYM_DEFINITION:
            entry.st_shndx = SHID_TEXT;
        case SYM_DECLARATION:
        case SYM_TENTATIVE:
            entry.st_info = (STB_GLOBAL << 4) | STT_FUNC;
            break;
        default:
            break;
        }
        elf_symtab_add(entry);
    } else if (sym->symtype == SYM_DEFINITION) {
        elf_data_align(sym_alignment(sym));
        entry.st_shndx = SHID_DATA;
        /* Linker goes into shock if we mix up local/global... */
        /*if (sym->linkage == LINK_EXTERN)*/
            entry.st_info = (STB_GLOBAL << 4) | STT_OBJECT;
        /*else
            entry.st_info = (STB_LOCAL << 4) | STT_OBJECT;*/
        entry.st_size = size_of(&sym->type);
        entry.st_value = shdr[SHID_DATA].sh_size;

        /* (Mis-)use member for storing index into ELF symbol table. Stack
         * offset is otherwise only used for local variables, which will not
         * live in this symbol table. */
        assert(!sym->stack_offset);
        ((struct symbol *) sym)->stack_offset = elf_symtab_add(entry);
    }

    return 0;
}

int elf_text(struct instruction instr)
{
    struct code c = encode(instr);

    if (c.val[0] == 0x90)
        return 0;

    text = realloc(text, shdr[SHID_TEXT].sh_size + c.len);
    memcpy(text + shdr[SHID_TEXT].sh_size, &c.val, c.len);

    shdr[SHID_TEXT].sh_size += c.len;

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

    return elf_data_add(ptr, w);
}

int elf_flush(void)
{
    flush_relocations();
    elf_data_align(0x10);
    fwrite(&header, sizeof(header), 1, object_file_output);

    /* Fill in missing offset and size values */
    SHDR_CHAIN_OFFSET(SHID_STRTAB, SHID_SYMTAB);
    SHDR_CHAIN_OFFSET(SHID_SYMTAB, SHID_RELA_TEXT);
    SHDR_CHAIN_OFFSET(SHID_RELA_TEXT, SHID_DATA);
    SHDR_CHAIN_OFFSET(SHID_DATA, SHID_TEXT);

    /* Section headers */
    fwrite(&shdr, sizeof(shdr), 1, object_file_output);

    /* Section data */
    fwrite(shstrtab, shdr[SHID_SHSTRTAB].sh_size, 1, object_file_output);
    fwrite(strtab, shdr[SHID_STRTAB].sh_size, 1, object_file_output);
    fwrite(symtab, shdr[SHID_SYMTAB].sh_size, 1, object_file_output);
    fwrite(rela_text, shdr[SHID_RELA_TEXT].sh_size, 1, object_file_output);
    fwrite(data, shdr[SHID_DATA].sh_size, 1, object_file_output);
    fwrite(text, shdr[SHID_TEXT].sh_size, 1, object_file_output);

    return 0;
}
