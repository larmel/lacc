#include "elf.h"

#include <assert.h>

#define NSHDR 6     /* Number of section headers */

#define SHDR_ZERO 0
#define SHDR_SHSTRTAB 1
#define SHDR_STRTAB 2
#define SHDR_SYMTAB 3
#define SHDR_TEXT 4
#define SHDR_DATA 5

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
    NSHDR,              /* e_shnum, number of section headers */
    SHDR_SHSTRTAB       /* e_shstrndx, index of shstrtab */
};

static char shstrtab[] =
    "\0.data\0.text\0.shstrtab\0.symtab\0.strtab\0.rela.text\0"
    "\0\0\0\0\0\0\0\0\0\0\0\0\0"; /* Make size % 16 = 0 */

static Elf64_Shdr shdr[] = {
    {0},                /* First section header must contain all-zeroes */
    {
        13,             /* sh_name, index into shstrtab */
        SHT_STRTAB,     /* sh_type */
        0,              /* sh_flags */
        0x0,            /* sh_addr */
        sizeof(Elf64_Ehdr) + NSHDR * sizeof(Elf64_Shdr),
        sizeof(shstrtab),
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        1,              /* sh_addralign */
        0               /* sh_entsize */
    },
    {
        31,             /* sh_name, index into shstrtab */
        SHT_STRTAB,     /* sh_type */
        0,              /* sh_flags */
        0x0,            /* sh_addr */
        sizeof(Elf64_Ehdr) + NSHDR * sizeof(Elf64_Shdr) + sizeof(shstrtab),
        0,              /* sh_size (TODO) */
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        1,              /* sh_addralign */
        0               /* sh_entsize */
    },
    {
        23,             /* sh_name, index into shstrtab */
        SHT_SYMTAB,     /* sh_type */
        0,              /* sh_flags */
        0x0,            /* sh_addr */
        0,              /* sh_offset (TODO) */
        0,              /* sh_size (TODO) */
        SHDR_STRTAB,    /* sh_link, section number of strtab */
        1,              /* sh_info, index of first non-local symbol (TODO) */
        4,              /* sh_addralign */
        sizeof(Elf64_Sym)
    },
    {
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
    {
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

static Elf64_Sym syms[] = {
    {0},
    {0, (STB_LOCAL << 4) | STT_SECTION, 0, SHDR_DATA, 0, 0},
    {0, (STB_LOCAL << 4) | STT_SECTION, 0, SHDR_TEXT, 0, 0}
};

static struct string {
    const char *str;
    int len;
    int offset;
} *strtab;
static int strtab_len;

static Elf64_Sym *symtab;
static int symtab_len;

/* Add string to .strtab, returning its offset into the section for use in
 * references.
 */
static int strtab_add(const char *str)
{
    struct string *entry;
    strtab_len += 1;
    strtab = realloc(strtab, strtab_len * sizeof(*strtab));

    entry = &strtab[strtab_len - 1];
    entry->str = str;
    entry->len = strlen(str);
    entry->offset = 1;
    if (strtab_len > 1)
        entry->offset = (entry - 1)->offset + (entry - 1)->len + 1;

    return entry->offset;
}

static size_t strtab_size(void)
{
    size_t size = 1; /* null entry */
    struct string *entry;

    if (strtab_len) {
        entry = &strtab[strtab_len - 1];
        size = entry->offset + entry->len + 1;
    }

    return size + (size % 16);
}

static int fwrite_strtab(const struct string *st, size_t n, FILE *stream)
{
    size_t i, w = 0;

    if (n) {
        fputc('\0', stream);
        w += 1;
        for (i = 0; i < n; ++i) {
            fputs(st[i].str, stream);
            fputc('\0', stream);
            w += st[i].len + 1;
        }

        while (w < strtab_size()) {
            fputc('\0', stream);
            w += 1;
        }
    }

    return w;
}

static size_t symtab_size(void)
{
    return (symtab_len + sizeof(syms) / sizeof(syms[0])) * sizeof(Elf64_Sym);
}

static int fwrite_symtab(FILE *stream)
{
    size_t i;

    fwrite(syms, sizeof(Elf64_Sym), sizeof(syms) / sizeof(syms[0]), stream);
    for (i = 0; i < symtab_len; ++i)
        fwrite(symtab + i, sizeof(Elf64_Sym), 1, stream);

    return 1;
}

int elf_symbol(const struct symbol *sym)
{
    Elf64_Sym *entry;

    symtab_len += 1;
    symtab = realloc(symtab, symtab_len * sizeof(*symtab));

    entry = &symtab[symtab_len - 1];
    entry = memset(entry, 0, sizeof(Elf64_Sym));

    entry->st_name = strtab_add(sym->name);
    if (is_function(&sym->type)) {
        switch (sym->symtype) {
        case SYM_DEFINITION:
            entry->st_shndx = SHDR_TEXT;
        case SYM_DECLARATION:
        case SYM_TENTATIVE:
            entry->st_info = (STB_GLOBAL << 4) | STT_FUNC;
            break;
        default:
            break;
        }
    }

    return 0;
}

int elf_text(struct instruction instr)
{
    return 0;
}

int elf_data(struct immediate data)
{
    return 0;
}

int elf_flush(void)
{
    fwrite(&header, sizeof(header), 1, object_file_output);

    shdr[SHDR_STRTAB].sh_size = strtab_size();
    shdr[SHDR_SYMTAB].sh_offset =
        shdr[SHDR_STRTAB].sh_offset + shdr[SHDR_STRTAB].sh_size;
    shdr[SHDR_SYMTAB].sh_size = symtab_size();

    fwrite(&shdr[SHDR_ZERO], sizeof(Elf64_Shdr), 1, object_file_output);
    fwrite(&shdr[SHDR_SHSTRTAB], sizeof(Elf64_Shdr), 1, object_file_output);
    fwrite(&shdr[SHDR_STRTAB], sizeof(Elf64_Shdr), 1, object_file_output);
    fwrite(&shdr[SHDR_SYMTAB], sizeof(Elf64_Shdr), 1, object_file_output);
    fwrite(&shdr[SHDR_DATA], sizeof(Elf64_Shdr), 1, object_file_output);
    fwrite(&shdr[SHDR_TEXT], sizeof(Elf64_Shdr), 1, object_file_output);

    fwrite(shstrtab, shdr[SHDR_SHSTRTAB].sh_size, 1, object_file_output);
    fwrite_strtab(strtab, strtab_len, object_file_output);
    fwrite_symtab(object_file_output);
    return 0;
}
