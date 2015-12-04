#include "elf.h"

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
    4,                  /* e_shnum, number of section headers */
    3                   /* e_shstrndx, index of shstrtab */
};

static char shstrtab[] =
    "\0.data\0.text\0.shstrtab\0.symtab\0.strtab\0.rela.text\0";

static Elf64_Shdr
    shdr_zero = {0},    /* First section header must contain all-zeroes */
    shdr_data = {
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
    shdr_text = {
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
    },
    shdr_shstrtab = {
        13,             /* sh_name, index into shstrtab */
        SHT_STRTAB,     /* sh_type */
        0,              /* sh_flags */
        0x0,            /* sh_addr */
        sizeof(Elf64_Ehdr) + 4 * sizeof(Elf64_Shdr),
        sizeof(shstrtab),
        SHN_UNDEF,      /* sh_link */
        0,              /* sh_info */
        1,              /* sh_addralign */
        0               /* sh_entsize */
    };

int elf_symbol(const struct symbol *sym)
{
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

    fwrite(&shdr_zero, sizeof(shdr_zero), 1, object_file_output);
    fwrite(&shdr_data, sizeof(shdr_data), 1, object_file_output);
    fwrite(&shdr_text, sizeof(shdr_text), 1, object_file_output);
    fwrite(&shdr_shstrtab, sizeof(shdr_shstrtab), 1, object_file_output);

    fwrite(shstrtab, shdr_shstrtab.sh_size, 1, object_file_output);
    return 0;
}
