#ifndef ELF_H
#define ELF_H

#include "encoding.h"
#include <lacc/symbol.h>

#include <stdio.h>

typedef unsigned long Elf64_Addr, Elf64_Off, Elf64_Xword;
typedef unsigned int Elf64_Word;
typedef unsigned short Elf64_Half;
typedef long Elf64_Sxword;
typedef int Elf64_Sword;

typedef struct {
    unsigned char   e_ident[16];    /* ELF identification. */
    Elf64_Half      e_type;         /* Object file type. */
    Elf64_Half      e_machine;      /* Machine type. */
    Elf64_Word      e_version;      /* Object file version. */
    Elf64_Addr      e_entry;        /* Entry point address. */
    Elf64_Off       e_phoff;        /* Program header offset. */
    Elf64_Off       e_shoff;        /* Section header offset. */
    Elf64_Word      e_flags;        /* Processor-specific flags. */
    Elf64_Half      e_ehsize;       /* ELF header size. */
    Elf64_Half      e_phentsize;    /* Size of program header entry. */
    Elf64_Half      e_phnum;        /* Program header entry count. */
    Elf64_Half      e_shentsize;    /* Size of section header entry. */
    Elf64_Half      e_shnum;        /* Section header entry count. */
    Elf64_Half      e_shstrndx;     /* Section name strtab index. */
} Elf64_Ehdr;

/*
 * Constants used in e_ident part of header. Only including values
 * relevant for target platform; 64 bit Linux System V.
 */
#define ELFCLASS64 2
#define ELFDATA2LSB 1               /* Little-endian data structures. */
#define EV_CURRENT 1                /* Current ELF version. */
#define ELFOSABI_SYSV 0             /* System V ABI. */
#define ET_REL 1                    /* Relocatable file. */

typedef struct {
    Elf64_Word      sh_name;        /* Section name. */
    Elf64_Word      sh_type;        /* Section type. */
    Elf64_Xword     sh_flags;       /* Section attributes. */
    Elf64_Addr      sh_addr;        /* Virtual address in memory. */
    Elf64_Off       sh_offset;      /* Offset in file. */
    Elf64_Xword     sh_size;        /* Size of section. */
    Elf64_Word      sh_link;        /* Link to other section. */
    Elf64_Word      sh_info;        /* Miscellaneous information. */
    Elf64_Xword     sh_addralign;   /* Address alignment boundary. */
    Elf64_Xword     sh_entsize;     /* Size of entries. */
} Elf64_Shdr;

#define SHN_UNDEF 0
#define SHN_ABS 0xFFF1              /* Absolute value reference. */
#define SHN_COMMON 0xFFF2           /* Tentative definitions. */

/* Section types, sh_type. */
#define SHT_NULL 0
#define SHT_PROGBITS 1
#define SHT_SYMTAB 2
#define SHT_STRTAB 3
#define SHT_RELA 4
#define SHT_HASH 5
#define SHT_DYNAMIC 6
#define SHT_NOTE 7
#define SHT_NOBITS 8                /* Uninitialized space. */
#define SHT_DYNSYM 11

/* Section attributes, sh_flags. */
#define SHF_WRITE 0x1
#define SHF_ALLOC 0x2
#define SHF_EXECINSTR 0x4

typedef struct {
    Elf64_Word      st_name;        /* Symbol name. */
    unsigned char   st_info;        /* Type and Binding attributes. */
    unsigned char   st_other;       /* Reserved. */
    Elf64_Half      st_shndx;       /* Section table index. */
    Elf64_Addr      st_value;       /* Symbol value. */
    Elf64_Xword     st_size;        /* Size of object. */
} Elf64_Sym;

#define STB_LOCAL 0
#define STB_GLOBAL 1

#define STT_NOTYPE 0
#define STT_OBJECT 1
#define STT_FUNC 2
#define STT_SECTION 3
#define STT_FILE 4

typedef struct {
    Elf64_Addr      r_offset;       /* Address of reference. */
    Elf64_Xword     r_info;         /* Symbol index and reloc type. */
    Elf64_Sxword    r_addend;       /* Constant part of expression. */
} Elf64_Rela;

/*
 * A: Represents the addend used to compute the value of the relocatable
 *    field.
 * P: Represents the place (section offset or address) of the storage
 *    unit being relocated (computed using r_offset).
 * S: Represents the value of the symbol whose index resides in the
 *    relocation entry.
 * L: Procedure linkage table position (PLT).
 *
 * GOT: Address of global offset table.
 * G:   Offset into GOT where symbol is placed.
 *
 */
enum rel_type {
    R_X86_64_NONE = 0,
    R_X86_64_64 = 1,                /* word64   S + A. */
    R_X86_64_PC32 = 2,              /* word32   S + A - P */
    R_X86_64_PLT32 = 4,             /* word32   L + A - P */
    R_X86_64_GOTPCREL = 9           /* word32   G + GOT + A - P */
};

#define ELF64_R_INFO(s, t) ((((long) s) << 32) + (((long) t) & 0xFFFFFFFFL))

INTERNAL void elf_init(FILE *output, const char *file);

INTERNAL int elf_symbol(const struct symbol *sym);

INTERNAL int elf_text(struct instruction instr);

INTERNAL int elf_data(struct immediate data);

/* Write pending label offsets. Required after each function. */
INTERNAL void elf_flush_text_displacements(void);

INTERNAL int elf_flush(void);

/* Free memory after all objects have been compiled. */
INTERNAL int elf_finalize(void);

/*
 * Insert relocation entry to symbol at the current position of .text.
 */
INTERNAL void elf_add_reloc_text(
    const struct symbol *symbol,
    enum rel_type type,
    int offset,
    int addend);

/*
 * Return offset between label and current position in text segment, if
 * label has already been calculated. For forward references, return 0
 * and store this location as pending. All pending displacements are
 * written on flush.
 */
INTERNAL int elf_text_displacement(const struct symbol *label, int offset);

/* Initialize a new section in the object file. */
INTERNAL int elf_section_init(const char *name,
    int type,
    int flags,
    int link,
    int info,
    int addralign,
    int entsize);

/* Write data to section, using id given by section_init. */
INTERNAL size_t elf_section_write(int shid, const void *data, size_t n);

/* Get raw pointer to section buffer. */
INTERNAL void *elf_section_buffer(int shid);

#endif
