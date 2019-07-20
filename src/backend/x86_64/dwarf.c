#if !AMALGAMATION
# define INTERNAL
# define EXTERNAL extern
#endif
#include "dwarf.h"
#include "elf.h"
#include <lacc/array.h>
#include <lacc/context.h>

#include <assert.h>
#include <stdarg.h>

/* Unit header. Only generate compile units. */
#define DW_UT_compile 0x01

/* Tag encodings. */
#define DW_TAG_compile_unit 0x11

/* Child property of entry. */
#define DW_CHILDREN_no 0x00
#define DW_CHILDREN_yes 0x01

/* Attribute encoding. */
#define DW_AT_name 0x03
#define DW_AT_language 0x13
#define DW_AT_producer 0x25
#define DW_AT_comp_dir 0x1b
#define DW_AT_stmt_list 0x10 /* lineptr */
#define DW_AT_low_pc 0x11
#define DW_AT_high_pc 0x12

/* Attribute form encoding. */
#define DW_FORM_addr 0x01
#define DW_FORM_string 0x08
#define DW_FORM_data1 0x0b
#define DW_FORM_data2 0x05
#define DW_FORM_data4 0x07
#define DW_FORM_data8 0x07

/* Language names. */
#define DW_LANG_C89 0x0001
#define DW_LANG_C99 0x000c
#define DW_LANG_C11 0x001d

/* Attribute of Debug information entry. */
struct dwarf_attr {
    int name; /* DW_AT_* */
    int form; /* DW_FORM_* */
    union {
        const char *str;
        const struct symbol *sym;
        long num;
        unsigned long unum;
    } value;
};

/* Debugging information entry (DIE). */
struct dwarf_die {
    int code;
    int tag; /* DW_TAG_* */
    array_of(struct dwarf_attr) attributes;
    array_of(struct dwarf_die *) children;
};

/* Each compile unit has a root DIE with code DW_UT_compile. */
static struct dwarf_die *dwarf_root_die;

static size_t ULEB128_encode(unsigned char *buf, unsigned long value)
{
    size_t i;
    unsigned char b;

    i = 0;
    do {
        i++;
        b = value & 0x7f;
        value >>= 7;
        if (value) {
            b |= 0x80;
        }
        *buf = b;
        buf += 1;
    } while (value);

    return i;
}

static void dwarf_write_ULEB128(int shid, unsigned long value)
{
    unsigned char buf[8];
    size_t len;

    len = ULEB128_encode(buf, value);
    elf_section_write(shid, buf, len);

    assert(len == 1);
}

static struct dwarf_die *dwarf_add_entry(int code, int tag)
{
    struct dwarf_die *die;

    die = calloc(1, sizeof(*die));
    die->code = code;
    die->tag = tag;
    return die;
}

static void dwarf_add_attribute(struct dwarf_die *die, int name, int form, ...)
{
    struct dwarf_attr attr = {0};
    va_list args;

    attr.name = name;
    attr.form = form;

    va_start(args, form);
    switch (form) {
    case DW_FORM_addr:
        attr.value.sym = va_arg(args, const struct symbol *);
        break;
    case DW_FORM_data1:
        attr.value.num = va_arg(args, int);
        break;
    case DW_FORM_data8:
        attr.value.num = va_arg(args, long);
        break;
    case DW_FORM_string:
        attr.value.str = va_arg(args, const char *);
        break;
    default: assert(0);
        break;
    }

    va_end(args);
    array_push_back(&die->attributes, attr);
}

static void dwarf_write_attribute(const struct dwarf_attr *attr)
{
    size_t size;

    if (!attr) {
        dwarf_write_ULEB128(section.debug_abbrev, 0);
        dwarf_write_ULEB128(section.debug_abbrev, 0);
    } else {
        dwarf_write_ULEB128(section.debug_abbrev, attr->name);
        dwarf_write_ULEB128(section.debug_abbrev, attr->form);

        switch (attr->form) {
        default: assert(0);
        case DW_FORM_addr:
            elf_add_relocation(section.rela_debug_info,
                attr->value.sym, R_X86_64_64, 0, 0);
            elf_section_write(section.debug_info, NULL, 8);
            break;
        case DW_FORM_string:
            size = strlen(attr->value.str) + 1;
            elf_section_write(section.debug_info, attr->value.str, size);
            break;
        case DW_FORM_data1:
            elf_section_write(section.debug_info, &attr->value.num, 1);
            break;
        case DW_FORM_data8:
            elf_section_write(section.debug_info, &attr->value.num, 8);
            break;
        }
    }
}

static void dwarf_write_entry(struct dwarf_die *die)
{
    int i;
    struct dwarf_attr *attr;
    struct dwarf_die *child;
    unsigned char children[] = {DW_CHILDREN_no, DW_CHILDREN_yes};

    assert(die);
    dwarf_write_ULEB128(section.debug_info, die->code);
    dwarf_write_ULEB128(section.debug_abbrev, die->code);
    dwarf_write_ULEB128(section.debug_abbrev, die->tag);
    elf_section_write(section.debug_abbrev,
        &children[array_len(&die->children) != 0], 1);

    for (i = 0; i < array_len(&die->attributes); ++i) {
        attr = &array_get(&die->attributes, i);
        dwarf_write_attribute(attr);
    }

    dwarf_write_attribute(NULL);

    for (i = 0; i < array_len(&die->children); ++i) {
        child = array_get(&die->children, i);
        dwarf_write_entry(child);
    }

    array_clear(&die->attributes);
    array_clear(&die->children);
    free(die);
}

INTERNAL int dwarf_init(const char *filename)
{
    struct dwarf_die *die;

    die = dwarf_add_entry(DW_UT_compile, DW_TAG_compile_unit);
    dwarf_add_attribute(die, DW_AT_name, DW_FORM_string, filename);
    dwarf_add_attribute(die, DW_AT_producer, DW_FORM_string, "lacc");
    switch (context.standard) {
    case STD_C89:
        dwarf_add_attribute(die, DW_AT_language, DW_FORM_data1, DW_LANG_C89);
        break;
    case STD_C99:
        dwarf_add_attribute(die, DW_AT_language, DW_FORM_data1, DW_LANG_C99);
        break;
    case STD_C11:
        dwarf_add_attribute(die, DW_AT_language, DW_FORM_data1, DW_LANG_C11);
        break;
    }

    section.debug_info = elf_section_init(
        ".debug_info", SHT_PROGBITS, 0, SHN_UNDEF, 0, 8, 0);

    section.rela_debug_info = elf_section_init(
        ".rela.debug_info", SHT_RELA, 0, section.symtab, section.debug_info, 8,
        sizeof(Elf64_Rela));

    section.debug_abbrev = elf_section_init(
        ".debug_abbrev", SHT_PROGBITS, 0, SHN_UNDEF, 0, 8, 0);

    dwarf_root_die = die;
    return 0;
}

static void dwarf_add_root_info(struct dwarf_die *die)
{
    const struct symbol *sym;
    size_t size;

    size = elf_section_write(section.text, NULL, 0);
    if (size) {
        sym = elf_section_symbol(section.text);
        dwarf_add_attribute(die, DW_AT_low_pc, DW_FORM_addr, sym);
        dwarf_add_attribute(die, DW_AT_high_pc, DW_FORM_data8, size);
    }
}

INTERNAL int dwarf_flush(void)
{
    char *buffer;
    size_t length;

    unsigned int unit_length = 0;
    unsigned short version = 4;
    unsigned int debug_abbrev_offset = 0;
    unsigned char address_size = 8;

    elf_section_write(section.debug_info, &unit_length, 4);
    elf_section_write(section.debug_info, &version, 2);
    elf_section_write(section.debug_info, &debug_abbrev_offset, 4);
    elf_section_write(section.debug_info, &address_size, 1);

    /* Put more metadata on root. */
    dwarf_add_root_info(dwarf_root_die);

    /* Write all entries with children recursively. */
    dwarf_write_entry(dwarf_root_die);

    /* End with zero byte. */
    length = elf_section_write(section.debug_info, NULL, 1) + 1;
    assert(length < 0xfffffff0);

    /*
     * Length of .debug_data contribution for this compilation unit,
     * not including itself. We use 32-bit format, so the length must
     * fit in an integer.
     */
    unit_length = length - 4;
    buffer = elf_section_buffer(section.debug_info);
    memcpy(buffer, &unit_length, 4);
    return 0;
}
