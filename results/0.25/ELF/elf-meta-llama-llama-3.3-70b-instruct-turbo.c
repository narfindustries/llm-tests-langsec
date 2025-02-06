#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define EI_NIDENT 16

typedef enum {
    ET_NONE = 0,
    ET_REL = 1,
    ET_EXEC = 2,
    ET_DYN = 3,
    ET_CORE = 4
} Elf_Type;

typedef enum {
    EM_NONE = 0,
    EM_M32 = 1,
    EM_SPARC = 2,
    EM_386 = 3,
    EM_68K = 4,
    EM_88K = 5,
    EM_860 = 7,
    EM_MIPS = 8,
    EM_PARISC = 15,
    EM_PPC = 20,
    EM_PPC64 = 21,
    EM_S390 = 22,
    EM_SPU = 23,
    EM_V800 = 36,
    EM_FR20 = 37,
    EM_RH32 = 38,
    EM_RCE = 39,
    EM_ARM = 40,
    EM_ALPHA = 41,
    EM_SH = 42,
    EM_SPARCV9 = 43,
    EM_TRICORE = 44,
    EM_ARC = 45,
    EM_H8_300 = 46,
    EM_H8_300H = 47,
    EM_H8S = 48,
    EM_H8_500 = 49,
    EM_IA_64 = 50,
    EM_MIPS_X = 51,
    EM_COLDFIRE = 52,
    EM_68HC12 = 53,
    EM_MMA = 54,
    EM_PCP = 55,
    EM_NCPU = 56,
    EM_NDR1 = 57,
    EM_STARCORE = 58,
    EM_ME16 = 59,
    EM_ST100 = 60,
    EM_TINYJ = 61,
    EM_X86_64 = 62,
    EM_PDSP = 63,
    EM_PDP10 = 64,
    EM_PDP11 = 65,
    EM_FX66 = 66,
    EM_ST9PLUS = 67,
    EM_ST7 = 68,
    EM_68HC16 = 69,
    EM_68HC11 = 70,
    EM_68HC08 = 71,
    EM_68HC05 = 72,
    EM_SVX = 73,
    EM_ST19 = 74,
    EM_VAX = 75,
    EM_CRIS = 76,
    EM_JAVELIN = 77,
    EM_FIREPATH = 78,
    EM_ZSP = 79,
    EM_MMIX = 80,
    EM_HUANY = 81,
    EM_PRISM = 82,
    EM_AVR = 83,
    EM_FR30 = 84,
    EM_D10V = 85,
    EM_D30V = 86,
    EM_V850 = 87,
    EM_M32R = 88,
    EM_MN10300 = 89,
    EM_MN10200 = 90,
    EM_PJ = 91,
    EM_OPENRISC = 92,
    EM_ARC_A5 = 93,
    EM_XTENSA = 94,
    EM_NUM = 95
} Elf_Machine;

typedef enum {
    PT_NULL = 0,
    PT_LOAD = 1,
    PT_DYNAMIC = 2,
    PT_INTERP = 3,
    PT_NOTE = 4,
    PT_SHLIB = 5,
    PT_PHDR = 6,
    PT_TLS = 7,
    PT_NUM = 8,
    PT_LOOS = 0x60000000,
    PT_HIOS = 0x6fffffff,
    PT_LOPROC = 0x70000000,
    PT_HIPROC = 0x7fffffff
} Elf_Phdr_Type;

typedef enum {
    SHT_NULL = 0,
    SHT_PROGBITS = 1,
    SHT_SYMTAB = 2,
    SHT_STRTAB = 3,
    SHT_RELA = 4,
    SHT_HASH = 5,
    SHT_DYNAMIC = 6,
    SHT_NOTE = 7,
    SHT_NOBITS = 8,
    SHT_REL = 9,
    SHT_SHLIB = 10,
    SHT_DYNSYM = 11,
    SHT_INIT_ARRAY = 14,
    SHT_FINI_ARRAY = 15,
    SHT_PREINIT_ARRAY = 16,
    SHT_GROUP = 17,
    SHT_SYMTAB_SHNDX = 18,
    SHT_NUM = 19,
    SHT_LOOS = 0x60000000,
    SHT_HIOS = 0x6fffffff,
    SHT_LOPROC = 0x70000000,
    SHT_HIPROC = 0x7fffffff,
    SHT_LOUSER = 0x80000000,
    SHT_HIUSER = 0x8fffffff
} Elf_Shdr_Type;

typedef enum {
    SHF_WRITE = 0x1,
    SHF_ALLOC = 0x2,
    SHF_EXECINSTR = 0x4,
    SHF_MERGE = 0x10,
    SHF_STRINGS = 0x20,
    SHF_INFO_LINK = 0x40,
    SHF_LINK_ORDER = 0x80,
    SHF_OS_NONCONFORMING = 0x100,
    SHF_GROUP = 0x200,
    SHF_TLS = 0x400,
    SHF_MASKOS = 0x0ff00000,
    SHF_MASKPROC = 0xf0000000
} Elf_Shdr_Flag;

typedef enum {
    STB_LOCAL = 0,
    STB_GLOBAL = 1,
    STB_WEAK = 2,
    STB_NUM = 3,
    STB_LOOS = 10,
    STB_HIOS = 12,
    STB_LOPROC = 13,
    STB_HIPROC = 15
} Elf_Sym_Binding;

typedef enum {
    STT_NOTYPE = 0,
    STT_OBJECT = 1,
    STT_FUNC = 2,
    STT_SECTION = 3,
    STT_FILE = 4,
    STT_COMMON = 5,
    STT_TLS = 6,
    STT_NUM = 7,
    STT_LOOS = 10,
    STT_HIOS = 12,
    STT_LOPROC = 13,
    STT_HIPROC = 15
} Elf_Sym_Type;

typedef enum {
    DT_NULL = 0,
    DT_NEEDED = 1,
    DT_PLTRELSZ = 2,
    DT_PLTGOT = 3,
    DT_HASH = 4,
    DT_STRTAB = 5,
    DT_SYMTAB = 6,
    DT_RELA = 7,
    DT_RELASZ = 8,
    DT_RELAENT = 9,
    DT_STRSZ = 10,
    DT_SYMENT = 11,
    DT_INIT = 12,
    DT_FINI = 13,
    DT_SONAME = 14,
    DT_RPATH = 15,
    DT_SYMBOLIC = 16,
    DT_REL = 17,
    DT_RELSZ = 18,
    DT_RELENT = 19,
    DT_PLTREL = 20,
    DT_DEBUG = 21,
    DT_TEXTREL = 22,
    DT_JMPREL = 23,
    DT_BIND_NOW = 24,
    DT_INIT_ARRAY = 25,
    DT_FINI_ARRAY = 26,
    DT_INIT_ARRAYSZ = 27,
    DT_FINI_ARRAYSZ = 28,
    DT_RUNPATH = 29,
    DT_FLAGS = 30,
    DT_PREINIT_ARRAY = 32,
    DT_PREINIT_ARRAYSZ = 33,
    DT_NUM = 34,
    DT_LOOS = 0x60000000,
    DT_HIOS = 0x6fffffff,
    DT_LOPROC = 0x70000000,
    DT_HIPROC = 0x7fffffff
} Elf_Dyn_Tag;

typedef struct {
    unsigned char e_ident[EI_NIDENT];
    Elf_Type e_type;
    Elf_Machine e_machine;
    unsigned int e_version;
    unsigned long e_entry;
    unsigned long e_phoff;
    unsigned long e_shoff;
    unsigned int e_flags;
    unsigned short e_ehsize;
    unsigned short e_phentsize;
    unsigned short e_phnum;
    unsigned short e_shentsize;
    unsigned short e_shnum;
    unsigned short e_shstrndx;
} Elf32_Ehdr;

typedef struct {
    unsigned char e_ident[EI_NIDENT];
    Elf_Type e_type;
    Elf_Machine e_machine;
    unsigned int e_version;
    unsigned long long e_entry;
    unsigned long long e_phoff;
    unsigned long long e_shoff;
    unsigned int e_flags;
    unsigned short e_ehsize;
    unsigned short e_phentsize;
    unsigned short e_phnum;
    unsigned short e_shentsize;
    unsigned short e_shnum;
    unsigned short e_shstrndx;
} Elf64_Ehdr;

typedef struct {
    Elf_Phdr_Type p_type;
    unsigned long p_offset;
    unsigned long p_vaddr;
    unsigned long p_paddr;
    unsigned long p_filesz;
    unsigned long p_memsz;
    unsigned long p_flags;
    unsigned long p_align;
} Elf32_Phdr;

typedef struct {
    Elf_Phdr_Type p_type;
    unsigned long long p_offset;
    unsigned long long p_vaddr;
    unsigned long long p_paddr;
    unsigned long long p_filesz;
    unsigned long long p_memsz;
    unsigned long p_flags;
    unsigned long long p_align;
} Elf64_Phdr;

typedef struct {
    unsigned int sh_name;
    Elf_Shdr_Type sh_type;
    Elf_Shdr_Flag sh_flags;
    unsigned long sh_addr;
    unsigned long sh_offset;
    unsigned long sh_size;
    unsigned int sh_link;
    unsigned int sh_info;
    unsigned long sh_addralign;
    unsigned long sh_entsize;
} Elf32_Shdr;

typedef struct {
    unsigned int sh_name;
    Elf_Shdr_Type sh_type;
    Elf_Shdr_Flag sh_flags;
    unsigned long long sh_addr;
    unsigned long long sh_offset;
    unsigned long long sh_size;
    unsigned int sh_link;
    unsigned int sh_info;
    unsigned long long sh_addralign;
    unsigned long long sh_entsize;
} Elf64_Shdr;

typedef struct {
    unsigned int st_name;
    unsigned char st_info;
    unsigned char st_other;
    unsigned short st_shndx;
    unsigned long st_value;
    unsigned long st_size;
} Elf32_Sym;

typedef struct {
    unsigned int st_name;
    unsigned char st_info;
    unsigned char st_other;
    unsigned short st_shndx;
    unsigned long long st_value;
    unsigned long long st_size;
} Elf64_Sym;

typedef struct {
    unsigned int d_tag;
    unsigned long d_un;
} Elf32_Dyn;

typedef struct {
    unsigned int d_tag;
    unsigned long long d_un;
} Elf64_Dyn;

typedef struct {
    unsigned int r_offset;
    unsigned int r_info;
} Elf32_Rel;

typedef struct {
    unsigned long long r_offset;
    unsigned int r_info;
    unsigned long long r_addend;
} Elf64_Rel;

typedef struct {
    unsigned long long r_offset;
    unsigned int r_info;
    unsigned long long r_addend;
} Elf64_Rela;

HParser* HParser_new(void) {
    HParser* parser = malloc(sizeof(HParser));
    return parser;
}

void HParser_set_input(HParser* parser, FILE* file) {
    parser->file = file;
}

void HParser_def(HParser* parser, const char* name, HParser* definition) {
    parser->definitions[name] = definition;
}

HParser* HParser_seq(HParser* parser, ...) {
    va_list args;
    va_start(args, parser);
    HParser* seq = malloc(sizeof(HParser));
    seq->type = HParser_SEQ;
    seq->args = args;
    va_end(args);
    return seq;
}

HParser* HParser_bytes(int size) {
    HParser* bytes = malloc(sizeof(HParser));
    bytes->type = HParser_BYTES;
    bytes->size = size;
    return bytes;
}

HParser* HParser_uint16(void) {
    HParser* uint16 = malloc(sizeof(HParser));
    uint16->type = HParser_UINT16;
    return uint16;
}

HParser* HParser_uint32(void) {
    HParser* uint32 = malloc(sizeof(HParser));
    uint32->type = HParser_UINT32;
    return uint32;
}

HParser* HParser_uint64(void) {
    HParser* uint64 = malloc(sizeof(HParser));
    uint64->type = HParser_UINT64;
    return uint64;
}

HParser* HParser_uint8(void) {
    HParser* uint8 = malloc(sizeof(HParser));
    uint8->type = HParser_UINT8;
    return uint8;
}

HParser* HParser_call(const char* name) {
    HParser* call = malloc(sizeof(HParser));
    call->type = HParser_CALL;
    call->name = name;
    return call;
}

HParser* HParser_repeat(HParser* parser, HParser* count) {
    HParser* repeat = malloc(sizeof(HParser));
    repeat->type = HParser_REPEAT;
    repeat->parser = parser;
    repeat->count = count;
    return repeat;
}

void HParser_parse(HParser* parser, const char* name) {
    // implementation of HParser_parse
}

void HParser_free(HParser* parser) {
    free(parser);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }

    HParser *parser = HParser_new();
    HParser_set_input(parser, file);

    HParser_def(parser, "elf32_ehdr", HParser_seq(
        HParser_bytes(EI_NIDENT),
        HParser_uint16(),
        HParser_uint16(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint16(),
        HParser_uint16(),
        HParser_uint16(),
        HParser_uint16(),
        HParser_uint16(),
        HParser_uint16()
    ));

    HParser_def(parser, "elf64_ehdr", HParser_seq(
        HParser_bytes(EI_NIDENT),
        HParser_uint16(),
        HParser_uint16(),
        HParser_uint32(),
        HParser_uint64(),
        HParser_uint64(),
        HParser_uint64(),
        HParser_uint32(),
        HParser_uint16(),
        HParser_uint16(),
        HParser_uint16(),
        HParser_uint16(),
        HParser_uint16()
    ));

    HParser_def(parser, "elf32_phdr", HParser_seq(
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32()
    ));

    HParser_def(parser, "elf64_phdr", HParser_seq(
        HParser_uint32(),
        HParser_uint64(),
        HParser_uint64(),
        HParser_uint64(),
        HParser_uint64(),
        HParser_uint64(),
        HParser_uint32(),
        HParser_uint64()
    ));

    HParser_def(parser, "elf32_shdr", HParser_seq(
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint32()
    ));

    HParser_def(parser, "elf64_shdr", HParser_seq(
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint64(),
        HParser_uint64(),
        HParser_uint64(),
        HParser_uint32(),
        HParser_uint32(),
        HParser_uint64(),
        HParser_uint64()
    ));

    HParser_def(parser, "elf32_sym", HParser_seq(
        HParser_uint32(),
        HParser_uint8(),
        HParser_uint8(),
        HParser_uint16(),
        HParser_uint32(),
        HParser_uint32()
    ));

    HParser_def(parser, "elf64_sym", HParser_seq(
        HParser_uint32(),
        HParser_uint8(),
        HParser_uint8(),
        HParser_uint16(),
        HParser_uint64(),
        HParser_uint64()
    ));

    HParser_def(parser, "elf32_dyn", HParser_seq(
        HParser_uint32(),
        HParser_uint32()
    ));

    HParser_def(parser, "elf64_dyn", HParser_seq(
        HParser_uint32(),
        HParser_uint64()
    ));

    HParser_def(parser, "elf32_rel", HParser_seq(
        HParser_uint32(),
        HParser_uint32()
    ));

    HParser_def(parser, "elf64_rel", HParser_seq(
        HParser_uint64(),
        HParser_uint32(),
        HParser_uint64()
    ));

    HParser_def(parser, "elf64_rela", HParser_seq(
        HParser_uint64(),
        HParser_uint32(),
        HParser_uint64()
    ));

    HParser_def(parser, "elf32", HParser_seq(
        HParser_call("elf32_ehdr"),
        HParser_repeat(HParser_call("elf32_phdr"), HParser_uint16()),
        HParser_repeat(HParser_call("elf32_shdr"), HParser_uint16())
    ));

    HParser_def(parser, "elf64", HParser_seq(
        HParser_call("elf64_ehdr"),
        HParser_repeat(HParser_call("elf64_phdr"), HParser_uint16()),
        HParser_repeat(HParser_call("elf64_shdr"), HParser_uint16())
    ));

    HParser_parse(parser, "elf32");
    HParser_parse(parser, "elf64");

    fclose(file);
    HParser_free(parser);
    return 0;
}