#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define EI_NIDENT 16

typedef enum {
    ET_NONE = 0,
    ET_REL = 1,
    ET_EXEC = 2,
    ET_DYN = 3,
    ET_CORE = 4
} ET;

typedef enum {
    EM_NONE = 0,
    EM_M32 = 1,
    EM_SPARC = 2,
    EM_386 = 3,
    EM_68K = 4,
    EM_88K = 5,
    EM_860 = 7,
    EM_MIPS = 8
} EM;

typedef enum {
    EV_NONE = 0,
    EV_CURRENT = 1
} EV;

typedef enum {
    EI_MAG0 = 0,
    EI_MAG1,
    EI_MAG2,
    EI_MAG3,
    EI_CLASS,
    EI_DATA,
    EI_VERSION,
    EI_OSABI,
    EI_ABIVERSION,
    EI_PAD
} EI;

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
    SHT_SYMTAB_SHNDX = 18
} SHT;

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
    SHF_COMPRESSED = 0x800
} SHF;

typedef enum {
    PT_NULL = 0,
    PT_LOAD = 1,
    PT_DYNAMIC = 2,
    PT_INTERP = 3,
    PT_NOTE = 4,
    PT_SHLIB = 5,
    PT_PHDR = 6,
    PT_LOPROC = 0x70000000,
    PT_HIPROC = 0x7fffffff
} PT;

typedef enum {
    PF_X = 0x1,
    PF_W = 0x2,
    PF_R = 0x4
} PF;

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
    DT_MAXPOSTAGS = 34,
    DT_LOPROC = 0x70000000,
    DT_HIPROC = 0x7fffffff
} DT;

typedef struct {
    uint8_t e_ident[EI_NIDENT];
    uint16_t e_type;
    uint16_t e_machine;
    uint32_t e_version;
    uint64_t e_entry;
    uint64_t e_phoff;
    uint64_t e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize;
    uint16_t e_phentsize;
    uint16_t e_phnum;
    uint16_t e_shentsize;
    uint16_t e_shnum;
    uint16_t e_shstrndx;
} Elf64_Ehdr;

typedef struct {
    uint8_t e_ident[EI_NIDENT];
    uint16_t e_type;
    uint16_t e_machine;
    uint32_t e_version;
    uint32_t e_entry;
    uint32_t e_phoff;
    uint32_t e_shoff;
    uint32_t e_flags;
    uint16_t e_ehsize;
    uint16_t e_phentsize;
    uint16_t e_phnum;
    uint16_t e_shentsize;
    uint16_t e_shnum;
    uint16_t e_shstrndx;
} Elf32_Ehdr;

typedef struct {
    uint32_t sh_name;
    uint32_t sh_type;
    uint32_t sh_flags;
    uint32_t sh_addr;
    uint32_t sh_offset;
    uint32_t sh_size;
    uint32_t sh_link;
    uint32_t sh_info;
    uint32_t sh_addralign;
    uint32_t sh_entsize;
} Elf32_Shdr;

typedef struct {
    uint64_t sh_name;
    uint64_t sh_type;
    uint64_t sh_flags;
    uint64_t sh_addr;
    uint64_t sh_offset;
    uint64_t sh_size;
    uint64_t sh_link;
    uint64_t sh_info;
    uint64_t sh_addralign;
    uint64_t sh_entsize;
} Elf64_Shdr;

typedef struct {
    uint32_t p_type;
    uint32_t p_offset;
    uint32_t p_vaddr;
    uint32_t p_paddr;
    uint32_t p_filesz;
    uint32_t p_memsz;
    uint32_t p_flags;
    uint32_t p_align;
} Elf32_Phdr;

typedef struct {
    uint64_t p_type;
    uint64_t p_offset;
    uint64_t p_vaddr;
    uint64_t p_paddr;
    uint64_t p_filesz;
    uint64_t p_memsz;
    uint64_t p_flags;
    uint64_t p_align;
} Elf64_Phdr;

typedef struct {
    uint32_t d_tag;
    union {
        uint32_t d_val;
        uint32_t d_ptr;
        uint32_t d_off;
    } d_un;
} Elf32_Dyn;

typedef struct {
    uint64_t d_tag;
    union {
        uint64_t d_val;
        uint64_t d_ptr;
        uint64_t d_off;
    } d_un;
} Elf64_Dyn;

HParser* elf64_parser() {
    HParser* e_ident = h_bytes(EI_NIDENT);
    HParser* e_type = h_word();
    HParser* e_machine = h_word();
    HParser* e_version = h_dword();
    HParser* e_entry = h_qword();
    HParser* e_phoff = h_qword();
    HParser* e_shoff = h_qword();
    HParser* e_flags = h_dword();
    HParser* e_ehsize = h_word();
    HParser* e_phentsize = h_word();
    HParser* e_phnum = h_word();
    HParser* e_shentsize = h_word();
    HParser* e_shnum = h_word();
    HParser* e_shstrndx = h_word();

    HParser* elf64_header = h_struct(
        e_ident,
        e_type,
        e_machine,
        e_version,
        e_entry,
        e_phoff,
        e_shoff,
        e_flags,
        e_ehsize,
        e_phentsize,
        e_phnum,
        e_shentsize,
        e_shnum,
        e_shstrndx
    );

    return elf64_header;
}

HParser* elf32_parser() {
    HParser* e_ident = h_bytes(EI_NIDENT);
    HParser* e_type = h_word();
    HParser* e_machine = h_word();
    HParser* e_version = h_dword();
    HParser* e_entry = h_dword();
    HParser* e_phoff = h_dword();
    HParser* e_shoff = h_dword();
    HParser* e_flags = h_dword();
    HParser* e_ehsize = h_word();
    HParser* e_phentsize = h_word();
    HParser* e_phnum = h_word();
    HParser* e_shentsize = h_word();
    HParser* e_shnum = h_word();
    HParser* e_shstrndx = h_word();

    HParser* elf32_header = h_struct(
        e_ident,
        e_type,
        e_machine,
        e_version,
        e_entry,
        e_phoff,
        e_shoff,
        e_flags,
        e_ehsize,
        e_phentsize,
        e_phnum,
        e_shentsize,
        e_shnum,
        e_shstrndx
    );

    return elf32_header;
}

HParser* program_header_parser64() {
    HParser* p_type = h_qword();
    HParser* p_offset = h_qword();
    HParser* p_vaddr = h_qword();
    HParser* p_paddr = h_qword();
    HParser* p_filesz = h_qword();
    HParser* p_memsz = h_qword();
    HParser* p_flags = h_qword();
    HParser* p_align = h_qword();

    HParser* program_header = h_struct(
        p_type,
        p_offset,
        p_vaddr,
        p_paddr,
        p_filesz,
        p_memsz,
        p_flags,
        p_align
    );

    return program_header;
}

HParser* program_header_parser32() {
    HParser* p_type = h_dword();
    HParser* p_offset = h_dword();
    HParser* p_vaddr = h_dword();
    HParser* p_paddr = h_dword();
    HParser* p_filesz = h_dword();
    HParser* p_memsz = h_dword();
    HParser* p_flags = h_dword();
    HParser* p_align = h_dword();

    HParser* program_header = h_struct(
        p_type,
        p_offset,
        p_vaddr,
        p_paddr,
        p_filesz,
        p_memsz,
        p_flags,
        p_align
    );

    return program_header;
}

HParser* section_header_parser64() {
    HParser* sh_name = h_qword();
    HParser* sh_type = h_qword();
    HParser* sh_flags = h_qword();
    HParser* sh_addr = h_qword();
    HParser* sh_offset = h_qword();
    HParser* sh_size = h_qword();
    HParser* sh_link = h_qword();
    HParser* sh_info = h_qword();
    HParser* sh_addralign = h_qword();
    HParser* sh_entsize = h_qword();

    HParser* section_header = h_struct(
        sh_name,
        sh_type,
        sh_flags,
        sh_addr,
        sh_offset,
        sh_size,
        sh_link,
        sh_info,
        sh_addralign,
        sh_entsize
    );

    return section_header;
}

HParser* section_header_parser32() {
    HParser* sh_name = h_dword();
    HParser* sh_type = h_dword();
    HParser* sh_flags = h_dword();
    HParser* sh_addr = h_dword();
    HParser* sh_offset = h_dword();
    HParser* sh_size = h_dword();
    HParser* sh_link = h_dword();
    HParser* sh_info = h_dword();
    HParser* sh_addralign = h_dword();
    HParser* sh_entsize = h_dword();

    HParser* section_header = h_struct(
        sh_name,
        sh_type,
        sh_flags,
        sh_addr,
        sh_offset,
        sh_size,
        sh_link,
        sh_info,
        sh_addralign,
        sh_entsize
    );

    return section_header;
}

HParser* dynamic_section_parser64() {
    HParser* d_tag = h_qword();
    HParser* d_un_d_val = h_qword();
    HParser* d_un_d_ptr = h_qword();
    HParser* d_un_d_off = h_qword();

    HParser* dynamic_section = h_choice(
        h_struct(d_tag, d_un_d_val),
        h_struct(d_tag, d_un_d_ptr),
        h_struct(d_tag, d_un_d_off)
    );

    return dynamic_section;
}

HParser* dynamic_section_parser32() {
    HParser* d_tag = h_dword();
    HParser* d_un_d_val = h_dword();
    HParser* d_un_d_ptr = h_dword();
    HParser* d_un_d_off = h_dword();

    HParser* dynamic_section = h_choice(
        h_struct(d_tag, d_un_d_val),
        h_struct(d_tag, d_un_d_ptr),
        h_struct(d_tag, d_un_d_off)
    );

    return dynamic_section;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    HParser* elf64_header = elf64_parser();
    HParser* elf32_header = elf32_parser();

    HParser* program_header64 = program_header_parser64();
    HParser* program_header32 = program_header_parser32();

    HParser* section_header64 = section_header_parser64();
    HParser* section_header32 = section_header_parser32();

    HParser* dynamic_section64 = dynamic_section_parser64();
    HParser* dynamic_section32 = dynamic_section_parser32();

    HParser* elf64_file = h_bind(elf64_header, (HContinuation)(void*)h_array(program_header64, 1), NULL);
    HParser* elf32_file = h_bind(elf32_header, (HContinuation)(void*)h_array(program_header32, 1), NULL);

    HParser* elf_file = h_choice(elf64_file, elf32_file);

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    char* buffer = malloc(file_size);
    fread(buffer, file_size, 1, file);

    HParseResult* result = h_parse(elf_file, buffer, file_size);
    if (result->ok) {
        printf("Parsed ELF file successfully\n");
    } else {
        printf("Error parsing ELF file\n");
    }

    free(buffer);
    fclose(file);

    return 0;
}