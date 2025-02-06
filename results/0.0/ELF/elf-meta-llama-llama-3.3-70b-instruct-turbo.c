#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define EI_NIDENT 16

typedef enum {
    ET_NONE,
    ET_REL,
    ET_EXEC,
    ET_DYN,
    ET_CORE
} e_type;

typedef enum {
    EM_NONE,
    EM_M32,
    EM_SPARC,
    EM_386,
    EM_68K,
    EM_88K,
    EM_860,
    EM_MIPS,
    EM_S370,
    EM_MIPS_RS3_LE
} e_machine;

typedef enum {
    SHT_NULL,
    SHT_PROGBITS,
    SHT_SYMTAB,
    SHT_STRTAB,
    SHT_RELA,
    SHT_HASH,
    SHT_DYNAMIC,
    SHT_NOTE,
    SHT_NOBITS,
    SHT_REL,
    SHT_SHLIB,
    SHT_DYNSYM
} sht_type;

typedef enum {
    SHF_WRITE,
    SHF_ALLOC,
    SHF_EXECINSTR
} shf_flag;

typedef enum {
    STB_LOCAL,
    STB_GLOBAL,
    STB_WEAK
} stb_binding;

typedef enum {
    STT_NOTYPE,
    STT_OBJECT,
    STT_FUNC,
    STT_SECTION,
    STT_FILE
} stt_type;

typedef enum {
    DT_NULL,
    DT_NEEDED,
    DT_PLTRELSZ,
    DT_PLTGOT,
    DT_HASH,
    DT_STRTAB,
    DT_SYMTAB,
    DT_RELA,
    DT_RELASZ,
    DT_RELAENT,
    DT_STRSZ,
    DT_SYMENT,
    DT_INIT,
    DT_FINI,
    DT_SONAME,
    DT_RPATH,
    DT_SYMBOLIC,
    DT_REL,
    DT_RELSZ,
    DT_RELENT,
    DT_PLTREL,
    DT_DEBUG,
    DT_TEXTREL,
    DT_JMPREL,
    DT_BIND_NOW,
    DT_INIT_ARRAY,
    DT_FINI_ARRAY,
    DT_INIT_ARRAYSZ,
    DT_FINI_ARRAYSZ,
    DT_RUNPATH,
    DT_FLAGS,
    DT_PREINIT_ARRAY,
    DT_PREINIT_ARRAYSZ,
    DT_MAXPOSTAGS
} dt_tag;

typedef struct {
    uint8_t e_ident[EI_NIDENT];
    e_type e_type;
    e_machine e_machine;
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
} elf_header;

typedef struct {
    uint32_t p_type;
    uint32_t p_offset;
    uint32_t p_vaddr;
    uint32_t p_paddr;
    uint32_t p_filesz;
    uint32_t p_memsz;
    uint32_t p_flags;
    uint32_t p_align;
} program_header;

typedef struct {
    uint32_t sh_name;
    sht_type sh_type;
    uint32_t sh_flags;
    uint32_t sh_addr;
    uint32_t sh_offset;
    uint32_t sh_size;
    uint32_t sh_link;
    uint32_t sh_info;
    uint32_t sh_addralign;
    uint32_t sh_entsize;
} section_header;

typedef struct {
    uint32_t st_name;
    uint32_t st_value;
    uint32_t st_size;
    uint8_t st_info;
    uint8_t st_other;
    uint16_t st_shndx;
} symbol;

typedef struct {
    uint32_t r_offset;
    uint32_t r_info;
    uint32_t r_addend;
} relocation;

typedef struct {
    dt_tag d_tag;
    uint32_t d_un;
} dynamic;

void elf_header_p(void *data, int size, void **out) {
    elf_header *header = malloc(sizeof(elf_header));
    memcpy(header, data, sizeof(elf_header));
    *out = header;
}

void program_header_p(void *data, int size, void **out) {
    program_header *header = malloc(sizeof(program_header));
    memcpy(header, data, sizeof(program_header));
    *out = header;
}

void section_header_p(void *data, int size, void **out) {
    section_header *header = malloc(sizeof(section_header));
    memcpy(header, data, sizeof(section_header));
    *out = header;
}

void symbol_p(void *data, int size, void **out) {
    symbol *sym = malloc(sizeof(symbol));
    memcpy(sym, data, sizeof(symbol));
    *out = sym;
}

void relocation_p(void *data, int size, void **out) {
    relocation *rel = malloc(sizeof(relocation));
    memcpy(rel, data, sizeof(relocation));
    *out = rel;
}

void dynamic_p(void *data, int size, void **out) {
    dynamic *dyn = malloc(sizeof(dynamic));
    memcpy(dyn, data, sizeof(dynamic));
    *out = dyn;
}

void elf_p(void *data, int size, void **out) {
    elf_header *header = malloc(sizeof(elf_header));
    memcpy(header, data, sizeof(elf_header));

    program_header *program_headers = malloc(header->e_phnum * sizeof(program_header));
    memcpy(program_headers, (uint8_t *)data + header->e_phoff, header->e_phnum * sizeof(program_header));

    section_header *section_headers = malloc(header->e_shnum * sizeof(section_header));
    memcpy(section_headers, (uint8_t *)data + header->e_shoff, header->e_shnum * sizeof(section_header));

    symbol *symbols = malloc(section_headers[1].sh_size / sizeof(symbol));
    memcpy(symbols, (uint8_t *)data + section_headers[1].sh_offset, section_headers[1].sh_size);

    relocation *relocations = malloc(section_headers[2].sh_size / sizeof(relocation));
    memcpy(relocations, (uint8_t *)data + section_headers[2].sh_offset, section_headers[2].sh_size);

    dynamic *dynamics = malloc(section_headers[3].sh_size / sizeof(dynamic));
    memcpy(dynamics, (uint8_t *)data + section_headers[3].sh_offset, section_headers[3].sh_size);

    *out = header;
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

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    rewind(file);

    uint8_t *data = malloc(size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    if (fread(data, size, 1, file) != 1) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    void *result;
    elf_p(data, size, &result);

    elf_header *header = result;

    printf("ELF Header:\n");
    printf("  e_type: %d\n", header->e_type);
    printf("  e_machine: %d\n", header->e_machine);
    printf("  e_version: %d\n", header->e_version);
    printf("  e_entry: %d\n", header->e_entry);
    printf("  e_phoff: %d\n", header->e_phoff);
    printf("  e_shoff: %d\n", header->e_shoff);
    printf("  e_flags: %d\n", header->e_flags);
    printf("  e_ehsize: %d\n", header->e_ehsize);
    printf("  e_phentsize: %d\n", header->e_phentsize);
    printf("  e_phnum: %d\n", header->e_phnum);
    printf("  e_shentsize: %d\n", header->e_shentsize);
    printf("  e_shnum: %d\n", header->e_shnum);
    printf("  e_shstrndx: %d\n", header->e_shstrndx);

    program_header *program_headers = (program_header *)((uint8_t *)data + header->e_phoff);
    printf("Program Headers:\n");
    for (int i = 0; i < header->e_phnum; i++) {
        printf("  p_type: %d\n", program_headers[i].p_type);
        printf("  p_offset: %d\n", program_headers[i].p_offset);
        printf("  p_vaddr: %d\n", program_headers[i].p_vaddr);
        printf("  p_paddr: %d\n", program_headers[i].p_paddr);
        printf("  p_filesz: %d\n", program_headers[i].p_filesz);
        printf("  p_memsz: %d\n", program_headers[i].p_memsz);
        printf("  p_flags: %d\n", program_headers[i].p_flags);
        printf("  p_align: %d\n", program_headers[i].p_align);
    }

    section_header *section_headers = (section_header *)((uint8_t *)data + header->e_shoff);
    printf("Section Headers:\n");
    for (int i = 0; i < header->e_shnum; i++) {
        printf("  sh_name: %d\n", section_headers[i].sh_name);
        printf("  sh_type: %d\n", section_headers[i].sh_type);
        printf("  sh_flags: %d\n", section_headers[i].sh_flags);
        printf("  sh_addr: %d\n", section_headers[i].sh_addr);
        printf("  sh_offset: %d\n", section_headers[i].sh_offset);
        printf("  sh_size: %d\n", section_headers[i].sh_size);
        printf("  sh_link: %d\n", section_headers[i].sh_link);
        printf("  sh_info: %d\n", section_headers[i].sh_info);
        printf("  sh_addralign: %d\n", section_headers[i].sh_addralign);
        printf("  sh_entsize: %d\n", section_headers[i].sh_entsize);
    }

    symbol *symbols = (symbol *)((uint8_t *)data + section_headers[1].sh_offset);
    printf("Symbols:\n");
    for (int i = 0; i < section_headers[1].sh_size / sizeof(symbol); i++) {
        printf("  st_name: %d\n", symbols[i].st_name);
        printf("  st_value: %d\n", symbols[i].st_value);
        printf("  st_size: %d\n", symbols[i].st_size);
        printf("  st_info: %d\n", symbols[i].st_info);
        printf("  st_other: %d\n", symbols[i].st_other);
        printf("  st_shndx: %d\n", symbols[i].st_shndx);
    }

    relocation *relocations = (relocation *)((uint8_t *)data + section_headers[2].sh_offset);
    printf("Relocations:\n");
    for (int i = 0; i < section_headers[2].sh_size / sizeof(relocation); i++) {
        printf("  r_offset: %d\n", relocations[i].r_offset);
        printf("  r_info: %d\n", relocations[i].r_info);
        printf("  r_addend: %d\n", relocations[i].r_addend);
    }

    dynamic *dynamics = (dynamic *)((uint8_t *)data + section_headers[3].sh_offset);
    printf("Dynamics:\n");
    for (int i = 0; i < section_headers[3].sh_size / sizeof(dynamic); i++) {
        printf("  d_tag: %d\n", dynamics[i].d_tag);
        printf("  d_un: %d\n", dynamics[i].d_un);
    }

    free(data);
    return 0;
}