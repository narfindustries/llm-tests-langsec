#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define EI_NIDENT 16

typedef enum {
    ELFCLASSNONE = 0,
    ELFCLASS32 = 1,
    ELFCLASS64 = 2
} Elf_Class;

typedef enum {
    ELFDATA2LSB = 1,
    ELFDATA2MSB = 2
} Elf_Data;

typedef enum {
    EV_CURRENT = 1
} Elf_Version;

typedef enum {
    ELFOSABI_NONE = 0,
    ELFOSABI_SYSV = 0,
    ELFOSABI_HPUX = 1,
    ELFOSABI_NETBSD = 2,
    ELFOSABI_LINUX = 3,
    ELFOSABI_SOLARIS = 6,
    ELFOSABI_AIX = 7,
    ELFOSABI_IRIX = 8,
    ELFOSABI_FREEBSD = 9,
    ELFOSABI_TRU64 = 10,
    ELFOSABI_MODESTO = 11,
    ELFOSABI_OPENBSD = 12,
    ELFOSABI_OPENVMS = 13,
    ELFOSABI_NSK = 14,
    ELFOSABI_AROS = 15
} Elf_OSABI;

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
    EM_S370 = 9,
    EM_MIPS_RS3_LE = 10
} Elf_Machine;

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
    SHT_DYNSYM = 11
} Elf_Section_Type;

typedef enum {
    SHF_WRITE = 0x1,
    SHF_ALLOC = 0x2,
    SHF_EXECINSTR = 0x4
} Elf_Section_Flag;

typedef enum {
    PT_NULL = 0,
    PT_LOAD = 1,
    PT_DYNAMIC = 2,
    PT_INTERP = 3,
    PT_NOTE = 4,
    PT_SHLIB = 5,
    PT_PHDR = 6
} Elf_Program_Type;

typedef enum {
    PF_X = 0x1,
    PF_W = 0x2,
    PF_R = 0x4
} Elf_Program_Flag;

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
    uint32_t sh_name;
    uint32_t sh_type;
    uint64_t sh_flags;
    uint64_t sh_addr;
    uint64_t sh_offset;
    uint64_t sh_size;
    uint32_t sh_link;
    uint32_t sh_info;
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
    uint32_t p_type;
    uint32_t p_flags;
    uint64_t p_offset;
    uint64_t p_vaddr;
    uint64_t p_paddr;
    uint64_t p_filesz;
    uint64_t p_memsz;
    uint64_t p_align;
} Elf64_Phdr;

#define parse_elf32(p) \
    h_sequence( \
        h_bytes_equal((uint8_t[]){0x7f, 'E', 'L', 'F', 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 16), \
        h_uint16(), \
        h_uint16(), \
        h_uint32(), \
        h_uint32(), \
        h_uint32(), \
        h_uint32(), \
        h_uint32(), \
        h_uint16(), \
        h_uint16(), \
        h_uint16(), \
        h_uint16(), \
        h_uint16() \
    )

#define parse_elf64(p) \
    h_sequence( \
        h_bytes_equal((uint8_t[]){0x7f, 'E', 'L', 'F', 2, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 16), \
        h_uint16(), \
        h_uint16(), \
        h_uint32(), \
        h_uint64(), \
        h_uint64(), \
        h_uint64(), \
        h_uint32(), \
        h_uint16(), \
        h_uint16(), \
        h_uint16(), \
        h_uint16(), \
        h_uint16() \
    )

#define parse_section_header(p) \
    h_sequence( \
        h_uint32(), \
        h_uint32(), \
        h_uint64(), \
        h_uint64(), \
        h_uint64(), \
        h_uint64(), \
        h_uint32(), \
        h_uint32(), \
        h_uint64(), \
        h_uint64() \
    )

#define parse_program_header32(p) \
    h_sequence( \
        h_uint32(), \
        h_uint32(), \
        h_uint32(), \
        h_uint32(), \
        h_uint32(), \
        h_uint32(), \
        h_uint32(), \
        h_uint32() \
    )

#define parse_program_header64(p) \
    h_sequence( \
        h_uint32(), \
        h_uint32(), \
        h_uint64(), \
        h_uint64(), \
        h_uint64(), \
        h_uint64(), \
        h_uint64(), \
        h_uint64() \
    )

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        printf("Failed to open file %s\n", argv[1]);
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t len = ftell(f);
    rewind(f);

    uint8_t *buf = malloc(len);
    fread(buf, 1, len, f);
    fclose(f);

    Elf32_Ehdr ehdr32;
    if (h_parse(parse_elf32(&ehdr32), buf, len)) {
        printf("ELF32 file\n");
        printf("  e_type: %u\n", ehdr32.e_type);
        printf("  e_machine: %u\n", ehdr32.e_machine);
        printf("  e_version: %u\n", ehdr32.e_version);
        printf("  e_entry: %u\n", ehdr32.e_entry);
        printf("  e_phoff: %u\n", ehdr32.e_phoff);
        printf("  e_shoff: %u\n", ehdr32.e_shoff);
        printf("  e_flags: %u\n", ehdr32.e_flags);
        printf("  e_ehsize: %u\n", ehdr32.e_ehsize);
        printf("  e_phentsize: %u\n", ehdr32.e_phentsize);
        printf("  e_phnum: %u\n", ehdr32.e_phnum);
        printf("  e_shentsize: %u\n", ehdr32.e_shentsize);
        printf("  e_shnum: %u\n", ehdr32.e_shnum);
        printf("  e_shstrndx: %u\n", ehdr32.e_shstrndx);

        for (size_t i = 0; i < ehdr32.e_phnum; i++) {
            Elf32_Phdr phdr;
            size_t offset = ehdr32.e_phoff + i * ehdr32.e_phentsize;
            if (h_parse(parse_program_header32(&phdr), buf + offset, ehdr32.e_phentsize)) {
                printf("  Program header %zu:\n", i);
                printf("    p_type: %u\n", phdr.p_type);
                printf("    p_offset: %u\n", phdr.p_offset);
                printf("    p_vaddr: %u\n", phdr.p_vaddr);
                printf("    p_paddr: %u\n", phdr.p_paddr);
                printf("    p_filesz: %u\n", phdr.p_filesz);
                printf("    p_memsz: %u\n", phdr.p_memsz);
                printf("    p_flags: %u\n", phdr.p_flags);
                printf("    p_align: %u\n", phdr.p_align);
            }
        }
    }

    Elf64_Ehdr ehdr64;
    if (h_parse(parse_elf64(&ehdr64), buf, len)) {
        printf("ELF64 file\n");
        printf("  e_type: %u\n", ehdr64.e_type);
        printf("  e_machine: %u\n", ehdr64.e_machine);
        printf("  e_version: %u\n", ehdr64.e_version);
        printf("  e_entry: %llu\n", ehdr64.e_entry);
        printf("  e_phoff: %llu\n", ehdr64.e_phoff);
        printf("  e_shoff: %llu\n", ehdr64.e_shoff);
        printf("  e_flags: %u\n", ehdr64.e_flags);
        printf("  e_ehsize: %u\n", ehdr64.e_ehsize);
        printf("  e_phentsize: %u\n", ehdr64.e_phentsize);
        printf("  e_phnum: %u\n", ehdr64.e_phnum);
        printf("  e_shentsize: %u\n", ehdr64.e_shentsize);
        printf("  e_shnum: %u\n", ehdr64.e_shnum);
        printf("  e_shstrndx: %u\n", ehdr64.e_shstrndx);

        for (size_t i = 0; i < ehdr64.e_phnum; i++) {
            Elf64_Phdr phdr;
            size_t offset = ehdr64.e_phoff + i * ehdr64.e_phentsize;
            if (h_parse(parse_program_header64(&phdr), buf + offset, ehdr64.e_phentsize)) {
                printf("  Program header %zu:\n", i);
                printf("    p_type: %u\n", phdr.p_type);
                printf("    p_flags: %u\n", phdr.p_flags);
                printf("    p_offset: %llu\n", phdr.p_offset);
                printf("    p_vaddr: %llu\n", phdr.p_vaddr);
                printf("    p_paddr: %llu\n", phdr.p_paddr);
                printf("    p_filesz: %llu\n", phdr.p_filesz);
                printf("    p_memsz: %llu\n", phdr.p_memsz);
                printf("    p_align: %llu\n", phdr.p_align);
            }
        }
    }

    free(buf);
    return 0;
}