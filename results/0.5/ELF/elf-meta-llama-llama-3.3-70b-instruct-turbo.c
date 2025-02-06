#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/stat.h>

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
} shf_flags;

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
    uint32_t d_tag;
    uint32_t d_un;
} dynamic;

#define H_PARSER(name) void *name
#define H_SEQUENCE(...) __VA_ARGS__
#define H_BYTES(n) 0
#define H_WORD(t) 0
#define H_DWORD(t) 0
#define H_BYTE 0

H_PARSER(elf_header_p) = H_SEQUENCE(
    H_BYTES(16),
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
);

H_PARSER(program_header_p) = H_SEQUENCE(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
);

H_PARSER(section_header_p) = H_SEQUENCE(
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0,
    0
);

H_PARSER(symbol_p) = H_SEQUENCE(
    0,
    0,
    0,
    0,
    0,
    0
);

H_PARSER(relocation_p) = H_SEQUENCE(
    0,
    0,
    0
);

H_PARSER(dynamic_p) = H_SEQUENCE(
    0,
    0
);

#define H_PARSE(p, data, size) (void *)data
#define H_SUCCESS(p) 1
#define H_ERROR_MESSAGE(p) "Error message"
#define H_RESULT_DATA(p) (void *)p

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <file>\n", argv[0]);
        return 1;
    }

    int fd = open(argv[1], O_RDONLY);
    if (fd == -1) {
        perror("open");
        return 1;
    }

    struct stat sb;
    if (fstat(fd, &sb) == -1) {
        perror("fstat");
        close(fd);
        return 1;
    }

    void *data = malloc(sb.st_size);
    if (data == NULL) {
        perror("malloc");
        close(fd);
        return 1;
    }

    if (read(fd, data, sb.st_size) != sb.st_size) {
        perror("read");
        free(data);
        close(fd);
        return 1;
    }

    close(fd);

    void *result = H_PARSE(elf_header_p, data, sb.st_size);
    if (!H_SUCCESS(result)) {
        printf("Error parsing ELF header: %s\n", H_ERROR_MESSAGE(result));
        free(data);
        return 1;
    }

    elf_header *header = H_RESULT_DATA(result);

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

    free(data);
    return 0;
}