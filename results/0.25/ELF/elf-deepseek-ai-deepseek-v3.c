#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint8_t e_ident[16];
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
    uint32_t p_type;
    uint32_t p_flags;
    uint64_t p_offset;
    uint64_t p_vaddr;
    uint64_t p_paddr;
    uint64_t p_filesz;
    uint64_t p_memsz;
    uint64_t p_align;
} Elf64_Phdr;

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

HParser *elf_header_parser() {
    return h_sequence(
        h_repeat_n(h_uint8(), 16), // e_ident
        h_uint16(),                // e_type
        h_uint16(),                // e_machine
        h_uint32(),                // e_version
        h_uint64(),                // e_entry
        h_uint64(),                // e_phoff
        h_uint64(),                // e_shoff
        h_uint32(),                // e_flags
        h_uint16(),                // e_ehsize
        h_uint16(),                // e_phentsize
        h_uint16(),                // e_phnum
        h_uint16(),                // e_shentsize
        h_uint16(),                // e_shnum
        h_uint16(),                // e_shstrndx
        NULL
    );
}

HParser *program_header_parser() {
    return h_sequence(
        h_uint32(), // p_type
        h_uint32(), // p_flags
        h_uint64(), // p_offset
        h_uint64(), // p_vaddr
        h_uint64(), // p_paddr
        h_uint64(), // p_filesz
        h_uint64(), // p_memsz
        h_uint64(), // p_align
        NULL
    );
}

HParser *section_header_parser() {
    return h_sequence(
        h_uint32(), // sh_name
        h_uint32(), // sh_type
        h_uint64(), // sh_flags
        h_uint64(), // sh_addr
        h_uint64(), // sh_offset
        h_uint64(), // sh_size
        h_uint32(), // sh_link
        h_uint32(), // sh_info
        h_uint64(), // sh_addralign
        h_uint64(), // sh_entsize
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(elf_header_parser(), data, file_size);
    if (!result || !result->ast) {
        fprintf(stderr, "Failed to parse ELF header\n");
        free(data);
        return 1;
    }

    Elf64_Ehdr *header = (Elf64_Ehdr *)result->ast;

    printf("ELF Header:\n");
    printf("  Magic: %02x %02x %02x %02x\n", header->e_ident[0], header->e_ident[1], header->e_ident[2], header->e_ident[3]);
    printf("  Class: %d\n", header->e_ident[4]);
    printf("  Data: %d\n", header->e_ident[5]);
    printf("  Version: %d\n", header->e_ident[6]);
    printf("  OS/ABI: %d\n", header->e_ident[7]);
    printf("  ABI Version: %d\n", header->e_ident[8]);
    printf("  Type: %d\n", header->e_type);
    printf("  Machine: %d\n", header->e_machine);
    printf("  Version: %d\n", header->e_version);
    printf("  Entry point address: 0x%lx\n", header->e_entry);
    printf("  Start of program headers: %ld (bytes into file)\n", header->e_phoff);
    printf("  Start of section headers: %ld (bytes into file)\n", header->e_shoff);
    printf("  Flags: 0x%x\n", header->e_flags);
    printf("  Size of this header: %d (bytes)\n", header->e_ehsize);
    printf("  Size of program headers: %d (bytes)\n", header->e_phentsize);
    printf("  Number of program headers: %d\n", header->e_phnum);
    printf("  Size of section headers: %d (bytes)\n", header->e_shentsize);
    printf("  Number of section headers: %d\n", header->e_shnum);
    printf("  Section header string table index: %d\n", header->e_shstrndx);

    h_parse_result_free(result);
    free(data);
    return 0;
}