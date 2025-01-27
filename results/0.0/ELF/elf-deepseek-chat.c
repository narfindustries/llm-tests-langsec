#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define ELF header structure
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
} ElfHeader;

// Define ELF program header structure
typedef struct {
    uint32_t p_type;
    uint32_t p_flags;
    uint64_t p_offset;
    uint64_t p_vaddr;
    uint64_t p_paddr;
    uint64_t p_filesz;
    uint64_t p_memsz;
    uint64_t p_align;
} ElfProgramHeader;

// Define ELF section header structure
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
} ElfSectionHeader;

// Parser for ELF header
HParser *elf_header_parser() {
    return h_sequence(
        h_many(h_uint8()), // e_ident
        h_uint16(),        // e_type
        h_uint16(),        // e_machine
        h_uint32(),        // e_version
        h_uint64(),        // e_entry
        h_uint64(),        // e_phoff
        h_uint64(),        // e_shoff
        h_uint32(),        // e_flags
        h_uint16(),        // e_ehsize
        h_uint16(),        // e_phentsize
        h_uint16(),        // e_phnum
        h_uint16(),        // e_shentsize
        h_uint16(),        // e_shnum
        h_uint16(),        // e_shstrndx
        NULL
    );
}

// Parser for ELF program header
HParser *elf_program_header_parser() {
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

// Parser for ELF section header
HParser *elf_section_header_parser() {
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
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
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

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *elf_parser = elf_header_parser();
    HParseResult *result = h_parse(elf_parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ELF header\n");
        free(buffer);
        return 1;
    }

    ElfHeader *header = (ElfHeader *)result->ast;
    printf("ELF Header parsed successfully.\n");

    // Parse program headers
    for (int i = 0; i < header->e_phnum; i++) {
        fseek(file, header->e_phoff + i * header->e_phentsize, SEEK_SET);
        fread(buffer, 1, header->e_phentsize, file);
        HParser *ph_parser = elf_program_header_parser();
        HParseResult *ph_result = h_parse(ph_parser, buffer, header->e_phentsize);
        if (!ph_result) {
            fprintf(stderr, "Failed to parse program header %d\n", i);
            free(buffer);
            return 1;
        }
        ElfProgramHeader *ph = (ElfProgramHeader *)ph_result->ast;
        printf("Program Header %d parsed successfully.\n", i);
    }

    // Parse section headers
    for (int i = 0; i < header->e_shnum; i++) {
        fseek(file, header->e_shoff + i * header->e_shentsize, SEEK_SET);
        fread(buffer, 1, header->e_shentsize, file);
        HParser *sh_parser = elf_section_header_parser();
        HParseResult *sh_result = h_parse(sh_parser, buffer, header->e_shentsize);
        if (!sh_result) {
            fprintf(stderr, "Failed to parse section header %d\n", i);
            free(buffer);
            return 1;
        }
        ElfSectionHeader *sh = (ElfSectionHeader *)sh_result->ast;
        printf("Section Header %d parsed successfully.\n", i);
    }

    free(buffer);
    return 0;
}