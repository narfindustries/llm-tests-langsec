#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for ELF Header fields
HParser *create_elf_header_parser() {
    return h_sequence(
        h_sequence(h_ch('\x7F'), h_ch('E'), h_ch('L'), h_ch('F'), NULL),
        h_uint8(),   // EI_CLASS
        h_uint8(),   // EI_DATA
        h_uint8(),   // EI_VERSION
        h_uint8(),   // EI_OSABI
        h_uint8(),   // EI_ABIVERSION
        h_repeat_n(h_uint8(), 7), // Padding bytes
        h_uint16(),   // e_type
        h_uint16(),   // e_machine
        h_uint32(),   // e_version
        h_uint32(),   // e_entry
        h_uint32(),   // e_phoff
        h_uint32(),   // e_shoff
        h_uint32(),   // e_flags
        h_uint16(),   // e_ehsize
        h_uint16(),   // e_phentsize
        h_uint16(),   // e_phnum
        h_uint16(),   // e_shentsize
        h_uint16(),   // e_shnum
        h_uint16(),   // e_shstrndx
        NULL
    );
}

// Define parsers for Program Header Table
HParser *create_program_header_parser() {
    return h_sequence(
        h_uint32(),   // p_type
        h_uint32(),   // p_offset
        h_uint32(),   // p_vaddr
        h_uint32(),   // p_paddr
        h_uint32(),   // p_filesz
        h_uint32(),   // p_memsz
        h_uint32(),   // p_flags
        h_uint32(),   // p_align
        NULL
    );
}

// Define parsers for Section Header Table
HParser *create_section_header_parser() {
    return h_sequence(
        h_uint32(),   // sh_name
        h_uint32(),   // sh_type
        h_uint32(),   // sh_flags
        h_uint32(),   // sh_addr
        h_uint32(),   // sh_offset
        h_uint32(),   // sh_size
        h_uint32(),   // sh_link
        h_uint32(),   // sh_info
        h_uint32(),   // sh_addralign
        h_uint32(),   // sh_entsize
        NULL
    );
}

// Function to parse ELF file
void parse_elf(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = (unsigned char *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *elf_header_parser = create_elf_header_parser();
    HParseResult *result = h_parse(elf_header_parser, buffer, file_size);
    if (result) {
        printf("ELF Header parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ELF Header.\n");
    }

    free(buffer);
    h_parser_free(elf_header_parser);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_elf(argv[1]);

    return EXIT_SUCCESS;
}