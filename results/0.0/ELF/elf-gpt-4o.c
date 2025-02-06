#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_elf_parser() {
    // ELF Identification
    HParser *e_ident = h_sequence(
        h_ch('\x7F'), h_ch('E'), h_ch('L'), h_ch('F'),
        h_uint8(), // EI_CLASS
        h_uint8(), // EI_DATA
        h_uint8(), // EI_VERSION
        h_uint8(), // EI_OSABI
        h_uint8(), // EI_ABIVERSION
        h_repeat_n(h_uint8(), 7), // EI_PAD
        NULL
    );

    // ELF Header
    HParser *elf_header = h_sequence(
        e_ident,
        h_uint16(), // e_type
        h_uint16(), // e_machine
        h_uint32(), // e_version
        h_uint32(), // e_entry
        h_uint32(), // e_phoff
        h_uint32(), // e_shoff
        h_uint32(), // e_flags
        h_uint16(), // e_ehsize
        h_uint16(), // e_phentsize
        h_uint16(), // e_phnum
        h_uint16(), // e_shentsize
        h_uint16(), // e_shnum
        h_uint16(), // e_shstrndx
        NULL
    );

    // Program Header
    HParser *program_header = h_sequence(
        h_uint32(), // p_type
        h_uint32(), // p_offset
        h_uint32(), // p_vaddr
        h_uint32(), // p_paddr
        h_uint32(), // p_filesz
        h_uint32(), // p_memsz
        h_uint32(), // p_flags
        h_uint32(), // p_align
        NULL
    );

    // Section Header
    HParser *section_header = h_sequence(
        h_uint32(), // sh_name
        h_uint32(), // sh_type
        h_uint32(), // sh_flags
        h_uint32(), // sh_addr
        h_uint32(), // sh_offset
        h_uint32(), // sh_size
        h_uint32(), // sh_link
        h_uint32(), // sh_info
        h_uint32(), // sh_addralign
        h_uint32(), // sh_entsize
        NULL
    );

    // ELF File
    HParser *elf_file = h_sequence(
        elf_header,
        h_repeat_n(program_header, 1), // Placeholder for e_phnum
        h_repeat_n(section_header, 1), // Placeholder for e_shnum
        NULL
    );

    return elf_file;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *elf_parser = create_elf_parser();
    HParseResult *result = h_parse(elf_parser, data, file_size);

    if (result) {
        printf("ELF file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ELF file.\n");
    }

    free(data);
    h_parser_free(elf_parser);

    return EXIT_SUCCESS;
}