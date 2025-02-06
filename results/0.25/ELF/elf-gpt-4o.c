#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_elf_parser() {
    // ELF Header
    HParser *e_ident = h_repeat_n(h_uint8(), 16);
    HParser *e_type = h_uint16();
    HParser *e_machine = h_uint16();
    HParser *e_version = h_uint32();
    HParser *e_entry = h_uint32();
    HParser *e_phoff = h_uint32();
    HParser *e_shoff = h_uint32();
    HParser *e_flags = h_uint32();
    HParser *e_ehsize = h_uint16();
    HParser *e_phentsize = h_uint16();
    HParser *e_phnum = h_uint16();
    HParser *e_shentsize = h_uint16();
    HParser *e_shnum = h_uint16();
    HParser *e_shstrndx = h_uint16();

    HParser *elf_header = h_sequence(
        e_ident, e_type, e_machine, e_version, e_entry,
        e_phoff, e_shoff, e_flags, e_ehsize, e_phentsize,
        e_phnum, e_shentsize, e_shnum, e_shstrndx, NULL
    );

    // Program Header
    HParser *p_type = h_uint32();
    HParser *p_offset = h_uint32();
    HParser *p_vaddr = h_uint32();
    HParser *p_paddr = h_uint32();
    HParser *p_filesz = h_uint32();
    HParser *p_memsz = h_uint32();
    HParser *p_flags = h_uint32();
    HParser *p_align = h_uint32();

    HParser *program_header = h_sequence(
        p_type, p_offset, p_vaddr, p_paddr, p_filesz,
        p_memsz, p_flags, p_align, NULL
    );

    // Section Header
    HParser *sh_name = h_uint32();
    HParser *sh_type = h_uint32();
    HParser *sh_flags = h_uint32();
    HParser *sh_addr = h_uint32();
    HParser *sh_offset = h_uint32();
    HParser *sh_size = h_uint32();
    HParser *sh_link = h_uint32();
    HParser *sh_info = h_uint32();
    HParser *sh_addralign = h_uint32();
    HParser *sh_entsize = h_uint32();

    HParser *section_header = h_sequence(
        sh_name, sh_type, sh_flags, sh_addr, sh_offset,
        sh_size, sh_link, sh_info, sh_addralign, sh_entsize, NULL
    );

    // Complete ELF Parser
    HParser *program_headers = h_repeat_n(program_header, 1); // Adjust count as needed
    HParser *section_headers = h_repeat_n(section_header, 1); // Adjust count as needed

    HParser *elf_parser = h_sequence(
        elf_header,
        program_headers,
        section_headers,
        NULL
    );

    return elf_parser;
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
        printf("Failed to parse ELF file.\n");
    }

    h_parser_unref(elf_parser);
    free(data);

    return EXIT_SUCCESS;
}