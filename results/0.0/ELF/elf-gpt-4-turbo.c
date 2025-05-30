#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define constants based on ELF specification
#define EI_NIDENT 16

// Parser for ELF Header
static HParser *elf_header() {
    return h_sequence(
        h_bits(32, false), // EI_MAG
        h_uint8(),  // EI_CLASS
        h_uint8(),  // EI_DATA
        h_uint8(),  // EI_VERSION
        h_uint8(),  // EI_OSABI
        h_uint8(),  // EI_ABIVERSION
        h_repeat_n(h_uint8(), 7), // EI_PAD
        h_uint16(), // e_type
        h_uint16(), // e_machine
        h_uint32(), // e_version
        h_uint64(), // e_entry
        h_uint64(), // e_phoff
        h_uint64(), // e_shoff
        h_uint32(), // e_flags
        h_uint16(), // e_ehsize
        h_uint16(), // e_phentsize
        h_uint16(), // e_phnum
        h_uint16(), // e_shentsize
        h_uint16(), // e_shnum
        h_uint16(), // e_shstrndx
        NULL
    );
}

// Parser for Section Header
static HParser *section_header() {
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

// Parser for Program Header
static HParser *program_header() {
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

// Main function to parse ELF file
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ELF file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    // Seek to the end of the file to determine the size
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    // Read the entire file into memory
    uint8_t *buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    fclose(fp);

    // Create a parser for an ELF file
    HParser *elf_parser = h_sequence(
        elf_header(),
        h_many(section_header()),
        h_many(program_header()),
        NULL
    );

    HParseResult *result = h_parse(elf_parser, buffer, size);
    if (result) {
        printf("ELF file parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        fprintf(stderr, "Failed to parse ELF file.\n");
    }

    // Clean up
    h_parse_result_free(result);
    h_free_parser(elf_parser);
    free(buffer);

    return 0;
}