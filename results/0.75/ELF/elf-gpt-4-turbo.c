#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function declaration for parsing ELF
HParser *parse_elf();

// Main function
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <ELF file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t file_size = ftell(fp);
    rewind(fp);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(fp);
        return EXIT_FAILURE;
    }

    if (fread(buffer, 1, file_size, fp) != file_size) {
        perror("File read failed");
        free(buffer);
        fclose(fp);
        return EXIT_FAILURE;
    }

    fclose(fp);

    HParser *elf_parser = parse_elf();
    HParseResult *result = h_parse(elf_parser, buffer, file_size);
    if (result) {
        printf("ELF parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        fprintf(stderr, "Failed to parse ELF.\n");
    }

    free(buffer);
    h_parse_result_free(result);
    h_parser_unref(elf_parser);
    
    return 0;
}

// Function to create an ELF parser
HParser *parse_elf() {
    HParser *magic = h_token("\x7F" "ELF", 4);
    HParser *ei_class = h_uint8();
    HParser *ei_data = h_uint8();
    HParser *ei_version = h_uint8();
    HParser *ei_osabi = h_uint8();
    HParser *ei_abiversion = h_uint8();
    HParser *ei_pad = h_sequence(h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), NULL);
    
    HParser *e_type = h_uint16();
    HParser *e_machine = h_uint16();
    HParser *e_version = h_uint32();
    HParser *e_entry = h_uint64();  // This will depend on EI_CLASS
    HParser *e_phoff = h_uint64();  // This will depend on EI_CLASS
    HParser *e_shoff = h_uint64();  // This will depend on EI_CLASS
    HParser *e_flags = h_uint32();
    HParser *e_ehsize = h_uint16();
    HParser *e_phentsize = h_uint16();
    HParser *e_phnum = h_uint16();
    HParser *e_shentsize = h_uint16();
    HParser *e_shnum = h_uint16();
    HParser *e_shstrndx = h_uint16();

    HParser *elf_header = h_sequence(magic, ei_class, ei_data, ei_version, ei_osabi, ei_abiversion, ei_pad, e_type, e_machine, e_version, e_entry, e_phoff, e_shoff, e_flags, e_ehsize, e_phentsize, e_phnum, e_shentsize, e_shnum, e_shstrndx, NULL);

    return elf_header;
}