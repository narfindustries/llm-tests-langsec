#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t magic[4];
    uint8_t class;
    uint8_t data_encoding;
    uint8_t version;
    uint8_t os_abi;
    uint8_t abi_version;
    uint8_t padding[7];
} ElfIdentStruct;

typedef struct {
    ElfIdentStruct ident;
    uint16_t type;
    uint16_t machine;
    uint32_t elf_version;
    uint64_t entry_point;
    uint64_t program_header_offset;
    uint64_t section_header_offset;
    uint32_t flags;
    uint16_t header_size;
    uint16_t program_header_entry_size;
    uint16_t program_header_num;
    uint16_t section_header_entry_size;
    uint16_t section_header_num;
    uint16_t section_header_string_index;
} ElfHeader;

HParser* parse_elf_ident(void) {
    return h_sequence(
        h_literal("\x7F""ELF"),
        h_int_range(h_end_p(), 0, 2),  // Class
        h_int_range(h_end_p(), 0, 2),  // Data Encoding
        h_int_range(h_end_p(), 0, 1),  // Version
        h_int_range(h_end_p(), 0, 255),  // OS ABI
        h_int_range(h_end_p(), 0, 255),  // ABI Version
        h_repeat_n(h_int_range(h_end_p(), 0, 255), 7),  // Padding
        NULL
    );
}

HParser* parse_elf_header(void) {
    return h_sequence(
        parse_elf_ident(),
        h_int_range(h_end_p(), 0, 4),  // Type
        h_int_range(h_end_p(), 0, 65535),  // Machine
        h_int_range(h_end_p(), 0, UINT32_MAX),  // ELF Version
        h_int_range(h_end_p(), 0, UINT64_MAX),  // Entry Point
        h_int_range(h_end_p(), 0, UINT64_MAX),  // Program Header Offset
        h_int_range(h_end_p(), 0, UINT64_MAX),  // Section Header Offset
        h_int_range(h_end_p(), 0, UINT32_MAX),  // Flags
        h_int_range(h_end_p(), 0, 65535),  // Header Size
        h_int_range(h_end_p(), 0, 65535),  // Program Header Entry Size
        h_int_range(h_end_p(), 0, 65535),  // Program Header Number
        h_int_range(h_end_p(), 0, 65535),  // Section Header Entry Size
        h_int_range(h_end_p(), 0, 65535),  // Section Header Number
        h_int_range(h_end_p(), 0, 65535),  // Section Header String Index
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <elf_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = parse_elf_header();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("ELF file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("ELF parsing failed\n");
    }

    free(buffer);
    h_parser_free(parser);
    return 0;
}