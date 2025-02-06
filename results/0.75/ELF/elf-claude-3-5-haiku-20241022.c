#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t magic[4];
    uint8_t class;
    uint8_t data_encoding;
    uint8_t version;
    uint8_t os_abi;
    uint8_t abi_version;
    uint8_t padding[7];
    uint16_t type;
    uint16_t machine;
    uint32_t elf_version;
    uint64_t entry_point;
    uint64_t program_header_offset;
    uint64_t section_header_offset;
    uint32_t flags;
    uint16_t header_size;
    uint16_t program_header_entry_size;
    uint16_t program_header_entry_count;
    uint16_t section_header_entry_size;
    uint16_t section_header_entry_count;
    uint16_t section_header_string_table_index;
} ElfHeader;

HParser* parse_elf_header(void) {
    return h_sequence(
        h_token("\x7F""ELF", 4),
        h_choice(
            h_int_range(0, 2, sizeof(uint8_t)),
            h_epsilon()
        ),
        h_choice(
            h_int_range(0, 2, sizeof(uint8_t)),
            h_epsilon()
        ),
        h_choice(
            h_int_range(0, 1, sizeof(uint8_t)),
            h_epsilon()
        ),
        h_choice(
            h_int_range(0, 20, sizeof(uint8_t)),
            h_epsilon()
        ),
        h_int_range(0, 255, sizeof(uint8_t)),
        h_repeat_n(h_int_range(0, 255, sizeof(uint8_t)), 7),
        h_choice(
            h_int_range(0, 4, sizeof(uint16_t)),
            h_epsilon()
        ),
        h_choice(
            h_int_range(0, 255, sizeof(uint16_t)),
            h_epsilon()
        ),
        h_int_range(0, 1, sizeof(uint32_t)),
        h_uint64(),
        h_uint64(),
        h_uint64(),
        h_uint32(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16()
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
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = parse_elf_header();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("ELF file parsed successfully\n");
    } else {
        printf("ELF file parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}