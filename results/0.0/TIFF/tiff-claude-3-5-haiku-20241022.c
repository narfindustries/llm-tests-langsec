#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint16_t byte_order;
    uint16_t version;
    uint32_t ifd_offset;
    // Additional IFD entries and data structures
} TIFFHeader;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_or_offset;
} TIFFDirEntry;

// Hammer parser definitions
static HParser *tiff_header_parser;
static HParser *tiff_dir_entry_parser;

static HParser* create_tiff_header_parser() {
    return h_sequence(
        h_choice(
            h_literal("\x49\x49"), // Little-endian
            h_literal("\x4D\x4D"), // Big-endian
            NULL
        ),
        h_uint16(),
        h_uint32(),
        NULL
    );
}

static HParser* create_tiff_dir_entry_parser() {
    return h_sequence(
        h_uint16(), // tag
        h_uint16(), // type
        h_uint32(), // count
        h_uint32(), // value or offset
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    // Initialize Hammer parsers
    tiff_header_parser = create_tiff_header_parser();
    tiff_dir_entry_parser = create_tiff_dir_entry_parser();

    // Read file contents
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(file_size);
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

    // Parse TIFF header
    HParseResult *header_result = h_parse(tiff_header_parser, buffer, file_size);
    if (!header_result) {
        fprintf(stderr, "TIFF header parsing failed\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    // Optional: Parse directory entries
    // Add more parsing logic as needed

    // Cleanup
    h_parse_result_free(header_result);
    free(buffer);
    fclose(file);

    return 0;
}