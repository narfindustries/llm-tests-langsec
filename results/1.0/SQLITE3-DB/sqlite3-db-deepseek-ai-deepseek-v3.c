#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define SQLITE_MAGIC_STRING "SQLite format 3\000"

HParser *sqlite_header_parser() {
    return h_sequence(
        h_bits(128, 0), // Magic Header String
        h_uint16(), // Page Size
        h_uint8(),  // Write Version
        h_uint8(),  // Read Version
        h_uint8(),  // Reserved Space
        h_uint8(),  // Maximum Embedded Payload Fraction
        h_uint8(),  // Minimum Embedded Payload Fraction
        h_uint8(),  // Leaf Payload Fraction
        h_uint32(), // File Change Counter
        h_uint32(), // Size of the Database in Pages
        h_uint32(), // Page Number of the First Freelist Trunk Page
        h_uint32(), // Number of Freelist Pages
        h_uint32(), // Schema Cookie
        h_uint32(), // Schema Format Number
        h_uint32(), // Default Page Cache Size
        h_uint32(), // Page Number of the Largest Root B-Tree
        h_uint32(), // Database Text Encoding
        h_int32(),  // User Version
        h_uint32(), // Incremental Vacuum Flag
        h_uint32(), // Application ID
        h_bits(160, 0) // Reserved for Expansion
    );
}

HParser *btree_page_parser() {
    return h_sequence(
        h_uint8(),  // Page Type
        h_uint16(), // First Free Block
        h_uint16(), // Number of Cells
        h_uint16(), // Start of Cell Content Area
        h_uint8(),  // Fragmented Free Bytes
        h_optional(h_uint32()) // Rightmost Pointer (optional, interior pages only)
    );
}

HParser *varint_parser() {
    return h_sequence(
        h_uint8(),
        h_optional(h_uint8()),
        h_optional(h_uint8()),
        h_optional(h_uint8()),
        h_optional(h_uint8()),
        h_optional(h_uint8()),
        h_optional(h_uint8()),
        h_optional(h_uint8()),
        h_optional(h_uint8())
    );
}

HParser *cell_content_parser() {
    return h_sequence(
        varint_parser(), // Payload Size
        h_optional(varint_parser()), // Row ID (optional, table b-tree cells only)
        h_uint8() // Payload (first byte for simplicity)
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
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

    HParser *parser = sqlite_header_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Successfully parsed SQLite header.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse SQLite header.\n");
    }

    free(buffer);
    return 0;
}