#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

#define SQLITE_HEADER_STRING "SQLite format 3\0"

HParser *sqlite_header_parser() {
    return h_sequence(
        h_length_value(h_uint16(), h_bits(16 * 8, false)), // Header String
        h_uint16(), // Page Size
        h_uint8(),  // Write Version
        h_uint8(),  // Read Version
        h_uint8(),  // Reserved Space
        h_uint8(),  // Maximum Embedded Payload Fraction
        h_uint8(),  // Minimum Embedded Payload Fraction
        h_uint8(),  // Leaf Payload Fraction
        h_uint32(), // File Change Counter
        h_uint32(), // Size of the Database File in Pages
        h_uint32(), // Page Number of the First Freelist Trunk Page
        h_uint32(), // Number of Freelist Pages
        h_uint32(), // Schema Cookie
        h_uint32(), // Schema Format Number
        h_uint32(), // Default Page Cache Size
        h_uint32(), // Page Number of the Largest Root B-Tree Page
        h_uint32(), // Text Encoding
        h_uint32(), // User Version
        h_uint32(), // Incremental Vacuum Mode
        h_uint32(), // Application ID
        h_length_value(h_uint16(), h_bits(20 * 8, false)), // Reserved for Expansion
        h_uint32(), // Version Valid For
        h_uint32(), // SQLite Version Number
        NULL
    );
}

HParser *page_type_parser() {
    return h_choice(
        h_token(NULL, 1), // B-Tree Leaf Page (0x0D)
        h_token(NULL, 1), // B-Tree Interior Page (0x05)
        h_token(NULL, 1), // Free Page (0x00)
        h_token(NULL, 1), // Overflow Page (0x02)
        h_token(NULL, 1), // Pointer Map Page (0x05)
        h_token(NULL, 1), // Lock Byte Page (0x06)
        NULL
    );
}

HParser *b_tree_page_parser() {
    return h_sequence(
        page_type_parser(),
        h_uint16(), // First Free Block Offset
        h_uint16(), // Number of Cells
        h_uint16(), // Cell Content Offset
        h_uint8(),  // Fragmented Free Bytes
        h_optional(h_uint32()), // Rightmost Pointer (interior pages only)
        NULL
    );
}

HParser *record_format_parser() {
    return h_sequence(
        h_length_value(h_uint16(), h_bits(0, false)), // Header Size
        h_length_value(h_uint16(), h_bits(0, false)), // Type Codes
        NULL
    );
}

HParser *sqlite_parser() {
    return h_sequence(
        sqlite_header_parser(),
        h_many(b_tree_page_parser()),
        h_many(record_format_parser()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
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

    HParseResult *result = h_parse(sqlite_parser(), buffer, file_size);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return 1;
    }

    printf("Parsing succeeded\n");
    free(buffer);
    h_parse_result_free(result);
    return 0;
}