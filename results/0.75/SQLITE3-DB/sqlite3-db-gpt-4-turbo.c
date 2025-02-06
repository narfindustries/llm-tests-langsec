#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Parsing the fixed-size database header
HParser *parse_header() {
    return h_sequence(
        h_token("SQLite format 3\000", 16),
        h_uint16(), // Page Size
        h_uint8(),  // File Format Write Version
        h_uint8(),  // File Format Read Version
        h_uint8(),  // Reserved Space
        h_uint8(),  // Maximum Embedded Payload Fraction
        h_uint8(),  // Minimum Embedded Payload Fraction
        h_uint8(),  // Leaf Payload Fraction
        h_uint32(), // File Change Counter
        h_uint32(), // Database Size in Pages
        h_uint32(), // First Freelist Page
        h_uint32(), // Number of Freelist Pages
        h_uint32(), // Schema Cookie
        h_uint32(), // Schema Format Number
        h_uint32(), // Default Page Cache Size
        h_uint32(), // Largest Root B-tree Page
        h_uint32(), // Database Text Encoding
        h_uint32(), // User Version
        h_uint32(), // Incremental Vacuum Mode
        h_uint32(), // Application ID
        h_ignore(h_bits(160, false)), // Reserved for expansion
        h_uint32(), // Version-valid-for Number
        h_uint32(), // SQLite Version Number
        NULL);
}

// Main function to parse SQLite database file
int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite_db_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    // Compute the file size
    fseek(file, 0, SEEK_END);
    size_t length = ftell(file);
    rewind(file);

    // Read the entire file into memory
    uint8_t *data = malloc(length);
    if (!data) {
        fclose(file);
        fprintf(stderr, "Failed to allocate memory\n");
        return 1;
    }

    if (fread(data, 1, length, file) != length) {
        fclose(file);
        free(data);
        fprintf(stderr, "Failed to read file\n");
        return 1;
    }

    fclose(file);

    // Combine parsers
    HParser *db_header_parser = parse_header();

    // Parse the database
    HParseResult *result = h_parse(db_header_parser, data, length);
    if (result) {
        printf("SQLite Database Parsed Successfully\n");
    } else {
        fprintf(stderr, "Failed to parse SQLite Database\n");
    }

    // Cleanup
    h_parser_free(db_header_parser);
    free(data);

    return result ? 0 : 1;
}