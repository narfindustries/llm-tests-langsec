#include <hammer/hammer.h>
#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>

// Define the Hammer parser for SQLite3 database header
HParser *sqlite3_header_parser() {
    return h_sequence(
        h_bytes(16, "SQLite format 3\0"), // Magic string
        h_int16(),                        // Page size
        h_int8(),                         // File format write version
        h_int8(),                         // File format read version
        h_int8(),                         // Reserved space at the end of each page
        h_int8(),                         // Maximum embedded payload fraction
        h_int8(),                         // Minimum embedded payload fraction
        h_int8(),                         // Leaf payload fraction
        h_int32(),                        // File change counter
        h_int32(),                        // Size of the database in pages
        h_int32(),                        // Page number of the first freelist trunk page
        h_int32(),                        // Total number of freelist pages
        h_int32(),                        // Schema cookie
        h_int32(),                        // Schema format number
        h_int32(),                        // Default page cache size
        h_int32(),                        // Largest auto-vacuum root page
        h_int32(),                        // Text encoding
        h_int32(),                        // User version
        h_int32(),                        // Incremental vacuum mode
        h_int32(),                        // Application ID
        h_bytes(20, NULL),                // Reserved for expansion
        h_int32(),                        // Version valid for
        h_int32(),                        // SQLite version number
        NULL
    );
}

// Main function to parse SQLite3 database header
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <sqlite3_db_file>\n", argv[0]);
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

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = sqlite3_header_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("SQLite3 database header parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse SQLite3 database header.\n");
    }

    free(buffer);
    return 0;
}