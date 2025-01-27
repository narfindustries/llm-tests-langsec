#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <sqlite3.h>
#include <stdio.h>
#include <stdlib.h>

// Define the SQLite3 database parser
HParser *sqlite3_db_parser() {
    return h_sequence(
        h_magic("\x53\x51\x4C\x69\x74\x65\x20\x66\x6F\x72\x6D\x61\x74\x20\x33\x00", 16), // SQLite format 3 header
        h_uint16(), // Page size
        h_uint8(),  // File format write version
        h_uint8(),  // File format read version
        h_uint8(),  // Reserved space at the end of each page
        h_uint8(),  // Maximum embedded payload fraction
        h_uint8(),  // Minimum embedded payload fraction
        h_uint8(),  // Leaf payload fraction
        h_uint32(), // File change counter
        h_uint32(), // Size of the database in pages
        h_uint32(), // Page number of the first freelist trunk page
        h_uint32(), // Total number of freelist pages
        h_uint32(), // Schema cookie
        h_uint32(), // Schema format number
        h_uint32(), // Default page cache size
        h_uint32(), // Largest root b-tree page number
        h_uint32(), // Text encoding
        h_uint32(), // User version
        h_uint32(), // Incremental vacuum mode
        h_uint32(), // Application ID
        h_bytes(20), // Reserved for expansion
        h_uint32(), // Version valid for
        h_uint32(), // SQLite version number
        NULL
    );
}

int main(int argc, char **argv) {
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

    HParser *parser = sqlite3_db_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("SQLite3 database parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse SQLite3 database.\n");
    }

    free(buffer);
    return 0;
}