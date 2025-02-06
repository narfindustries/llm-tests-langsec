#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Helper function to parse a SQLite file format
HParser* sqlite_header() {
    return h_sequence(
        h_token("SQLite format 3\000", 16),
        h_uint16(),               // Page size
        h_uint8(),                // Write version
        h_uint8(),                // Read version
        h_uint8(),                // Max embedded payload fraction (must be 64)
        h_uint8(),                // Min embedded payload fraction (must be 32)
        h_uint8(),                // Leaf payload fraction (must be 32)
        h_uint32(),               // File change counter
        h_uint32(),               // Number of pages in the database file
        h_uint32(),               // First freelist trunk page
        h_uint32(),               // Total number of freelist pages
        h_uint32(),               // Schema cookie
        h_uint32(),               // Schema format number
        h_uint32(),               // Default page cache size
        h_uint32(),               // Largest B-tree page
        h_uint32(),               // Text encoding
        h_uint32(),               // User version
        h_uint32(),               // Incremental vacuum mode
        h_uint32(),               // Application ID
        h_uint32(),               // Version valid for number
        h_uint32(),               // SQLite version number
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite3_db_file>\n", argv[0]);
        exit(1);
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(buffer, fileSize, 1, fp);
    fclose(fp);

    HParseResult* result = h_parse(sqlite_header(), buffer, fileSize);
    if (result == NULL) {
        printf("Parsing failed.\n");
        free(buffer);
        return EXIT_FAILURE;
    }

    printf("Parsing successful. Header information is parsed.\n");
    
    // You would need additional handling here to process and interpret the parsed data, and walk through B-tree pages, etc.

    free(buffer);
    return EXIT_SUCCESS;
}