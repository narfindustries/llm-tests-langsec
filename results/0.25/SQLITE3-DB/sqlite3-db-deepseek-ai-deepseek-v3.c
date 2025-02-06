#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

void parse_sqlite_file(const uint8_t *buffer, size_t size) {
    HParser *uint8_parser = h_uint8();
    HParser *uint16_parser = h_uint16();
    HParser *uint32_parser = h_uint32();
    HParser *int32_parser = h_int32();
    HParser *int64_parser = h_int64();

    HParser *magic_header_parser = h_sequence(h_token("SQLite format 3", 16), h_end_p(), NULL);

    HParser *database_header_parser = h_sequence(
        magic_header_parser,
        uint16_parser, // Page size
        uint8_parser,  // Write version
        uint8_parser,  // Read version
        uint8_parser,  // Reserved space
        uint8_parser,  // Maximum embedded payload fraction
        uint8_parser,  // Minimum embedded payload fraction
        uint8_parser,  // Leaf payload fraction
        uint32_parser, // File change counter
        uint32_parser, // Size of the database file in pages
        uint32_parser, // Page number of the first freelist trunk page
        uint32_parser, // Number of freelist pages
        uint32_parser, // Schema cookie
        uint32_parser, // Schema format number
        uint32_parser, // Default page cache size
        uint32_parser, // Page number of the largest root B-tree page
        uint32_parser, // Database text encoding
        int32_parser,  // User version
        uint32_parser, // Incremental vacuum mode
        uint32_parser, // Application ID
        h_repeat_n(h_uint8(), 20), // Reserved for expansion
        uint32_parser, // Version valid for number
        uint32_parser, // SQLite version number
        NULL
    );

    HParser *btree_page_parser = h_sequence(
        uint8_parser,  // Page type
        uint16_parser, // Number of cells
        uint16_parser, // Start of cell content area
        uint32_parser, // Rightmost child pointer (for interior pages)
        NULL
    );

    HParser *cell_parser = h_sequence(
        int64_parser,  // Row ID
        int64_parser, // Payload size
        h_repeat_n(h_uint8(), h_int64()), // Payload
        NULL
    );

    HParser *sqlite_file_parser = h_sequence(
        database_header_parser,
        h_repeat_n(btree_page_parser, h_int64()),
        NULL
    );

    HParseResult *result = h_parse(sqlite_file_parser, buffer, size);
    if (!result) {
        fprintf(stderr, "Failed to parse the SQLite file\n");
        return;
    }

    printf("Successfully parsed the SQLite file\n");
    h_parse_result_free(result);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
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

    parse_sqlite_file(buffer, file_size);

    free(buffer);
    return 0;
}