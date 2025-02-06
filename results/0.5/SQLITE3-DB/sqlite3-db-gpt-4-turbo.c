#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations for parsing functions
static HParser *parse_header();
static HParser *parse_btree_page();
static HParser *parse_cell();
static HParser *parse_overflow_page();

// Main parsing function for SQLite Database File
static HParser *parse_sqlite_db() {
    return h_sequence(
        parse_header(),
        h_many(parse_btree_page()),
        NULL
    );
}

// Parse the database header
static HParser *parse_header() {
    return h_sequence(
        h_token("SQLite format 3\0", 16),
        h_uint16(), // Page size
        h_uint8(),  // File format write version
        h_uint8(),  // File format read version
        h_uint8(),  // Reserved space
        h_uint8(),  // Max embedded payload fraction
        h_uint8(),  // Min embedded payload fraction
        h_uint8(),  // Leaf payload fraction
        h_uint32(), // File change counter
        h_uint32(), // Database size in pages
        h_uint32(), // First freelist page
        h_uint32(), // Number of freelist pages
        h_uint32(), // Schema cookie
        h_uint32(), // Schema format number
        h_int32(),  // Default page cache size
        h_uint32(), // Largest root B-tree page number
        h_uint32(), // Database text encoding
        h_uint32(), // User version
        h_uint32(), // Incremental vacuum mode
        h_uint32(), // Application ID
        h_bits(160, false), // Reserved for expansion (20 bytes)
        h_uint32(), // Version-valid-for number
        h_uint32(), // SQLite version number
        NULL
    );
}

// Parse B-tree page
static HParser *parse_btree_page() {
    return h_sequence(
        h_uint8(),  // Page type
        h_uint16(), // First freeblock
        h_uint16(), // Number of cells
        h_uint16(), // Start of cell content area
        h_uint8(),  // Number of fragmented free bytes
        h_many1(parse_cell()), // Cells
        NULL
    );
}

// Parse a cell in a B-tree page
static HParser *parse_cell() {
    return h_choice(
        h_sequence(
            h_uint32(), // Left child pointer (for interior pages)
            h_uint64(), // Payload size
            h_uint64(), // Rowid (for table B-trees)
            h_bits(0, false), // Payload (variable size)
            h_uint32(), // Overflow page number
            NULL
        ),
        h_sequence(
            h_uint64(), // Payload size
            h_uint64(), // Rowid (for table B-trees)
            h_bits(0, false), // Payload (variable size)
            h_uint32(), // Overflow page number
            NULL
        ),
        NULL
    );
}

// Parse overflow page
static HParser *parse_overflow_page() {
    return h_sequence(
        h_uint32(), // Next overflow page
        h_bits(0, false), // Data (variable size)
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <SQLite db file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    rewind(file);

    uint8_t *buffer = malloc(size);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, size, file) != size) {
        fprintf(stderr, "Failed to read file\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    fclose(file);

    HParser *parser = parse_sqlite_db();
    HParseResult *result = h_parse(parser, buffer, size);
    if (result) {
        printf("Parse successful.\n");
    } else {
        printf("Parse failed.\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    free(buffer);

    return 0;
}