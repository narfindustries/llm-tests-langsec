#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// SQLite3 file format parser using Hammer

// Forward declarations
HParser* create_sqlite_parser(void);
HParser* create_header_parser(void);
HParser* create_record_parser(void);

// Helper parsers
static HParser* uint16_be() {
    return h_uint16();
}

static HParser* uint32_be() {
    return h_uint32();
}

static HParser* uint64_be() {
    return h_uint64();
}

static HParser* varint_parser() {
    return h_many(h_uint8());
}

// Main parser components
HParser* create_header_parser() {
    return h_sequence(
        h_token((const uint8_t*)"SQLite format 3\000", 16),
        uint16_be(),  // Page size
        h_uint8(),    // Write version
        h_uint8(),    // Read version
        h_uint8(),    // Reserved space
        h_uint8(),    // Max fraction
        h_uint8(),    // Min fraction
        h_uint8(),    // Leaf fraction
        uint32_be(),  // File change counter
        uint32_be(),  // Database size
        uint32_be(),  // First free page
        uint32_be(),  // Free page count
        uint32_be(),  // Schema cookie
        uint32_be(),  // Schema format
        uint32_be(),  // Page cache size
        uint32_be(),  // Vacuum page
        uint32_be(),  // Text encoding
        uint32_be(),  // User version
        uint32_be(),  // Incremental vacuum
        uint32_be(),  // Application ID
        h_repeat_n(h_uint8(), 20),  // Reserved
        uint32_be(),  // Version valid for
        uint32_be(),  // SQLite version number
        NULL);
}

HParser* create_record_parser() {
    return h_sequence(
        varint_parser(),  // Header size
        h_many(h_choice(h_uint8(), varint_parser(), NULL)),  // Column types and values
        NULL);
}

HParser* create_btree_page_parser() {
    return h_sequence(
        h_uint8(),    // Page type
        h_uint16(),   // First freeblock
        h_uint16(),   // Cell count
        h_uint16(),   // Cell content area
        h_uint8(),    // Fragmented free bytes
        h_many(h_uint16()),  // Cell pointers
        NULL);
}

HParser* create_sqlite_parser() {
    return h_sequence(
        create_header_parser(),
        h_many(create_btree_page_parser()),
        NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite_file>\n", argv[0]);
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

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser *sqlite_parser = create_sqlite_parser();
    HParseResult *result = h_parse(sqlite_parser, buffer, file_size);

    if (result) {
        printf("Successfully parsed SQLite3 database\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse SQLite3 database\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}