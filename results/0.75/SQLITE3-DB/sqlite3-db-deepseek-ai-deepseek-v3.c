#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *sqlite_header_parser() {
    return h_sequence(
        h_bits(16, 0x000053514C697465), // Magic header string
        h_uint16(),                      // Page size
        h_uint8(),                       // File format write version
        h_uint8(),                       // File format read version
        h_uint8(),                        // Reserved space at end of page
        h_uint8(),                        // Maximum embedded payload fraction
        h_uint8(),                        // Minimum embedded payload fraction
        h_uint8(),                        // Leaf payload fraction
        h_uint32(),                       // File change counter
        h_uint32(),                       // Size of the database file in pages
        h_uint32(),                       // Page number of the first freelist trunk page
        h_uint32(),                       // Number of freelist pages
        h_uint32(),                       // Schema cookie
        h_uint32(),                       // Schema format number
        h_uint32(),                       // Default page cache size
        h_uint32(),                       // Largest root B-tree page
        h_uint32(),                       // Text encoding
        h_uint32(),                       // User version
        h_uint32(),                       // Incremental vacuum mode
        h_uint32(),                       // Application ID
        h_ignore(h_bits(160, 0)),         // Reserved for expansion
        h_uint32(),                       // Version valid for number
        h_uint32(),                       // SQLite version number
        NULL
    );
}

HParser *btree_page_header_parser() {
    return h_sequence(
        h_uint8(),                        // Flags
        h_uint16(),                       // Number of cells
        h_uint16(),                       // First free block
        h_uint8(),                        // Number of fragmented free bytes
        h_uint32(),                       // Right child pointer (interior pages only)
        NULL
    );
}

HParser *cell_pointer_array_parser(uint16_t num_cells) {
    return h_repeat_n(h_uint16(), num_cells);
}

HParser *cell_content_parser() {
    return h_sequence(
        h_uint32(),                       // Payload size
        h_uint64(),                       // Row ID
        h_uint32(),                       // Child page (interior B-tree cells)
        h_length_value(h_uint32(), h_sequence(h_uint8(), NULL)), // Payload
        NULL
    );
}

HParser *freelist_trunk_page_parser() {
    return h_sequence(
        h_uint32(),                       // Next trunk page
        h_uint32(),                       // Number of leaf pages
        h_length_value(h_uint32(), h_repeat_n(h_uint32(), h_uint32())), // Leaf page numbers
        NULL
    );
}

HParser *freelist_leaf_page_parser() {
    return h_sequence(
        h_uint32(),                       // Page number
        NULL
    );
}

HParser *pointer_map_entry_parser() {
    return h_sequence(
        h_uint8(),                        // Type
        h_uint32(),                       // Parent page
        h_uint32(),                       // Page number
        NULL
    );
}

HParser *overflow_page_parser() {
    return h_sequence(
        h_uint32(),                       // Next page
        h_length_value(h_uint32(), h_sequence(h_uint8(), NULL)), // Data
        NULL
    );
}

HParser *wal_header_parser() {
    return h_sequence(
        h_uint32(),                       // Magic number
        h_uint32(),                       // File format version
        h_uint32(),                       // Page size
        h_uint32(),                       // Checkpoint sequence number
        h_uint64(),                       // Salt
        h_uint64(),                       // Checksum
        NULL
    );
}

HParser *wal_frame_parser() {
    return h_sequence(
        h_uint32(),                       // Page number
        h_uint32(),                       // Commit frame number
        h_uint64(),                       // Salt
        h_length_value(h_uint32(), h_sequence(h_uint8(), NULL)), // Page data
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

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = sqlite_header_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse SQLite header\n");
        free(data);
        return 1;
    }

    printf("Successfully parsed SQLite header\n");
    h_parse_result_free(result);
    free(data);

    return 0;
}