#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Forward declarations
HParser* init_sqlite3_parser(void);
HParser* init_header_parser(void);
HParser* init_btree_page_parser(void);
HParser* init_cell_parser(void);
HParser* init_record_parser(void);
HParser* init_varint_parser(void);
HParser* init_freelist_parser(void);
HParser* init_ptrmap_parser(void);

// Varint parser
HParser* init_varint_parser(void) {
    return h_many1(h_bits(8, false));  // Simplified varint parser
}

// Database Header parser
HParser* init_header_parser(void) {
    return h_sequence(
        h_token((const uint8_t*)"SQLite format 3\000", 16),
        h_uint16(),  // page size
        h_uint8(),   // file format write version
        h_uint8(),   // file format read version
        h_uint8(),   // reserved space
        h_uint8(),   // max embedded payload fraction
        h_uint8(),   // min embedded payload fraction
        h_uint8(),   // leaf payload fraction
        h_uint32(),  // file change counter
        h_uint32(),  // db size in pages
        h_uint32(),  // first freelist trunk page
        h_uint32(),  // number of freelist pages
        h_uint32(),  // schema cookie
        h_uint32(),  // schema format number
        h_uint32(),  // default page cache size
        h_uint32(),  // largest root btree page
        h_uint32(),  // text encoding
        h_uint32(),  // user version
        h_uint32(),  // incremental vacuum mode
        h_uint32(),  // application id
        h_repeat_n(h_uint8(), 20),  // reserved space
        h_uint32(),  // version valid for number
        h_uint32(),  // sqlite version number
        NULL
    );
}

// B-tree Page Header parser
HParser* init_btree_page_parser(void) {
    return h_sequence(
        h_uint8(),   // page type
        h_uint16(),  // first freeblock offset
        h_uint16(),  // number of cells
        h_uint16(),  // cell content area offset
        h_uint8(),   // fragmented free bytes
        h_optional(h_uint32()),  // right child pointer (for interior pages)
        NULL
    );
}

// Cell parser
HParser* init_cell_parser(void) {
    return h_choice(
        h_sequence(h_uint32(), init_varint_parser(), NULL),  // interior table
        h_sequence(init_varint_parser(), init_varint_parser(), h_many1(h_uint8()), NULL),  // leaf table
        h_sequence(h_uint32(), h_many1(h_uint8()), NULL),  // interior index
        h_sequence(init_varint_parser(), h_many1(h_uint8()), NULL),  // leaf index
        NULL
    );
}

// Record parser
HParser* init_record_parser(void) {
    return h_sequence(
        init_varint_parser(),  // header size
        h_many1(init_varint_parser()),  // serial type codes
        h_many1(h_uint8()),  // data values
        NULL
    );
}

// Freelist parser
HParser* init_freelist_parser(void) {
    return h_sequence(
        h_uint32(),  // next trunk page
        h_uint32(),  // number of leaf pages
        h_many1(h_uint32()),  // leaf page numbers
        NULL
    );
}

// Pointer Map parser
HParser* init_ptrmap_parser(void) {
    return h_many1(h_uint8());
}

// Main SQLite3 parser
HParser* init_sqlite3_parser(void) {
    return h_sequence(
        init_header_parser(),
        h_many1(h_choice(
            init_btree_page_parser(),
            init_freelist_parser(),
            init_ptrmap_parser(),
            NULL
        )),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite3_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }

    HParser *sqlite3_parser = init_sqlite3_parser();
    HParseResult *result = h_parse(sqlite3_parser, input, size);

    if (result) {
        printf("Successfully parsed SQLite3 database\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse SQLite3 database\n");
    }

    free(input);
    fclose(f);
    return 0;
}