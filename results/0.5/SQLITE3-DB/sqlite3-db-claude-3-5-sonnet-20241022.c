#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Forward declarations
HParser* init_sqlite3_parser(void);
HParser* create_header_parser(void);
HParser* create_page_parser(void);
HParser* create_btree_page_header_parser(void);
HParser* create_cell_parser(void);
HParser* create_record_parser(void);
HParser* create_freelist_parser(void);
HParser* create_overflow_parser(void);

// Helper parsers
static HParser* create_varint_parser(void) {
    return h_many1(h_bits(8, false));
}

// Main parser components
HParser* create_header_parser(void) {
    return h_sequence(
        h_token((const uint8_t*)"SQLite format 3\000", 16),
        h_uint16(), // page size
        h_uint8(),  // file format write version
        h_uint8(),  // file format read version
        h_uint8(),  // reserved space
        h_uint8(),  // max embedded payload fraction
        h_uint8(),  // min embedded payload fraction
        h_uint8(),  // leaf payload fraction
        h_uint32(), // file change counter
        h_uint32(), // db size in pages
        h_uint32(), // first freelist trunk page
        h_uint32(), // total freelist pages
        h_uint32(), // schema cookie
        h_uint32(), // schema format number
        h_uint32(), // default page cache size
        h_uint32(), // largest root btree page
        h_uint32(), // text encoding
        h_uint32(), // user version
        h_uint32(), // incremental vacuum mode
        h_uint32(), // application id
        h_repeat_n(h_uint8(), 20), // reserved for expansion
        h_uint32(), // version valid for number
        h_uint32(), // sqlite version number
        NULL
    );
}

HParser* create_btree_page_header_parser(void) {
    return h_sequence(
        h_uint8(),  // page type
        h_uint16(), // first freeblock offset
        h_uint16(), // number of cells
        h_uint16(), // cell content offset
        h_uint8(),  // fragmented free bytes
        h_optional(h_uint32()), // right child pointer (for interior pages)
        NULL
    );
}

HParser* create_cell_parser(void) {
    return h_choice(
        h_sequence(h_uint32(), create_varint_parser(), NULL), // interior table
        h_sequence(create_varint_parser(), create_varint_parser(), 
                  h_many1(h_uint8()), h_optional(h_uint32()), NULL), // leaf table
        h_sequence(h_uint32(), create_varint_parser(), h_many1(h_uint8()), NULL), // interior index
        h_sequence(create_varint_parser(), h_many1(h_uint8()), NULL), // leaf index
        NULL
    );
}

HParser* create_record_parser(void) {
    return h_sequence(
        create_varint_parser(), // header size
        h_many1(create_varint_parser()), // serial type codes
        h_many1(h_uint8()), // data values
        NULL
    );
}

HParser* create_freelist_parser(void) {
    return h_sequence(
        h_uint32(), // next trunk page
        h_uint32(), // number of leaf pages
        h_many1(h_uint32()), // page numbers
        NULL
    );
}

HParser* create_overflow_parser(void) {
    return h_sequence(
        h_uint32(), // next overflow page
        h_many1(h_uint8()), // overflow data
        NULL
    );
}

HParser* create_page_parser(void) {
    return h_sequence(
        create_btree_page_header_parser(),
        h_many1(create_cell_parser()),
        NULL
    );
}

HParser* init_sqlite3_parser(void) {
    return h_sequence(
        create_header_parser(),
        h_many1(create_page_parser()),
        h_optional(h_many1(create_freelist_parser())),
        h_optional(h_many1(create_overflow_parser())),
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

    HParser *parser = init_sqlite3_parser();
    HParseResult *result = h_parse(parser, input, size);

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