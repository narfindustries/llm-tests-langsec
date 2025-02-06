#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Forward declarations
HParser* sqlite_header();
HParser* page_parser();
HParser* btree_page();
HParser* freelist_page();
HParser* overflow_page();
HParser* cell_parser();
HParser* varint_parser();
HParser* record_parser();

// Varint parser
HParser* varint_parser() {
    return h_many1(h_bits(8, false));
}

// Record format parser
HParser* record_parser() {
    return h_sequence(
        varint_parser(),  // header length
        h_optional(varint_parser()),  // optional row ID
        h_many1(h_choice(h_int_range(h_uint8(), 0, 11),  // Serial types 0-11
                        varint_parser(),                  // BLOB and TEXT types
                        NULL))
    );
}

// Cell parser
HParser* cell_parser() {
    return h_sequence(
        varint_parser(),  // payload size
        varint_parser(),  // row ID
        record_parser(),  // payload
        NULL
    );
}

// B-tree page parser
HParser* btree_page() {
    return h_sequence(
        h_int_range(h_uint8(), 2, 13),  // page type
        h_uint16(),  // first freeblock offset
        h_uint16(),  // number of cells
        h_uint16(),  // cell content offset
        h_uint8(),   // fragmented free bytes
        h_optional(h_uint32()),  // right child page number (for interior pages)
        h_many1(cell_parser()),  // cells
        NULL
    );
}

// Freelist page parser
HParser* freelist_page() {
    return h_sequence(
        h_uint32(),  // next trunk page
        h_uint32(),  // leaf count
        h_many1(h_uint32()),  // page numbers
        NULL
    );
}

// Overflow page parser
HParser* overflow_page() {
    return h_sequence(
        h_uint32(),  // next page number
        h_many1(h_uint8()),  // content
        NULL
    );
}

// Page parser
HParser* page_parser() {
    return h_choice(
        btree_page(),
        freelist_page(),
        overflow_page(),
        NULL
    );
}

// SQLite header parser
HParser* sqlite_header() {
    return h_sequence(
        h_token((const uint8_t*)"SQLite format 3\000", 16),  // Magic string
        h_uint16(),  // page size
        h_uint8(),   // file format write version
        h_uint8(),   // file format read version
        h_uint8(),   // reserved space
        h_uint8(),   // maximum embedded payload fraction
        h_uint8(),   // minimum embedded payload fraction
        h_uint8(),   // leaf payload fraction
        h_uint32(),  // file change counter
        h_uint32(),  // database size in pages
        h_uint32(),  // first freelist trunk page
        h_uint32(),  // number of freelist pages
        h_uint32(),  // schema cookie
        h_uint32(),  // schema format number
        h_uint32(),  // default page cache size
        h_uint32(),  // largest root b-tree page
        h_int_range(h_uint32(), 1, 3),  // text encoding
        h_uint32(),  // user version
        h_uint32(),  // incremental vacuum mode
        h_uint32(),  // application ID
        h_token((const uint8_t*)"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000", 20),  // reserved
        h_uint32(),  // version valid for number
        h_uint32(),  // SQLite version number
        NULL
    );
}

// Complete SQLite database parser
HParser* sqlite_database() {
    return h_sequence(
        sqlite_header(),
        h_many1(page_parser()),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
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

    HParser* parser = sqlite_database();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Successfully parsed SQLite database\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse SQLite database\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}