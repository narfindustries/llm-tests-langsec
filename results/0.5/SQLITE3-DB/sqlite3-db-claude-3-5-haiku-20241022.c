#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    char magic_header[16];
    uint16_t page_size;
    uint8_t write_version;
    uint8_t read_version;
    uint8_t unused;
    uint8_t max_embedded_payload;
    uint8_t min_embedded_payload;
    uint8_t leaf_payload;
    uint32_t file_change_counter;
    uint32_t database_size_pages;
    uint32_t first_freelist_trunk_page;
    uint32_t total_freelist_pages;
    uint32_t schema_cookie;
    uint32_t schema_format;
    uint32_t default_encoding;
    uint32_t user_version;
    uint32_t incremental_vacuum_mode;
    uint32_t application_id;
    uint8_t reserved[20];
} SQLiteHeader;

HParser* create_sqlite_header_parser() {
    HParser* magic_header = h_token((uint8_t*)"SQLite format 3\0", 16);
    HParser* page_size = h_uint16();
    HParser* write_version = h_uint8();
    HParser* read_version = h_uint8();
    HParser* unused = h_uint8();
    HParser* max_embedded_payload = h_uint8();
    HParser* min_embedded_payload = h_uint8();
    HParser* leaf_payload = h_uint8();
    HParser* file_change_counter = h_uint32();
    HParser* database_size_pages = h_uint32();
    HParser* first_freelist_trunk_page = h_uint32();
    HParser* total_freelist_pages = h_uint32();
    HParser* schema_cookie = h_uint32();
    HParser* schema_format = h_uint32();
    HParser* default_encoding = h_uint32();
    HParser* user_version = h_uint32();
    HParser* incremental_vacuum_mode = h_uint32();
    HParser* application_id = h_uint32();
    HParser* reserved = h_repeat_n(h_uint8(), 20);

    return h_sequence(
        magic_header, page_size, write_version, read_version, 
        unused, max_embedded_payload, min_embedded_payload, 
        leaf_payload, file_change_counter, database_size_pages, 
        first_freelist_trunk_page, total_freelist_pages, 
        schema_cookie, schema_format, default_encoding, 
        user_version, incremental_vacuum_mode, application_id, 
        reserved, NULL
    );
}

typedef struct {
    uint8_t page_type;
    uint16_t first_freeblock;
    uint16_t cell_count;
    uint16_t cell_content_start;
    uint8_t fragmented_free_bytes;
} PageHeader;

HParser* parse_page_header() {
    HParser* page_type = h_uint8();
    HParser* first_freeblock = h_uint16();
    HParser* cell_count = h_uint16();
    HParser* cell_content_start = h_uint16();
    HParser* fragmented_free_bytes = h_uint8();

    return h_sequence(
        page_type, first_freeblock, cell_count, 
        cell_content_start, fragmented_free_bytes, NULL
    );
}

typedef enum {
    TYPE_NULL = 0,
    TYPE_INT1 = 1,
    TYPE_INT2 = 2,
    TYPE_INT3 = 3,
    TYPE_INT4 = 4,
    TYPE_INT6 = 5,
    TYPE_INT8 = 6,
    TYPE_FLOAT = 7,
    TYPE_ZERO = 8,
    TYPE_ONE = 9
} RecordType;

HParser* parse_record_type() {
    return h_choice(
        h_uint8(),
        h_uint16(),
        h_uint32(),
        h_uint64(),
        h_int_range(H_SIGNED, 0, INT64_MAX),
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
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* sqlite_header_parser = create_sqlite_header_parser();
    HParseResult* parsed_header = h_parse(sqlite_header_parser, buffer, file_size);
    
    if (!parsed_header || !parsed_header->ast) {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    h_parse_result_free(parsed_header);
    free(buffer);
    fclose(file);

    return 0;
}