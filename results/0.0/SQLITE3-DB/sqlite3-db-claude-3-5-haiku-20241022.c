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
    uint8_t unused_space;
    uint8_t max_embedded_payload_fraction;
    uint8_t min_embedded_payload_fraction;
    uint8_t leaf_payload_fraction;
    uint32_t file_change_counter;
    uint32_t database_size_pages;
    uint32_t first_freelist_page;
    uint32_t total_freelist_pages;
    uint32_t schema_cookie;
    uint32_t schema_format;
    uint32_t default_page_cache_size;
    uint32_t largest_root_page;
    uint32_t text_encoding;
    uint32_t user_version;
    uint32_t incremental_vacuum_mode;
    uint32_t application_id;
    uint8_t reserved_space[20];
} SQLiteHeader;

HParser* parse_sqlite_header() {
    HParser* magic_header = h_token("SQLite format 3\0", 16);
    HParser* page_size = h_uint16();
    HParser* write_version = h_uint8();
    HParser* read_version = h_uint8();
    HParser* unused_space = h_uint8();
    HParser* max_payload = h_uint8();
    HParser* min_payload = h_uint8();
    HParser* leaf_payload = h_uint8();
    HParser* file_change_counter = h_uint32();
    HParser* db_size_pages = h_uint32();
    HParser* first_freelist = h_uint32();
    HParser* total_freelist = h_uint32();
    HParser* schema_cookie = h_uint32();
    HParser* schema_format = h_uint32();
    HParser* page_cache_size = h_uint32();
    HParser* largest_root = h_uint32();
    HParser* text_encoding = h_uint32();
    HParser* user_version = h_uint32();
    HParser* vacuum_mode = h_uint32();
    HParser* app_id = h_uint32();
    HParser* reserved = h_repeat_n(h_uint8(), 20);

    return h_sequence(
        magic_header, page_size, write_version, read_version,
        unused_space, max_payload, min_payload, leaf_payload,
        file_change_counter, db_size_pages, first_freelist,
        total_freelist, schema_cookie, schema_format,
        page_cache_size, largest_root, text_encoding,
        user_version, vacuum_mode, app_id, reserved, NULL
    );
}

HParser* parse_serial_type() {
    HParser* null_type = h_int_range(h_uint8(), 0, 11);
    HParser* blob_text_type = h_int_range(h_uint8(), 12, INT16_MAX);
    return h_choice(null_type, blob_text_type, NULL);
}

HParser* parse_record() {
    HParser* header_length = h_int_range(h_uint8(), 0, 255);
    HParser* serial_types = h_many(parse_serial_type());
    HParser* record_data = h_many(h_uint8());

    return h_sequence(header_length, serial_types, record_data, NULL);
}

HParser* parse_page() {
    HParser* page_type = h_uint8();
    HParser* records = h_many(parse_record());

    return h_sequence(page_type, records, NULL);
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

    size_t read_size = fread(buffer, 1, file_size, file);
    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* sqlite_parser = parse_sqlite_header();
    HParseResult* result = h_parse(sqlite_parser, buffer, file_size);

    if (result && result->ast) {
        printf("SQLite file parsed successfully\n");
    } else {
        printf("Parsing failed\n");
    }

    h_parse_result_free(result);
    h_parser_free(sqlite_parser);
    free(buffer);
    fclose(file);

    return 0;
}