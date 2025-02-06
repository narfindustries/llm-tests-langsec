#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

typedef struct {
    char magic_header[16];
    uint16_t page_size;
    uint8_t write_version;
    uint8_t read_version;
    uint8_t reserved_space;
    uint8_t max_embedded_payload;
    uint8_t min_embedded_payload;
    uint8_t leaf_payload_fraction;
    uint32_t file_change_counter;
    uint32_t database_size_pages;
    uint32_t first_freelist_page;
    uint32_t total_freelist_pages;
    uint32_t schema_cookie;
    uint32_t schema_format;
    uint32_t default_encoding;
    uint32_t user_version;
    uint32_t incremental_vacuum_mode;
    uint32_t application_id;
} SQLiteHeader;

HParseResult* parse_sqlite_header(void* input, size_t length) {
    HParser* magic_header = h_token((uint8_t*)"SQLite format 3\0", 16);
    HParser* page_size = h_int_range(h_uintrange(512, 65536), 512, 65536);
    HParser* write_version = h_uint8();
    HParser* read_version = h_uint8();
    HParser* reserved_space = h_uint8();
    HParser* max_embedded_payload = h_uint8();
    HParser* min_embedded_payload = h_uint8();
    HParser* leaf_payload_fraction = h_uint8();
    HParser* file_change_counter = h_uint32();
    HParser* database_size_pages = h_uint32();
    HParser* first_freelist_page = h_uint32();
    HParser* total_freelist_pages = h_uint32();
    HParser* schema_cookie = h_uint32();
    HParser* schema_format = h_uint32();
    HParser* default_encoding = h_uint32();
    HParser* user_version = h_uint32();
    HParser* incremental_vacuum_mode = h_uint32();
    HParser* application_id = h_uint32();

    HParser* sqlite_header = h_sequence(
        magic_header, page_size, 
        write_version, read_version, 
        reserved_space, 
        max_embedded_payload, 
        min_embedded_payload, 
        leaf_payload_fraction,
        file_change_counter,
        database_size_pages,
        first_freelist_page,
        total_freelist_pages,
        schema_cookie,
        schema_format,
        default_encoding,
        user_version,
        incremental_vacuum_mode,
        application_id, 
        NULL);

    return h_parse(sqlite_header, input, length);
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
    long file_length = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_length);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_length, file);
    fclose(file);

    if (read_size != file_length) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParseResult* parsed_result = parse_sqlite_header(buffer, read_size);
    
    if (parsed_result) {
        printf("SQLite file parsed successfully\n");
        h_parse_result_free(parsed_result);
    } else {
        printf("Parsing failed\n");
    }

    free(buffer);
    return 0;
}