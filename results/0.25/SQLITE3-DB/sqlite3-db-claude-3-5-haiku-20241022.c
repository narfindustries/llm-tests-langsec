#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t magic[16];
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

HParser* parse_magic_header() {
    return h_token((const uint8_t*)"SQLite format 3\0", 16);
}

HParser* parse_page_size() {
    return h_int_range(h_uint16(), 512, 65536);
}

HParser* parse_version() {
    return h_choice(h_token((const uint8_t*)"\1", 1), h_token((const uint8_t*)"\2", 1), NULL);
}

HParser* parse_sqlite_header() {
    return h_sequence(
        parse_magic_header(),
        parse_page_size(),
        parse_version(),
        parse_version(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
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
    fclose(file);

    HParser* parser = parse_sqlite_header();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("SQLite file parsed successfully\n");
    } else {
        printf("Parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}