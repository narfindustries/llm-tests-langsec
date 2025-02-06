#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef enum {
    SQLITE_VERSION_3 = 0
} sqlite_magic_version;

typedef enum {
    LEGACY_WRITE = 1,
    WAL_WRITE = 2
} sqlite_write_version;

typedef enum {
    LEGACY_READ = 1,
    WAL_READ = 2
} sqlite_read_version;

typedef enum {
    INTERIOR_INDEX = 0x02,
    INTERIOR_TABLE = 0x05,
    LEAF_INDEX = 0x0A,
    LEAF_TABLE = 0x0D
} sqlite_page_type;

typedef enum {
    UTF8 = 1,
    UTF16LE = 2,
    UTF16BE = 3
} sqlite_text_encoding;

typedef struct {
    uint8_t magic_header[16];
    uint16_t page_size;
    sqlite_write_version write_version;
    sqlite_read_version read_version;
    uint8_t unused_space;
    uint8_t max_embedded_payload_fraction;
    uint8_t min_embedded_payload_fraction;
    uint8_t leaf_payload_fraction;
    uint32_t file_change_counter;
    uint32_t database_size_pages;
    uint32_t first_freelist_trunk_page;
    uint32_t total_freelist_pages;
    uint32_t schema_cookie;
    uint32_t schema_format;
    uint32_t default_page_cache_size;
    uint32_t largest_root_page;
    sqlite_text_encoding text_encoding;
    uint32_t user_version;
    uint32_t incremental_vacuum_mode;
    uint32_t application_id;
    uint8_t reserved_space[20];
} sqlite_file_header;

typedef struct {
    sqlite_page_type page_type;
    uint16_t first_freeblock_offset;
    uint16_t number_of_cells;
    uint16_t cell_content_area_start;
    uint8_t fragmented_free_bytes;
    uint32_t right_child_page;
} sqlite_page_header;

static HParser* sqlite_magic_parser() {
    const char* magic_str = "SQLite format 3\0";
    return h_literal_str(magic_str, 16);
}

static HParser* sqlite_file_header_parser() {
    return h_sequence(
        sqlite_magic_parser(),
        h_uint16(),
        h_uint8(),
        h_uint8(),
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
        h_uint32(),
        h_uint32(),
        h_repeat_n(h_uint8(), 20),
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

    size_t bytes_read = fread(buffer, 1, file_size, file);
    fclose(file);

    if (bytes_read != file_size) {
        perror("Error reading file");
        free(buffer);
        return 1;
    }

    HParser* header_parser = sqlite_file_header_parser();
    HParseResult* parsed_result = h_parse(header_parser, buffer, file_size);

    if (!parsed_result || !parsed_result->ast) {
        fprintf(stderr, "Parsing failed\n");
        h_parse_result_free(parsed_result);
        h_parser_free(header_parser);
        free(buffer);
        return 1;
    }

    h_parse_result_free(parsed_result);
    h_parser_free(header_parser);
    free(buffer);

    return 0;
}