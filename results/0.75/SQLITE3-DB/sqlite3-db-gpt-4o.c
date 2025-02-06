#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_sqlite3_parser() {
    HParser *header_string = h_sequence(
        h_token((const uint8_t *)"SQLite format 3\0", 16),
        NULL
    );
    HParser *page_size = h_be_u16(); // Big-endian 0x10 – 0x11
    HParser *write_version = h_u8();
    HParser *read_version = h_u8();
    HParser *reserved_space = h_u8();
    HParser *max_payload_fraction = h_u8();
    HParser *min_payload_fraction = h_u8();
    HParser *leaf_payload_fraction = h_u8();
    HParser *file_change_counter = h_be_u32(); // Big-endian 0x18 – 0x1B
    HParser *database_size_pages = h_be_u32(); // Big-endian 0x1C – 0x1F
    HParser *first_freelist_trunk_page = h_be_u32(); // Big-endian 0x20 – 0x23
    HParser *total_freelist_pages = h_be_u32(); // Big-endian 0x24 – 0x27
    HParser *schema_cookie = h_be_u32(); // Big-endian 0x28 – 0x2B
    HParser *schema_format_number = h_be_u16(); // Big-endian 0x2C – 0x2D
    HParser *default_cache_size = h_be_u32(); // Big-endian 0x2E – 0x31
    HParser *largest_root_btree_page = h_be_u32(); // Big-endian 0x32 – 0x35
    HParser *text_encoding = h_be_u16(); // Big-endian 0x36 – 0x37
    HParser *user_version = h_be_u32(); // Big-endian 0x38 – 0x3B
    HParser *incremental_vacuum_mode = h_be_u32(); // Big-endian 0x3C – 0x3F
    HParser *application_id = h_be_u32(); // Big-endian 0x40 – 0x43
    HParser *reserved_expansion = h_repeat_n(h_u8(), 12); // 0x44 – 0x4F
    HParser *version_valid_for = h_be_u32(); // Big-endian 0x50 – 0x53
    HParser *sqlite_version = h_be_u32(); // Big-endian 0x54 – 0x57

    return h_sequence(
        header_string,
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_payload_fraction,
        min_payload_fraction,
        leaf_payload_fraction,
        file_change_counter,
        database_size_pages,
        first_freelist_trunk_page,
        total_freelist_pages,
        schema_cookie,
        schema_format_number,
        default_cache_size,
        largest_root_btree_page,
        text_encoding,
        user_version,
        incremental_vacuum_mode,
        application_id,
        reserved_expansion,
        version_valid_for,
        sqlite_version,
        NULL
    );
}

void parse_sqlite3_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = (unsigned char *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = create_sqlite3_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("SQLite file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse SQLite file.\n");
    }

    free(buffer);
    h_parser_destroy(parser);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite3_db_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_sqlite3_file(argv[1]);

    return EXIT_SUCCESS;
}