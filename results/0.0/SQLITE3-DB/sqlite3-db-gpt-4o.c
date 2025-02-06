#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_sqlite3_parser() {
    // Define parsers for each field in the SQLite3 header
    HParser *header_string = h_token("SQLite format 3\000", 16);
    HParser *page_size = h_uint16();
    HParser *write_version = h_uint8();
    HParser *read_version = h_uint8();
    HParser *reserved_space = h_uint8();
    HParser *max_embedded_payload_fraction = h_uint8();
    HParser *min_embedded_payload_fraction = h_uint8();
    HParser *leaf_payload_fraction = h_uint8();
    HParser *file_change_counter = h_uint32();
    HParser *database_size_in_pages = h_uint32();
    HParser *first_freelist_trunk_page = h_uint32();
    HParser *total_freelist_pages = h_uint32();
    HParser *schema_cookie = h_uint32();
    HParser *schema_format_number = h_uint32();
    HParser *default_page_cache_size = h_uint32();
    HParser *largest_root_btree_page_number = h_uint32();
    HParser *text_encoding = h_uint32();
    HParser *user_version = h_uint32();
    HParser *incremental_vacuum_mode = h_uint32();
    HParser *application_id = h_uint32();
    HParser *reserved_for_expansion = h_repeat_n(h_uint8(), 20);
    HParser *version_valid_for_number = h_uint32();
    HParser *sqlite_version_number = h_uint32();

    // Combine all parsers into a single parser for the SQLite3 header
    HParser *sqlite3_header = h_sequence(
        header_string,
        page_size,
        write_version,
        read_version,
        reserved_space,
        max_embedded_payload_fraction,
        min_embedded_payload_fraction,
        leaf_payload_fraction,
        file_change_counter,
        database_size_in_pages,
        first_freelist_trunk_page,
        total_freelist_pages,
        schema_cookie,
        schema_format_number,
        default_page_cache_size,
        largest_root_btree_page_number,
        text_encoding,
        user_version,
        incremental_vacuum_mode,
        application_id,
        reserved_for_expansion,
        version_valid_for_number,
        sqlite_version_number,
        NULL
    );

    return sqlite3_header;
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

    unsigned char *buffer = malloc(file_size);
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
        printf("SQLite3 file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse SQLite3 file.\n");
    }

    free(buffer);
    h_parser_free(parser);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite3-db-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_sqlite3_file(argv[1]);
    return EXIT_SUCCESS;
}