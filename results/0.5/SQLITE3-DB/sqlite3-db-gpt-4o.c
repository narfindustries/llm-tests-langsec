#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *sqlite3_parser() {
    // Define parsers for each field in the SQLite3 header
    HParser *header_string = h_token((const uint8_t *)"SQLite format 3\000", 16);
    HParser *page_size = h_choice(
        h_int_range(h_uint16(), 512, 65536), NULL
    );
    HParser *file_format_write_version = h_choice(h_uint8(), NULL);
    HParser *file_format_read_version = h_choice(h_uint8(), NULL);
    HParser *reserved_space = h_uint8();
    HParser *max_embedded_payload_fraction = h_uint8();
    HParser *min_embedded_payload_fraction = h_uint8();
    HParser *leaf_payload_fraction = h_uint8();
    HParser *file_change_counter = h_uint32();
    HParser *database_size_in_pages = h_uint32();
    HParser *first_freelist_trunk_page = h_uint32();
    HParser *total_freelist_pages = h_uint32();
    HParser *schema_cookie = h_uint32();
    HParser *schema_format_number = h_choice(h_uint32(), NULL);
    HParser *default_page_cache_size = h_uint32();
    HParser *largest_root_btree_page_number = h_uint32();
    HParser *text_encoding = h_choice(h_uint32(), NULL);
    HParser *user_version = h_uint32();
    HParser *incremental_vacuum_mode = h_choice(h_uint32(), NULL);
    HParser *application_id = h_uint32();
    HParser *reserved_for_expansion = h_repeat_n(h_uint8(), 20);
    HParser *version_valid_for_number = h_uint32();
    HParser *sqlite_version_number = h_uint32();

    // Combine all fields into a single parser
    return h_sequence(
        header_string,
        page_size,
        file_format_write_version,
        file_format_read_version,
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
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite3-database-file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *file_contents = malloc(file_size);
    if (!file_contents) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(file_contents, 1, file_size, file);
    fclose(file);

    HParser *parser = sqlite3_parser();
    HParseResult *result = h_parse(parser, file_contents, file_size);

    if (result) {
        printf("SQLite3 database header parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse SQLite3 database header.\n");
    }

    free(file_contents);
    h_parser_destroy(parser);

    return EXIT_SUCCESS;
}