#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *sqlite3_parser() {
    HParser *header_string = h_token("SQLite format 3\000", 16);
    HParser *page_size = h_uint16();
    HParser *file_format_write_version = h_uint8(); 
    HParser *file_format_read_version = h_uint8();
    HParser *reserved_space = h_uint8();
    HParser *maximum_embedded_payload_fraction = h_uint8();
    HParser *minimum_embedded_payload_fraction = h_uint8();
    HParser *leaf_payload_fraction = h_uint8();
    HParser *file_change_counter = h_uint32();
    HParser *database_size_in_pages = h_uint32();
    HParser *first_freelist_trunk_page = h_uint32();
    HParser *total_number_of_freelist_pages = h_uint32();
    HParser *schema_cookie = h_uint32();
    HParser *schema_format_number = h_uint32();
    HParser *default_page_cache_size = h_uint32();
    HParser *largest_root_btree_page = h_uint32();
    HParser *text_encoding = h_uint32();
    HParser *user_version = h_uint32();
    HParser *incremental_vacuum_mode = h_uint32();
    HParser *application_id = h_uint32();
    HParser *reserved_for_expansion = h_repeat_n(h_uint8(), 20);
    HParser *version_valid_for_number = h_uint32();
    HParser *sqlite_version_number = h_uint32();

    HParser *parser = h_sequence(
        header_string,
        page_size,
        file_format_write_version,
        file_format_read_version,
        reserved_space,
        maximum_embedded_payload_fraction,
        minimum_embedded_payload_fraction,
        leaf_payload_fraction,
        file_change_counter,
        database_size_in_pages,
        first_freelist_trunk_page,
        total_number_of_freelist_pages,
        schema_cookie,
        schema_format_number,
        default_page_cache_size,
        largest_root_btree_page,
        text_encoding,
        user_version,
        incremental_vacuum_mode,
        application_id,
        reserved_for_expansion,
        version_valid_for_number,
        sqlite_version_number,
        NULL);

    return parser;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <sqlite3_db_file>\n", argv[0]);
        return EXIT_FAILURE;
    }
    
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = sqlite3_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);
    free(buffer);

    if (result) {
        printf("Successfully parsed SQLite3 database file.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse SQLite3 database file.\n");
    }

    h_data_free(parser);
    return EXIT_SUCCESS;
}