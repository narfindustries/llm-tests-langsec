#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

void init_parsers(HParser **uint8_parser, HParser **uint16_parser, HParser **uint32_parser, HParser **uint64_parser, HParser **int64_parser, HParser **magic_header_parser, HParser **page_size_parser, HParser **write_version_parser, HParser **read_version_parser, HParser **reserved_space_parser, HParser **max_embedded_payload_parser, HParser **min_embedded_payload_parser, HParser **leaf_payload_fraction_parser, HParser **file_change_counter_parser, HParser **database_size_in_pages_parser, HParser **first_freelist_page_parser, HParser **freelist_page_count_parser, HParser **schema_cookie_parser, HParser **schema_format_number_parser, HParser **default_page_cache_size_parser, HParser **largest_b_tree_page_parser, HParser **text_encoding_parser, HParser **user_version_parser, HParser **incremental_vacuum_parser, HParser **application_id_parser, HParser **reserved_parser, HParser **version_valid_for_parser, HParser **sqlite_version_number_parser, HParser **database_header_parser, HParser **page_type_parser, HParser **first_free_block_parser, HParser **cell_count_parser, HParser **cell_content_offset_parser, HParser **fragmented_free_bytes_parser, HParser **right_child_pointer_parser, HParser **cell_pointers_parser, HParser **cell_content_parser, HParser **b_tree_page_parser, HParser **row_id_parser, HParser **payload_size_parser, HParser **payload_parser, HParser **overflow_page_number_parser, HParser **cell_structure_parser, HParser **next_freelist_page_parser, HParser **number_of_leaves_parser, HParser **leaf_page_numbers_parser, HParser **freelist_page_parser, HParser **overflow_data_parser, HParser **overflow_page_parser, HParser **magic_number_parser, HParser **file_format_version_parser, HParser **wal_page_size_parser, HParser **checkpoint_sequence_parser, HParser **salt_values_parser, HParser **checksum_parser, HParser **frames_parser, HParser **wal_journal_parser) {
    *uint8_parser = h_uint8();
    *uint16_parser = h_uint16();
    *uint32_parser = h_uint32();
    *uint64_parser = h_uint64();
    *int64_parser = h_int64();
    *magic_header_parser = h_sequence(h_ch('\x53'), h_ch('\x51'), h_ch('\x4c'), h_ch('\x69'), h_ch('\x74'), h_ch('\x65'), h_ch('\x20'), h_ch('\x66'), h_ch('\x6f'), h_ch('\x72'), h_ch('\x6d'), h_ch('\x61'), h_ch('\x74'), h_ch('\x20'), h_ch('\x33'), h_ch('\x00'), NULL);
    *page_size_parser = *uint16_parser;
    *write_version_parser = *uint8_parser;
    *read_version_parser = *uint8_parser;
    *reserved_space_parser = *uint8_parser;
    *max_embedded_payload_parser = *uint8_parser;
    *min_embedded_payload_parser = *uint8_parser;
    *leaf_payload_fraction_parser = *uint8_parser;
    *file_change_counter_parser = *uint32_parser;
    *database_size_in_pages_parser = *uint32_parser;
    *first_freelist_page_parser = *uint32_parser;
    *freelist_page_count_parser = *uint32_parser;
    *schema_cookie_parser = *uint32_parser;
    *schema_format_number_parser = *uint32_parser;
    *default_page_cache_size_parser = *uint32_parser;
    *largest_b_tree_page_parser = *uint32_parser;
    *text_encoding_parser = *uint32_parser;
    *user_version_parser = *uint32_parser;
    *incremental_vacuum_parser = *uint32_parser;
    *application_id_parser = *uint32_parser;
    *reserved_parser = h_repeat_n(*uint8_parser, 20);
    *version_valid_for_parser = *uint32_parser;
    *sqlite_version_number_parser = *uint32_parser;
    *database_header_parser = h_sequence(*magic_header_parser, *page_size_parser, *write_version_parser, *read_version_parser, *reserved_space_parser, *max_embedded_payload_parser, *min_embedded_payload_parser, *leaf_payload_fraction_parser, *file_change_counter_parser, *database_size_in_pages_parser, *first_freelist_page_parser, *freelist_page_count_parser, *schema_cookie_parser, *schema_format_number_parser, *default_page_cache_size_parser, *largest_b_tree_page_parser, *text_encoding_parser, *user_version_parser, *incremental_vacuum_parser, *application_id_parser, *reserved_parser, *version_valid_for_parser, *sqlite_version_number_parser, NULL);
    *page_type_parser = *uint8_parser;
    *first_free_block_parser = *uint16_parser;
    *cell_count_parser = *uint16_parser;
    *cell_content_offset_parser = *uint16_parser;
    *fragmented_free_bytes_parser = *uint8_parser;
    *right_child_pointer_parser = *uint32_parser;
    *cell_pointers_parser = h_repeat_n(*uint16_parser, 0);
    *cell_content_parser = h_repeat_n(*uint8_parser, 0);
    *b_tree_page_parser = h_sequence(*page_type_parser, *first_free_block_parser, *cell_count_parser, *cell_content_offset_parser, *fragmented_free_bytes_parser, *right_child_pointer_parser, *cell_pointers_parser, *cell_content_parser, NULL);
    *row_id_parser = *int64_parser;
    *payload_size_parser = h_uint32(); // Replaced h_variable_length_integer() with h_uint32()
    *payload_parser = h_repeat_n(*uint8_parser, 0);
    *overflow_page_number_parser = *uint32_parser;
    *cell_structure_parser = h_sequence(*row_id_parser, *payload_size_parser, *payload_parser, *overflow_page_number_parser, NULL);
    *next_freelist_page_parser = *uint32_parser;
    *number_of_leaves_parser = *uint32_parser;
    *leaf_page_numbers_parser = h_repeat_n(*uint32_parser, 0);
    *freelist_page_parser = h_sequence(*next_freelist_page_parser, *number_of_leaves_parser, *leaf_page_numbers_parser, NULL);
    *overflow_data_parser = h_repeat_n(*uint8_parser, 0);
    *overflow_page_parser = h_sequence(*overflow_data_parser, NULL);
    *magic_number_parser = *uint32_parser;
    *file_format_version_parser = *uint32_parser;
    *wal_page_size_parser = *uint32_parser;
    *checkpoint_sequence_parser = *uint32_parser;
    *salt_values_parser = h_repeat_n(*uint32_parser, 2);
    *checksum_parser = h_repeat_n(*uint32_parser, 2);
    *frames_parser = h_repeat_n(*uint8_parser, 0);
    *wal_journal_parser = h_sequence(*magic_number_parser, *file_format_version_parser, *wal_page_size_parser, *checkpoint_sequence_parser, *salt_values_parser, *checksum_parser, *frames_parser, NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *uint8_parser, *uint16_parser, *uint32_parser, *uint64_parser, *int64_parser, *magic_header_parser, *page_size_parser, *write_version_parser, *read_version_parser, *reserved_space_parser, *max_embedded_payload_parser, *min_embedded_payload_parser, *leaf_payload_fraction_parser, *file_change_counter_parser, *database_size_in_pages_parser, *first_freelist_page_parser, *freelist_page_count_parser, *schema_cookie_parser, *schema_format_number_parser, *default_page_cache_size_parser, *largest_b_tree_page_parser, *text_encoding_parser, *user_version_parser, *incremental_vacuum_parser, *application_id_parser, *reserved_parser, *version_valid_for_parser, *sqlite_version_number_parser, *database_header_parser, *page_type_parser, *first_free_block_parser, *cell_count_parser, *cell_content_offset_parser, *fragmented_free_bytes_parser, *right_child_pointer_parser, *cell_pointers_parser, *cell_content_parser, *b_tree_page_parser, *row_id_parser, *payload_size_parser, *payload_parser, *overflow_page_number_parser, *cell_structure_parser, *next_freelist_page_parser, *number_of_leaves_parser, *leaf_page_numbers_parser, *freelist_page_parser, *overflow_data_parser, *overflow_page_parser, *magic_number_parser, *file_format_version_parser, *wal_page_size_parser, *checkpoint_sequence_parser, *salt_values_parser, *checksum_parser, *frames_parser, *wal_journal_parser;

    init_parsers(&uint8_parser, &uint16_parser, &uint32_parser, &uint64_parser, &int64_parser, &magic_header_parser, &page_size_parser, &write_version_parser, &read_version_parser, &reserved_space_parser, &max_embedded_payload_parser, &min_embedded_payload_parser, &leaf_payload_fraction_parser, &file_change_counter_parser, &database_size_in_pages_parser, &first_freelist_page_parser, &freelist_page_count_parser, &schema_cookie_parser, &schema_format_number_parser, &default_page_cache_size_parser, &largest_b_tree_page_parser, &text_encoding_parser, &user_version_parser, &incremental_vacuum_parser, &application_id_parser, &reserved_parser, &version_valid_for_parser, &sqlite_version_number_parser, &database_header_parser, &page_type_parser, &first_free_block_parser, &cell_count_parser, &cell_content_offset_parser, &fragmented_free_bytes_parser, &right_child_pointer_parser, &cell_pointers_parser, &cell_content_parser, &b_tree_page_parser, &row_id_parser, &payload_size_parser, &payload_parser, &overflow_page_number_parser, &cell_structure_parser, &next_freelist_page_parser, &number_of_leaves_parser, &leaf_page_numbers_parser, &freelist_page_parser, &overflow_data_parser, &overflow_page_parser, &magic_number_parser, &file_format_version_parser, &wal_page_size_parser, &checkpoint_sequence_parser, &salt_values_parser, &checksum_parser, &frames_parser, &wal_journal_parser);

    HParseResult *result = h_parse(database_header_parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse file\n");
        free(buffer);
        return 1;
    }

    printf("Parsed database header successfully\n");

    free(buffer);
    h_parse_result_free(result);
    return 0;
}