#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for ZIP fields
HParser *version_needed = h_uint16_le;
HParser *general_purpose_flag = h_uint16_le;
HParser *compression_method = h_uint16_le;
HParser *last_mod_time = h_uint16_le;
HParser *last_mod_date = h_uint16_le;
HParser *crc32 = h_uint32_le;
HParser *compressed_size = h_uint32_le;
HParser *uncompressed_size = h_uint32_le;
HParser *filename_length = h_uint16_le;
HParser *extra_field_length = h_uint16_le;

HParser *file_data = h_length_value(h_uint32_le, h_uint8());
HParser *filename = h_length_value(filename_length, h_uint8());
HParser *extra_field = h_length_value(extra_field_length, h_uint8());

// Local file header parser
HParser *local_file_header = h_sequence(
    h_const_uint32(0x04034b50), // Local File Header Signature
    version_needed,
    general_purpose_flag,
    compression_method,
    last_mod_time,
    last_mod_date,
    crc32,
    compressed_size,
    uncompressed_size,
    filename_length,
    extra_field_length,
    filename,
    extra_field,
    file_data,
    NULL
);

// Optional Data Descriptor
HParser *data_descriptor = h_optional(
    h_sequence(
        h_const_uint32(0x08074b50), // Optional Signature
        crc32,
        compressed_size,
        uncompressed_size,
        NULL
    )
);

// Central Directory File Header
HParser *version_made_by = h_uint16_le;
HParser *comment_length = h_uint16_le;
HParser *disk_number_start = h_uint16_le;
HParser *internal_file_attributes = h_uint16_le;
HParser *external_file_attributes = h_uint32_le;
HParser *relative_offset_local_header = h_uint32_le;

HParser *file_comment = h_length_value(comment_length, h_uint8());

HParser *central_dir_file_header = h_sequence(
    h_const_uint32(0x02014b50), // Central Directory File Header Signature
    version_made_by,
    version_needed,
    general_purpose_flag,
    compression_method,
    last_mod_time,
    last_mod_date,
    crc32,
    compressed_size,
    uncompressed_size,
    filename_length,
    extra_field_length,
    comment_length,
    disk_number_start,
    internal_file_attributes,
    external_file_attributes,
    relative_offset_local_header,
    filename,
    extra_field,
    file_comment,
    NULL
);

// End of Central Directory Record
HParser *this_disk = h_uint16_le;
HParser *disk_with_start = h_uint16_le;
HParser *total_entries_this_disk = h_uint16_le;
HParser *total_entries = h_uint16_le;
HParser *size_central_directory = h_uint32_le;
HParser *offset_central_directory = h_uint32_le;
HParser *zip_comment_length = h_uint16_le;
HParser *zip_comment = h_length_value(zip_comment_length, h_uint8());

HParser *end_of_central_directory = h_sequence(
    h_const_uint32(0x06054b50), // End of Central Directory Signature
    this_disk,
    disk_with_start,
    total_entries_this_disk,
    total_entries,
    size_central_directory,
    offset_central_directory,
    zip_comment_length,
    zip_comment,
    NULL
);

// Entire ZIP File Parser
HParser *zip_file = h_sequence(
    h_many(local_file_header),
    data_descriptor,
    h_many(central_dir_file_header),
    end_of_central_directory,
    NULL
);

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
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

    unsigned char *data = malloc(file_size);
    if (!data) {
        fclose(file);
        fprintf(stderr, "Memory allocation failed\n");
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(zip_file, data, file_size);
    if (result) {
        printf("ZIP file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ZIP file.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}