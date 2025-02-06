#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Signature constants
#define LOCAL_FILE_HEADER_SIG 0x04034b50
#define CENTRAL_DIR_SIG 0x02014b50
#define END_CENTRAL_DIR_SIG 0x06054b50
#define DIGITAL_SIG 0x05054b50

// Parser declarations
HParser *zip_parser;
HParser *local_file_header;
HParser *central_directory;
HParser *end_central_directory;
HParser *digital_signature;

void init_parsers() {
    // Basic types
    HParser *uint16 = h_uint16();
    HParser *uint32 = h_uint32();
    HParser *uint8 = h_uint8();
    
    // Local File Header
    HParser *local_header_sig = h_int_range(h_uint32(), LOCAL_FILE_HEADER_SIG, LOCAL_FILE_HEADER_SIG);
    HParser *version_needed = uint16;
    HParser *general_purpose_flag = uint16;
    HParser *compression_method = uint16;
    HParser *last_mod_time = uint16;
    HParser *last_mod_date = uint16;
    HParser *crc32 = uint32;
    HParser *compressed_size = uint32;
    HParser *uncompressed_size = uint32;
    HParser *filename_length = uint16;
    HParser *extra_field_length = uint16;
    
    local_file_header = h_sequence(
        local_header_sig,
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
        h_length_value(filename_length, uint8),
        h_length_value(extra_field_length, uint8),
        NULL
    );

    // Central Directory
    HParser *central_dir_sig = h_int_range(h_uint32(), CENTRAL_DIR_SIG, CENTRAL_DIR_SIG);
    HParser *version_made_by = uint16;
    HParser *file_comment_length = uint16;
    HParser *disk_number_start = uint16;
    HParser *internal_attrs = uint16;
    HParser *external_attrs = uint32;
    HParser *local_header_offset = uint32;

    central_directory = h_sequence(
        central_dir_sig,
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
        file_comment_length,
        disk_number_start,
        internal_attrs,
        external_attrs,
        local_header_offset,
        h_length_value(filename_length, uint8),
        h_length_value(extra_field_length, uint8),
        h_length_value(file_comment_length, uint8),
        NULL
    );

    // End of Central Directory
    HParser *end_central_dir_sig = h_int_range(h_uint32(), END_CENTRAL_DIR_SIG, END_CENTRAL_DIR_SIG);
    HParser *disk_number = uint16;
    HParser *central_dir_disk = uint16;
    HParser *num_entries_disk = uint16;
    HParser *num_entries_total = uint16;
    HParser *central_dir_size = uint32;
    HParser *central_dir_offset = uint32;
    HParser *comment_length = uint16;

    end_central_directory = h_sequence(
        end_central_dir_sig,
        disk_number,
        central_dir_disk,
        num_entries_disk,
        num_entries_total,
        central_dir_size,
        central_dir_offset,
        comment_length,
        h_length_value(comment_length, uint8),
        NULL
    );

    // Digital Signature
    HParser *digital_sig_header = h_int_range(h_uint32(), DIGITAL_SIG, DIGITAL_SIG);
    HParser *sig_size = uint16;

    digital_signature = h_sequence(
        digital_sig_header,
        sig_size,
        h_length_value(sig_size, uint8),
        NULL
    );

    // Complete ZIP file structure
    zip_parser = h_sequence(
        h_many(local_file_header),
        h_many(central_directory),
        end_central_directory,
        h_optional(digital_signature),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *input = malloc(size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(input, 1, size, f) != size) {
        perror("Failed to read file");
        free(input);
        fclose(f);
        return 1;
    }
    fclose(f);

    init_parsers();

    HParseResult *result = h_parse(zip_parser, input, size);
    if (!result) {
        fprintf(stderr, "Failed to parse ZIP file\n");
        free(input);
        return 1;
    }

    // Success
    printf("Successfully parsed ZIP file\n");

    h_parse_result_free(result);
    free(input);
    return 0;
}