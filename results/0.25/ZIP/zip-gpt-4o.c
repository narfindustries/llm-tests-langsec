#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define parsers for the ZIP file format
HParser *signature_parser;
HParser *version_parser;
HParser *bit_flag_parser;
HParser *compression_method_parser;
HParser *mod_time_parser;
HParser *mod_date_parser;
HParser *crc32_parser;
HParser *size_parser;
HParser *name_length_parser;
HParser *extra_length_parser;
HParser *comment_length_parser;
HParser *disk_number_parser;
HParser *internal_attr_parser;
HParser *external_attr_parser;
HParser *offset_parser;

// Helper function for variable-length fields
HParser *variable_length_field(HParser *length_parser) {
    return h_bind(length_parser, (HParser *(*)(const HParseResult *, void *))h_repeat_n, h_uint8(), NULL);
}

void initialize_parsers() {
    signature_parser = h_uint32_le();
    version_parser = h_uint16_le();
    bit_flag_parser = h_uint16_le();
    compression_method_parser = h_uint16_le();
    mod_time_parser = h_uint16_le();
    mod_date_parser = h_uint16_le();
    crc32_parser = h_uint32_le();
    size_parser = h_uint32_le();
    name_length_parser = h_uint16_le();
    extra_length_parser = h_uint16_le();
    comment_length_parser = h_uint16_le();
    disk_number_parser = h_uint16_le();
    internal_attr_parser = h_uint16_le();
    external_attr_parser = h_uint32_le();
    offset_parser = h_uint32_le();
}

// Local File Header parser
HParser *local_file_header_parser() {
    return h_sequence(
        signature_parser,
        version_parser,
        bit_flag_parser,
        compression_method_parser,
        mod_time_parser,
        mod_date_parser,
        crc32_parser,
        size_parser, // Compressed size
        size_parser, // Uncompressed size
        name_length_parser,
        extra_length_parser,
        variable_length_field(name_length_parser), // File name
        variable_length_field(extra_length_parser), // Extra field
        NULL
    );
}

// Central Directory File Header parser
HParser *central_directory_file_header_parser() {
    return h_sequence(
        signature_parser,
        version_parser, // Version made by
        version_parser, // Version needed to extract
        bit_flag_parser,
        compression_method_parser,
        mod_time_parser,
        mod_date_parser,
        crc32_parser,
        size_parser, // Compressed size
        size_parser, // Uncompressed size
        name_length_parser,
        extra_length_parser,
        comment_length_parser,
        disk_number_parser,
        internal_attr_parser,
        external_attr_parser,
        offset_parser,
        variable_length_field(name_length_parser), // File name
        variable_length_field(extra_length_parser), // Extra field
        variable_length_field(comment_length_parser), // File comment
        NULL
    );
}

// End of Central Directory Record parser
HParser *end_of_central_directory_record_parser() {
    return h_sequence(
        signature_parser,
        disk_number_parser,
        disk_number_parser, // Disk where central directory starts
        h_uint16_le(), // Number of central directory records on this disk
        h_uint16_le(), // Total number of central directory records
        size_parser, // Size of central directory
        offset_parser, // Offset of start of central directory
        comment_length_parser,
        variable_length_field(comment_length_parser), // ZIP file comment
        NULL
    );
}

// Main ZIP parser
HParser *zip_parser() {
    return h_many(h_choice(
        local_file_header_parser(),
        central_directory_file_header_parser(),
        end_of_central_directory_record_parser(),
        NULL
    ));
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zipfile>\n", argv[0]);
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
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    initialize_parsers();
    HParseResult *result = h_parse(zip_parser(), data, file_size);
    if (result) {
        printf("ZIP file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ZIP file.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}