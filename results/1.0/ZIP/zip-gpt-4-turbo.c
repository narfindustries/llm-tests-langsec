#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the magic numbers
#define LOCAL_FILE_HEADER_SIG 0x04034b50
#define CENTRAL_DIRECTORY_FILE_HEADER_SIG 0x02014b50
#define END_OF_CENTRAL_DIRECTORY_RECORD_SIG 0x06054b50

// Parser for file names
HParser *parse_filename() {
    return h_length_value(h_int16(), h_uint8());
}

// Helper function to parse variable length data
HParser *parse_extra_field() {
    return h_length_value(h_int16(), h_arbitrary());  // h_arbitrary allows for any data
}

// Parser for ZIP file Local File Header
HParser *parse_local_file_header() {
    return h_sequence(
        h_int32(), // Signature
        h_int16(), // Version needed to extract
        h_bits(16, false), // General purpose bit flag
        h_int16(), // Compression method
        h_int16(), // Last modification time
        h_int16(), // Last modification date
        h_int32(), // CRC-32
        h_int32(), // Compressed size
        h_int32(), // Uncompressed size
        h_length_value(h_int16(), parse_filename()), // File name
        h_optional(parse_extra_field()), // Extra field
        NULL
    );
}

// Parser for ZIP file Central Directory File Header
HParser *parse_central_directory_file_header() {
    return h_sequence(
        h_int32(), // Signature
        h_int16(), // Version made by
        h_int16(), // Version needed to extract
        h_int16(), // General purpose bit flag
        h_int16(), // Compression method
        h_int16(), // Last modification time
        h_int16(), // Last modification date
        h_int32(), // CRC-32
        h_int32(), // Compressed size
        h_int32(), // Uncompressed size
        h_int16(), // Disk number start
        h_int16(), // Internal file attributes
        h_int32(), // External file attributes
        h_int32(), // Relative offset of local header
        h_length_value(h_int16(), parse_filename()), // File name
        h_optional(parse_extra_field()), // Extra field
        h_optional(parse_extra_field()), // File comment
        NULL
    );
}

// Parser for ZIP file End of Central Directory Record
HParser *parse_end_of_central_dir_record() {
    return h_sequence(
        h_int32(), // Signature
        h_int16(), // Number of this disk
        h_int16(), // Disk where central directory starts
        h_int16(), // Number of central directory records on this disk
        h_int16(), // Total number of central directory records
        h_int32(), // Size of central directory
        h_int32(), // Offset of start of central directory
        h_length_value(h_int16(), parse_extra_field()), // ZIP file comment
        NULL
    );
}

// Constructing the full parser
HParser *create_zip_parser() {
    return h_sequence(
        h_many1(parse_local_file_header()),
        h_many1(parse_central_directory_file_header()),
        parse_end_of_central_dir_record(),
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    FILE *zip_file = fopen(argv[1], "rb");
    if (!zip_file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(zip_file, 0, SEEK_END);
    long file_size = ftell(zip_file);
    rewind(zip_file);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(zip_file);
        exit(EXIT_FAILURE);
    }

    fread(buffer, 1, file_size, zip_file);
    fclose(zip_file);

    HParser *zip_parser = create_zip_parser();
    HParseResult *result = h_parse(zip_parser, buffer, file_size);

    if (result) {
        printf("Parse successful!\n");
    } else {
        printf("Parse failed!\n");
    }

    h_parse_result_free(result);
    h_parser_unref(zip_parser);
    free(buffer);

    return EXIT_SUCCESS;
}