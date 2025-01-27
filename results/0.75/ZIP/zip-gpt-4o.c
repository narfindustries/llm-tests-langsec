#include <hammer/hammer.h>

static HParser *zip_parser(void) {
    HParser *local_file_header = h_sequence(
        h_literally("\x50\x4B\x03\x04", 4), // Local file header signature
        h_uint16(), // Version needed to extract
        h_uint16(), // General purpose bit flag
        h_uint16(), // Compression method
        h_uint16(), // Last mod file time
        h_uint16(), // Last mod file date
        h_uint32(), // CRC-32
        h_uint32(), // Compressed size
        h_uint32(), // Uncompressed size
        h_uint16(), // File name length
        h_uint16(), // Extra field length
        NULL
    );

    HParser *central_directory_file_header = h_sequence(
        h_literally("\x50\x4B\x01\x02", 4), // Central directory file header signature
        h_uint16(), // Version made by
        h_uint16(), // Version needed to extract
        h_uint16(), // General purpose bit flag
        h_uint16(), // Compression method
        h_uint16(), // Last mod file time
        h_uint16(), // Last mod file date
        h_uint32(), // CRC-32
        h_uint32(), // Compressed size
        h_uint32(), // Uncompressed size
        h_uint16(), // File name length
        h_uint16(), // Extra field length
        h_uint16(), // File comment length
        h_uint16(), // Disk number start
        h_uint16(), // Internal file attributes
        h_uint32(), // External file attributes
        h_uint32(), // Relative offset of local header
        NULL
    );

    HParser *end_of_central_directory_record = h_sequence(
        h_literally("\x50\x4B\x05\x06", 4), // End of central directory signature
        h_uint16(), // Number of this disk
        h_uint16(), // Disk where central directory starts
        h_uint16(), // Number of central directory records on this disk
        h_uint16(), // Total number of central directory records
        h_uint32(), // Size of central directory (bytes)
        h_uint32(), // Offset of start of central directory, relative to start of archive
        h_uint16(), // Comment length
        NULL
    );

    HParser *zip_file = h_many1(
        h_choice(
            local_file_header,
            central_directory_file_header,
            end_of_central_directory_record,
            NULL
        )
    );

    return zip_file;
}

int main(int argc, char **argv) {
    HParser *parser = zip_parser();
    if (!parser) {
        fprintf(stderr, "Failed to create parser\n");
        return EXIT_FAILURE;
    }

    // Initialize input data (for testing purposes, replace this with actual data input)
    const uint8_t data[] = { /* ZIP file binary data */ };
    size_t data_length = sizeof(data) / sizeof(data[0]);

    HParseResult *result = h_parse(parser, data, data_length);
    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        h_parser_free(parser);
        return EXIT_FAILURE;
    }

    // Process result (for demonstration, we just print success)
    printf("Parsing succeeded!\n");

    h_parse_result_free(result);
    h_parser_free(parser);
    return EXIT_SUCCESS;
}