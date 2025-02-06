#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Helper function for length-prefixed data
HParser *h_length_prefixed_u16(HParser *value_parser) {
    return h_bind(h_uint16(),
                  (HParser *(*)(HAllocator *a, const HParsedToken *t, void *user)) h_repeat_n,
                  value_parser);
}

// Define ZIP structures

// Local File Header
HParser *local_file_header_parser() {
    return h_sequence(
        h_token("\x50\x4b\x03\x04", 4), // Signature
        h_uint16(), // Version Needed to Extract
        h_uint16(), // General Purpose Bit Flag
        h_uint16(), // Compression Method
        h_uint16(), // Last Mod File Time
        h_uint16(), // Last Mod File Date
        h_uint32(), // CRC-32
        h_uint32(), // Compressed Size
        h_uint32(), // Uncompressed Size
        h_length_prefixed_u16(h_uint8()), // File Name
        h_length_prefixed_u16(h_uint8()), // Extra Field
        NULL);
}

// Central Directory File Header
HParser *central_directory_file_header_parser() {
    return h_sequence(
        h_token("\x50\x4b\x01\x02", 4), // Signature
        h_uint16(), // Version Made By
        h_uint16(), // Version Needed to Extract
        h_uint16(), // General Purpose Bit Flag
        h_uint16(), // Compression Method
        h_uint16(), // Last Mod File Time
        h_uint16(), // Last Mod File Date
        h_uint32(), // CRC-32
        h_uint32(), // Compressed Size
        h_uint32(), // Uncompressed Size
        h_length_prefixed_u16(h_uint8()), // File Name
        h_length_prefixed_u16(h_uint8()), // Extra Field
        h_length_prefixed_u16(h_uint8()), // File Comment
        h_uint16(), // Disk Number Start
        h_uint16(), // Internal File Attributes
        h_uint32(), // External File Attributes
        h_uint32(), // Relative Offset of Local Header
        NULL);
}

// End of Central Directory Record
HParser *end_of_central_directory_record_parser() {
    return h_sequence(
        h_token("\x50\x4b\x05\x06", 4), // Signature
        h_uint16(), // Number of This Disk
        h_uint16(), // Disk Where Central Directory Starts
        h_uint16(), // Number of Central Directory Records on This Disk
        h_uint16(), // Total Number of Central Directory Records
        h_uint32(), // Size of Central Directory
        h_uint32(), // Offset of Start of Central Directory
        h_length_prefixed_u16(h_uint8()), // ZIP File Comment
        NULL);
}

// ZIP Parser
HParser *zip_parser() {
    return h_many(h_choice(local_file_header_parser(),
                           central_directory_file_header_parser(),
                           end_of_central_directory_record_parser(),
                           NULL));
}

// Main function to parse ZIP file
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }
    fread(data, 1, length, file);
    fclose(file);

    HParser *parser = zip_parser();
    HParseResult *result = h_parse(parser, data, length);

    if (result) {
        printf("ZIP file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ZIP file.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}