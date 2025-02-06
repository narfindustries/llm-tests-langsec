#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define constants for signatures
#define LOCAL_FILE_HEADER_SIG 0x04034b50
#define CENTRAL_DIR_HEADER_SIG 0x02014b50
#define END_OF_CENTRAL_DIR_SIG 0x06054b50

// Function prototypes
void parse_zip(const char *filename);

// Hammer parsers for ZIP file structure
HParser *local_file_header;
HParser *central_directory_file_header;
HParser *end_of_central_directory_record;

void init_parsers() {
    // Local File Header
    local_file_header = h_sequence(
        h_int32(), // Signature
        h_int16(), // Version needed to extract
        h_int16(), // General purpose bit flag
        h_int16(), // Compression method
        h_int16(), // Last mod file time
        h_int16(), // Last mod file date
        h_int32(), // CRC-32
        h_int32(), // Compressed size
        h_int32(), // Uncompressed size
        h_int16(), // File name length
        h_int16(), // Extra field length
        h_length_value(h_uint16(), h_uint8()), // File name
        h_length_value(h_uint16(), h_uint8()), // Extra field
        NULL
    );

    // Central Directory File Header
    central_directory_file_header = h_sequence(
        h_int32(), // Signature
        h_int16(), // Version made by
        h_int16(), // Version needed to extract
        h_int16(), // General purpose bit flag
        h_int16(), // Compression method
        h_int16(), // Last mod file time
        h_int16(), // Last mod file date
        h_int32(), // CRC-32
        h_int32(), // Compressed size
        h_int32(), // Uncompressed size
        h_int16(), // File name length
        h_int16(), // Extra field length
        h_int16(), // File comment length
        h_int16(), // Disk number start
        h_int16(), // Internal file attributes
        h_int32(), // External file attributes
        h_int32(), // Relative offset of local header
        h_length_value(h_uint16(), h_uint8()), // File name
        h_length_value(h_uint16(), h_uint8()), // Extra field
        h_length_value(h_uint16(), h_uint8()), // File comment
        NULL
    );

    // End of Central Directory Record
    end_of_central_directory_record = h_sequence(
        h_int32(), // Signature
        h_int16(), // Number of this disk
        h_int16(), // Disk where central directory starts
        h_int16(), // Number of central directory records on this disk
        h_int16(), // Total number of central directory records
        h_int32(), // Size of central directory
        h_int32(), // Offset of start of central directory
        h_int16(), // ZIP file comment length
        h_length_value(h_uint16(), h_uint8()), // ZIP file comment
        NULL
    );
}

void parse_zip(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        fprintf(stderr, "Failed to open file: %s\n", filename);
        return;
    }

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        fprintf(stderr, "Failed to allocate memory for file data\n");
        fclose(file);
        return;
    }

    fread(data, 1, size, file);
    fclose(file);

    HParseResult *result = h_parse(local_file_header, data, size);
    if (result) {
        printf("Parsed Local File Header successfully.\n");
    } else {
        fprintf(stderr, "Failed to parse Local File Header.\n");
    }

    free(data);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }

    init_parsers();
    parse_zip(argv[1]);

    return 0;
}