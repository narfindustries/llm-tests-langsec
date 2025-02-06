#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define ZIP_LOCAL_FILE_HEADER_SIGNATURE 0x04034b50
#define ZIP_CENTRAL_DIRECTORY_HEADER_SIGNATURE 0x02014b50
#define ZIP_END_OF_CENTRAL_DIRECTORY_SIGNATURE 0x06054b50
#define ZIP_DATA_DESCRIPTOR_SIGNATURE 0x08074b50

HParser *zip_local_file_header() {
    return h_sequence(
        h_int32(), // Local file header signature
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
        h_length_value(h_int16(), h_uint8()), // File name
        h_length_value(h_int16(), h_uint8()) // Extra field
    );
}

HParser *zip_central_directory_file_header() {
    return h_sequence(
        h_int32(), // Central file header signature
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
        h_length_value(h_int16(), h_uint8()), // File name
        h_length_value(h_int16(), h_uint8()), // Extra field
        h_length_value(h_int16(), h_uint8()) // File comment
    );
}

HParser *zip_end_of_central_directory_record() {
    return h_sequence(
        h_int32(), // End of central dir signature
        h_int16(), // Number of this disk
        h_int16(), // Disk where central directory starts
        h_int16(), // Number of central directory records on this disk
        h_int16(), // Total number of central directory records
        h_int32(), // Size of central directory
        h_int32(), // Offset of start of central directory
        h_int16(), // ZIP file comment length
        h_length_value(h_int16(), h_uint8()) // ZIP file comment
    );
}

HParser *zip_data_descriptor() {
    return h_sequence(
        h_int32(), // Data descriptor signature
        h_int32(), // CRC-32
        h_int32(), // Compressed size
        h_int32() // Uncompressed size
    );
}

HParser *zip_file() {
    return h_sequence(
        h_many(zip_local_file_header()),
        h_many(zip_central_directory_file_header()),
        zip_end_of_central_directory_record(),
        h_optional(zip_data_descriptor())
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
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

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(zip_file(), data, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}