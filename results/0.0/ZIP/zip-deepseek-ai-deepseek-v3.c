#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

#define ZIP_SIGNATURE_LOCAL_FILE_HEADER 0x04034b50
#define ZIP_SIGNATURE_CENTRAL_DIRECTORY 0x02014b50
#define ZIP_SIGNATURE_END_OF_CENTRAL_DIR 0x06054b50
#define ZIP_SIGNATURE_DATA_DESCRIPTOR 0x08074b50

HParser *zip_local_file_header() {
    return h_sequence(
        h_int32(), // Signature
        h_int16(), // Version Needed to Extract
        h_int16(), // General Purpose Bit Flag
        h_int16(), // Compression Method
        h_int16(), // Last Mod File Time
        h_int16(), // Last Mod File Date
        h_int32(), // CRC-32
        h_int32(), // Compressed Size
        h_int32(), // Uncompressed Size
        h_int16(), // File Name Length
        h_int16(), // Extra Field Length
        h_length_value(h_int16(), h_uint8()), // File Name
        h_length_value(h_int16(), h_uint8()) // Extra Field
    );
}

HParser *zip_central_directory_file_header() {
    return h_sequence(
        h_int32(), // Signature
        h_int16(), // Version Made By
        h_int16(), // Version Needed to Extract
        h_int16(), // General Purpose Bit Flag
        h_int16(), // Compression Method
        h_int16(), // Last Mod File Time
        h_int16(), // Last Mod File Date
        h_int32(), // CRC-32
        h_int32(), // Compressed Size
        h_int32(), // Uncompressed Size
        h_int16(), // File Name Length
        h_int16(), // Extra Field Length
        h_int16(), // File Comment Length
        h_int16(), // Disk Number Start
        h_int16(), // Internal File Attributes
        h_int32(), // External File Attributes
        h_int32(), // Relative Offset of Local Header
        h_length_value(h_int16(), h_uint8()), // File Name
        h_length_value(h_int16(), h_uint8()), // Extra Field
        h_length_value(h_int16(), h_uint8()) // File Comment
    );
}

HParser *zip_end_of_central_directory_record() {
    return h_sequence(
        h_int32(), // Signature
        h_int16(), // Number of This Disk
        h_int16(), // Disk Where Central Directory Starts
        h_int16(), // Number of Central Directory Records on This Disk
        h_int16(), // Total Number of Central Directory Records
        h_int32(), // Size of Central Directory
        h_int32(), // Offset of Start of Central Directory
        h_int16(), // ZIP File Comment Length
        h_length_value(h_int16(), h_uint8()) // ZIP File Comment
    );
}

HParser *zip_data_descriptor() {
    return h_sequence(
        h_int32(), // Signature
        h_int32(), // CRC-32
        h_int32(), // Compressed Size
        h_int32() // Uncompressed Size
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

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = zip_file();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}