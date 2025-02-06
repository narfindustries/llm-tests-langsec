#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
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

    HParser *uint16_parser = h_uint16();
    HParser *uint32_parser = h_uint32();
    HParser *uint64_parser = h_uint64();

    HParser *local_file_header_parser = h_sequence(
        h_uint32(), // Signature
        uint16_parser, // Version Needed to Extract
        uint16_parser, // General Purpose Bit Flag
        uint16_parser, // Compression Method
        uint16_parser, // Last Mod File Time
        uint16_parser, // Last Mod File Date
        uint32_parser, // CRC-32
        uint32_parser, // Compressed Size
        uint32_parser, // Uncompressed Size
        uint16_parser, // File Name Length
        uint16_parser, // Extra Field Length
        h_length_value(uint16_parser, h_many(h_uint8())), // File Name
        h_length_value(uint16_parser, h_many(h_uint8())), // Extra Field
        NULL
    );

    HParser *data_descriptor_parser = h_sequence(
        h_optional(h_uint32()), // Optional Signature
        uint32_parser, // CRC-32
        uint32_parser, // Compressed Size
        uint32_parser, // Uncompressed Size
        NULL
    );

    HParser *central_directory_file_header_parser = h_sequence(
        h_uint32(), // Signature
        uint16_parser, // Version Made By
        uint16_parser, // Version Needed to Extract
        uint16_parser, // General Purpose Bit Flag
        uint16_parser, // Compression Method
        uint16_parser, // Last Mod File Time
        uint16_parser, // Last Mod File Date
        uint32_parser, // CRC-32
        uint32_parser, // Compressed Size
        uint32_parser, // Uncompressed Size
        uint16_parser, // File Name Length
        uint16_parser, // Extra Field Length
        uint16_parser, // File Comment Length
        uint16_parser, // Disk Number Start
        uint16_parser, // Internal File Attributes
        uint32_parser, // External File Attributes
        uint32_parser, // Relative Offset of Local Header
        h_length_value(uint16_parser, h_many(h_uint8())), // File Name
        h_length_value(uint16_parser, h_many(h_uint8())), // Extra Field
        h_length_value(uint16_parser, h_many(h_uint8())), // File Comment
        NULL
    );

    HParser *end_of_central_directory_record_parser = h_sequence(
        h_uint32(), // Signature
        uint16_parser, // Disk Number
        uint16_parser, // Disk Number with Start of Central Directory
        uint16_parser, // Number of Entries on This Disk
        uint16_parser, // Total Number of Entries
        uint32_parser, // Size of Central Directory
        uint32_parser, // Offset of Start of Central Directory
        uint16_parser, // ZIP File Comment Length
        h_length_value(uint16_parser, h_many(h_uint8())), // ZIP File Comment
        NULL
    );

    HParseResult *result = h_parse(local_file_header_parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse file\n");
        free(buffer);
        return 1;
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}