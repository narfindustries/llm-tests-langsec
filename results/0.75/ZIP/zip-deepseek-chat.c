#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the ZIP file structure
typedef struct {
    uint32_t signature;
    uint16_t version_needed;
    uint16_t flags;
    uint16_t compression_method;
    uint16_t last_mod_time;
    uint16_t last_mod_date;
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t file_name_length;
    uint16_t extra_field_length;
    char *file_name;
    char *extra_field;
    uint8_t *file_data;
} ZIPLocalFileHeader;

// Define the Hammer parser for the ZIP file
HParser *zip_parser() {
    H_RULE(signature, h_int32());
    H_RULE(version_needed, h_int16());
    H_RULE(flags, h_int16());
    H_RULE(compression_method, h_int16());
    H_RULE(last_mod_time, h_int16());
    H_RULE(last_mod_date, h_int16());
    H_RULE(crc32, h_int32());
    H_RULE(compressed_size, h_int32());
    H_RULE(uncompressed_size, h_int32());
    H_RULE(file_name_length, h_int16());
    H_RULE(extra_field_length, h_int16());
    H_RULE(file_name, h_length_value(h_int16(), h_utf8()));
    H_RULE(extra_field, h_length_value(h_int16(), h_utf8()));
    H_RULE(file_data, h_length_value(h_int32(), h_uint8()));

    return h_sequence(
        signature,
        version_needed,
        flags,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name_length,
        extra_field_length,
        file_name,
        extra_field,
        file_data,
        NULL
    );
}

// Main function to parse the ZIP file
int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <zip_file>\n", argv[0]);
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

    uint8_t *file_data = (uint8_t *)malloc(file_size);
    if (!file_data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(file_data, 1, file_size, file);
    fclose(file);

    HParser *parser = zip_parser();
    HParseResult *result = h_parse(parser, file_data, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ZIP file\n");
        free(file_data);
        return 1;
    }

    ZIPLocalFileHeader *zip_header = (ZIPLocalFileHeader *)result->ast;
    printf("Parsed ZIP file successfully!\n");
    printf("Signature: 0x%08x\n", zip_header->signature);
    printf("Version Needed: %d\n", zip_header->version_needed);
    printf("Flags: %d\n", zip_header->flags);
    printf("Compression Method: %d\n", zip_header->compression_method);
    printf("Last Modified Time: %d\n", zip_header->last_mod_time);
    printf("Last Modified Date: %d\n", zip_header->last_mod_date);
    printf("CRC32: 0x%08x\n", zip_header->crc32);
    printf("Compressed Size: %d\n", zip_header->compressed_size);
    printf("Uncompressed Size: %d\n", zip_header->uncompressed_size);
    printf("File Name Length: %d\n", zip_header->file_name_length);
    printf("Extra Field Length: %d\n", zip_header->extra_field_length);
    printf("File Name: %s\n", zip_header->file_name);
    printf("Extra Field: %s\n", zip_header->extra_field);

    h_parse_result_free(result);
    free(file_data);

    return 0;
}