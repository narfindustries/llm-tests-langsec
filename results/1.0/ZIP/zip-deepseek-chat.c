#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    char *file_data;
} zip_local_file_header;

// Define the Hammer parser for the ZIP file
HParser *zip_parser() {
    HParser *signature = h_int32();
    HParser *version_needed = h_int16();
    HParser *flags = h_int16();
    HParser *compression_method = h_int16();
    HParser *last_mod_time = h_int16();
    HParser *last_mod_date = h_int16();
    HParser *crc32 = h_int32();
    HParser *compressed_size = h_int32();
    HParser *uncompressed_size = h_int32();
    HParser *file_name_length = h_int16();
    HParser *extra_field_length = h_int16();
    HParser *file_name = h_length_value(h_int16(), h_utf8());
    HParser *extra_field = h_length_value(h_int16(), h_utf8());
    HParser *file_data = h_length_value(h_int32(), h_utf8());

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
    if (argc != 2) {
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

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = zip_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ZIP file\n");
        free(buffer);
        return 1;
    }

    zip_local_file_header *header = (zip_local_file_header *)result->ast;
    printf("Signature: 0x%08X\n", header->signature);
    printf("Version Needed: %d\n", header->version_needed);
    printf("Flags: 0x%04X\n", header->flags);
    printf("Compression Method: %d\n", header->compression_method);
    printf("Last Modified Time: %d\n", header->last_mod_time);
    printf("Last Modified Date: %d\n", header->last_mod_date);
    printf("CRC32: 0x%08X\n", header->crc32);
    printf("Compressed Size: %d\n", header->compressed_size);
    printf("Uncompressed Size: %d\n", header->uncompressed_size);
    printf("File Name: %s\n", header->file_name);
    printf("Extra Field: %s\n", header->extra_field);

    free(buffer);
    h_parse_result_free(result);

    return 0;
}