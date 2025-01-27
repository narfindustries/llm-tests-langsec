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
} ZIP_Local_File_Header;

// Define the Hammer parser for the ZIP Local File Header
HParser *zip_local_file_header_parser() {
    return h_sequence(
        h_int32(),  // signature
        h_int16(),  // version_needed
        h_int16(),  // flags
        h_int16(),  // compression_method
        h_int16(),  // last_mod_time
        h_int16(),  // last_mod_date
        h_int32(),  // crc32
        h_int32(),  // compressed_size
        h_int32(),  // uncompressed_size
        h_int16(),  // file_name_length
        h_int16(),  // extra_field_length
        h_length_value(h_int16(), h_utf8()),  // file_name
        h_length_value(h_int16(), h_utf8()),  // extra_field
        NULL
    );
}

// Main function to parse the ZIP file
int main(int argc, char **argv) {
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

    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = zip_local_file_header_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ZIP file\n");
        free(buffer);
        return 1;
    }

    ZIP_Local_File_Header *header = (ZIP_Local_File_Header *)result->ast;
    printf("Signature: 0x%08X\n", header->signature);
    printf("Version Needed: %d\n", header->version_needed);
    printf("Flags: %d\n", header->flags);
    printf("Compression Method: %d\n", header->compression_method);
    printf("Last Modified Time: %d\n", header->last_mod_time);
    printf("Last Modified Date: %d\n", header->last_mod_date);
    printf("CRC32: 0x%08X\n", header->crc32);
    printf("Compressed Size: %d\n", header->compressed_size);
    printf("Uncompressed Size: %d\n", header->uncompressed_size);
    printf("File Name: %s\n", header->file_name);
    printf("Extra Field: %s\n", header->extra_field);

    h_parse_result_free(result);
    free(buffer);

    return 0;
}