#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// Header structure for ZIP file
typedef struct {
    uint32_t local_file_header_signature;
    uint16_t version_needed_to_extract;
    uint16_t flags;
    uint16_t compression_method;
    uint16_t last_mod_time;
    uint16_t last_mod_date;
    uint32_t crc32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t file_name_length;
    uint16_t extra_field_length;
} __attribute__((packed)) LocalFileHeader;

// Function to generate Hammer output
int hammer_output(LocalFileHeader* header) {
    if (header->local_file_header_signature != 0x04034b50) {
        printf("Invalid ZIP file signature\n");
        return 1;
    }
    // Process the ZIP file based on the header information
    printf("ZIP file signature: 0x%x\n", header->local_file_header_signature);
    printf("Version needed to extract: %u\n", header->version_needed_to_extract);
    printf("Flags: 0x%x\n", header->flags);
    printf("Compression method: %u\n", header->compression_method);
    printf("Last modified time: %u\n", header->last_mod_time);
    printf("Last modified date: %u\n", header->last_mod_date);
    printf("CRC32: 0x%x\n", header->crc32);
    printf("Compressed size: %u\n", header->compressed_size);
    printf("Uncompressed size: %u\n", header->uncompressed_size);
    printf("File name length: %u\n", header->file_name_length);
    printf("Extra field length: %u\n", header->extra_field_length);
    return 0;
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <zip_file>\n", argv[0]);
        return 1;
    }
    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open file: %s\n", argv[1]);
        return 1;
    }
    LocalFileHeader header;
    if (fread(&header, sizeof(LocalFileHeader), 1, file) != 1) {
        printf("Failed to read ZIP file header\n");
        fclose(file);
        return 1;
    }
    int result = hammer_output(&header);
    fclose(file);
    return result;
}