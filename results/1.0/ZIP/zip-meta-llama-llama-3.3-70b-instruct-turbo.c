#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Define the ZIP file structure
typedef struct {
    char signature[4];
    uint16_t version;
    uint16_t flags;
    uint16_t compression_method;
    uint16_t last_modification_time;
    uint16_t last_modification_date;
    uint32_t crc;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t filename_length;
    uint16_t extra_field_length;
} zip_local_file_header_t;

typedef struct {
    char signature[4];
    uint16_t disk_number;
    uint16_t start_disk_number;
    uint16_t entries_in_directory_on_disk;
    uint16_t total_entries_in_directory;
    uint32_t directory_size;
    uint32_t directory_offset;
    uint16_t comment_length;
} zip_end_of_central_directory_t;

// Define the ZIP file parser
int parse_zip_file(const char* filename) {
    // Open the file
    FILE* file = fopen(filename, "rb");
    if (file == NULL) {
        return -1;
    }

    // Read the end of central directory record
    zip_end_of_central_directory_t eocd;
    fseek(file, -22, SEEK_END);
    fread(&eocd, sizeof(eocd), 1, file);

    // Calculate the offset of the central directory
    uint32_t central_directory_offset = eocd.directory_offset;

    // Read the central directory
    fseek(file, central_directory_offset, SEEK_SET);
    uint16_t num_entries = eocd.total_entries_in_directory;
    for (uint16_t i = 0; i < num_entries; i++) {
        // Read the central directory entry
        uint32_t entry_size = 46;
        char* entry_data = malloc(entry_size);
        fread(entry_data, entry_size, 1, file);

        // Extract the local file header offset
        uint32_t local_file_header_offset = *(uint32_t*)(entry_data + 42);

        // Read the local file header
        fseek(file, local_file_header_offset, SEEK_SET);
        zip_local_file_header_t local_file_header;
        fread(&local_file_header, sizeof(local_file_header), 1, file);

        // Print the filename
        char* filename_data = malloc(local_file_header.filename_length);
        fread(filename_data, local_file_header.filename_length, 1, file);
        printf("Filename: %s\n", filename_data);

        free(filename_data);
        free(entry_data);
    }

    fclose(file);
    return 0;
}

int main() {
    parse_zip_file("example.zip");
    return 0;
}