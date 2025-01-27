#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

// Structure to represent the ZIP file
typedef struct {
    uint32_t local_file_header_signature;
    uint16_t version_needed_to_extract;
    uint16_t general_purpose_bit_flag;
    uint16_t compression_method;
    uint16_t last_modification_time;
    uint16_t last_modification_date;
    uint32_t crc_32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
    uint16_t file_name_length;
    uint16_t extra_field_length;
    char* file_name;
    char* extra_field;
} zip_local_file_header_t;

// Structure to represent the ZIP file descriptor
typedef struct {
    uint32_t signature;
    uint32_t crc_32;
    uint32_t compressed_size;
    uint32_t uncompressed_size;
} zip_file_descriptor_t;

// Function to parse the ZIP file header
bool parse_zip_file_header(const uint8_t* data, size_t size) {
    if (size < 30) {
        return false;
    }

    const zip_local_file_header_t* header = (const zip_local_file_header_t*)data;

    if (header->local_file_header_signature != 0x04034b50) {
        return false;
    }

    // Check the compression method
    if (header->compression_method != 0 && header->compression_method != 8) {
        return false;
    }

    return true;
}

// Function to parse the ZIP file descriptor
bool parse_zip_file_descriptor(const uint8_t* data, size_t size) {
    if (size < 16) {
        return false;
    }

    const zip_file_descriptor_t* descriptor = (const zip_file_descriptor_t*)data;

    if (descriptor->signature != 0x08074b50) {
        return false;
    }

    return true;
}

int main() {
    const uint8_t zip_data[] = {
        0x50, 0x4b, 0x03, 0x04, 0x14, 0x00, 0x00, 0x00, 0x08, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    if (parse_zip_file_header(zip_data, sizeof(zip_data))) {
        // Process the ZIP file header
    }

    const uint8_t descriptor_data[] = {
        0x50, 0x4b, 0x07, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    };

    if (parse_zip_file_descriptor(descriptor_data, sizeof(descriptor_data))) {
        // Process the ZIP file descriptor
    }

    return 0;
}