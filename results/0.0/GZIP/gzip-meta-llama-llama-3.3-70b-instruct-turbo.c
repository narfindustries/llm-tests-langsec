#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

// Define the structure for the gzip file format
typedef struct {
    uint8_t id1;
    uint8_t id2;
    uint8_t cm;
    uint8_t flags;
    uint32_t mtime;
    uint8_t xflags;
    uint8_t os;
} gzip_header_t;

// Define the structure for the gzip file footer
typedef struct {
    uint32_t crc32;
    uint32_t isize;
} gzip_footer_t;

// Function to calculate the CRC32 checksum
uint32_t calculate_crc32(const uint8_t* data, size_t length) {
    uint32_t crc = 0xffffffff;
    for (size_t i = 0; i < length; i++) {
        crc = (crc >> 8) ^ (crc & 0xff) ^ data[i];
    }
    return ~crc;
}

// Function to compress data using the gzip algorithm
void gzip_compress(const uint8_t* input, size_t input_length, uint8_t** output, size_t* output_length) {
    // Initialize the gzip header
    gzip_header_t header;
    header.id1 = 0x1f;
    header.id2 = 0x8b;
    header.cm = 0x08; // DEFLATE compression method
    header.flags = 0x00; // No flags set
    header.mtime = 0x00000000; // Modification time (not used)
    header.xflags = 0x00; // No extra flags
    header.os = 0x03; // Operating system (Unix)

    // Initialize the gzip footer
    gzip_footer_t footer;
    footer.crc32 = calculate_crc32(input, input_length);
    footer.isize = input_length;

    // Calculate the output length
    *output_length = sizeof(header) + input_length + sizeof(footer);

    // Allocate memory for the output
    *output = malloc(*output_length);

    // Copy the gzip header
    memcpy(*output, &header, sizeof(header));

    // Copy the input data
    memcpy(*output + sizeof(header), input, input_length);

    // Copy the gzip footer
    memcpy(*output + sizeof(header) + input_length, &footer, sizeof(footer));
}

int main() {
    // Example usage
    const uint8_t input[] = "Hello, World!";
    size_t input_length = strlen((const char*)input);
    uint8_t* output;
    size_t output_length;

    gzip_compress(input, input_length, &output, &output_length);

    // Print the compressed output
    for (size_t i = 0; i < output_length; i++) {
        printf("%02x ", output[i]);
    }
    printf("\n");

    free(output);
    return 0;
}