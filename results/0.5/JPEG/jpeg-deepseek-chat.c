#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the JPEG header structure
typedef struct {
    uint8_t soi_marker[2];  // Start of Image marker (0xFF, 0xD8)
    uint8_t app0_marker[2]; // APP0 marker (0xFF, 0xE0)
    uint16_t app0_length;   // Length of APP0 segment
    uint8_t identifier[5];  // Identifier "JFIF\0"
    uint16_t version;       // JFIF version
    uint8_t units;          // Units for density
    uint16_t x_density;     // Horizontal density
    uint16_t y_density;     // Vertical density
    uint8_t x_thumbnail;    // Thumbnail horizontal pixels
    uint8_t y_thumbnail;    // Thumbnail vertical pixels
} JPEGHeader;

// Define the Hammer parser for the JPEG header
HParser *jpeg_header_parser() {
    return h_sequence(
        h_bytes(2, (uint8_t[]){0xFF, 0xD8}), // SOI marker
        h_bytes(2, (uint8_t[]){0xFF, 0xE0}), // APP0 marker
        h_uint16(),                          // APP0 length
        h_bytes(5, (uint8_t[]){'J', 'F', 'I', 'F', '\0'}), // Identifier
        h_uint16(),                          // JFIF version
        h_uint8(),                           // Units for density
        h_uint16(),                          // X density
        h_uint16(),                          // Y density
        h_uint8(),                           // X thumbnail
        h_uint8(),                           // Y thumbnail
        NULL
    );
}

// Main function to parse the JPEG header
int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
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

    HParser *parser = jpeg_header_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        JPEGHeader *header = (JPEGHeader *)result->ast;
        printf("JPEG Header Parsed Successfully:\n");
        printf("SOI Marker: 0x%02X 0x%02X\n", header->soi_marker[0], header->soi_marker[1]);
        printf("APP0 Marker: 0x%02X 0x%02X\n", header->app0_marker[0], header->app0_marker[1]);
        printf("APP0 Length: %u\n", header->app0_length);
        printf("Identifier: %s\n", header->identifier);
        printf("JFIF Version: %u\n", header->version);
        printf("Units: %u\n", header->units);
        printf("X Density: %u\n", header->x_density);
        printf("Y Density: %u\n", header->y_density);
        printf("X Thumbnail: %u\n", header->x_thumbnail);
        printf("Y Thumbnail: %u\n", header->y_thumbnail);

        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse JPEG header\n");
    }

    free(buffer);
    return 0;
}