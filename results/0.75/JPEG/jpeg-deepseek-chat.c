#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the JPEG parser using Hammer
HParser *jpeg_parser() {
    // SOI (Start of Image) marker
    HParser *soi = h_sequence(h_token((uint8_t[]){0xFF, 0xD8}, 2), NULL);

    // APP0 marker (optional, but common in JPEG files)
    HParser *app0 = h_sequence(
        h_token((uint8_t[]){0xFF, 0xE0}, 2), // APP0 marker
        h_uint16(), // Length of APP0 segment
        h_token((uint8_t[]){'J', 'F', 'I', 'F', 0x00}, 5), // Identifier
        h_uint16(), // Version
        h_uint8(),  // Density units
        h_uint16(), // X density
        h_uint16(), // Y density
        h_uint8(),  // Thumbnail width
        h_uint8(),  // Thumbnail height
        NULL
    );

    // DQT (Define Quantization Table) marker
    HParser *dqt = h_sequence(
        h_token((uint8_t[]){0xFF, 0xDB}, 2), // DQT marker
        h_uint16(), // Length of DQT segment
        h_many(h_uint8()), // Quantization table data
        NULL
    );

    // SOF0 (Start of Frame 0) marker
    HParser *sof0 = h_sequence(
        h_token((uint8_t[]){0xFF, 0xC0}, 2), // SOF0 marker
        h_uint16(), // Length of SOF0 segment
        h_uint8(),  // Precision
        h_uint16(), // Image height
        h_uint16(), // Image width
        h_uint8(),  // Number of components
        h_many(h_uint8()), // Component data
        NULL
    );

    // DHT (Define Huffman Table) marker
    HParser *dht = h_sequence(
        h_token((uint8_t[]){0xFF, 0xC4}, 2), // DHT marker
        h_uint16(), // Length of DHT segment
        h_many(h_uint8()), // Huffman table data
        NULL
    );

    // SOS (Start of Scan) marker
    HParser *sos = h_sequence(
        h_token((uint8_t[]){0xFF, 0xDA}, 2), // SOS marker
        h_uint16(), // Length of SOS segment
        h_uint8(),  // Number of components in scan
        h_many(h_uint8()), // Scan component data
        h_uint8(),  // Spectral selection start
        h_uint8(),  // Spectral selection end
        h_uint8(),  // Successive approximation
        NULL
    );

    // EOI (End of Image) marker
    HParser *eoi = h_token((uint8_t[]){0xFF, 0xD9}, 2);

    // JPEG file structure
    HParser *jpeg = h_sequence(
        soi,
        h_optional(app0),
        h_many(dqt),
        sof0,
        h_many(dht),
        sos,
        h_many(h_uint8()), // Encoded image data
        eoi,
        NULL
    );

    return jpeg;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <jpeg file>\n", argv[0]);
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

    HParser *parser = jpeg_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("JPEG file parsed successfully!\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse JPEG file.\n");
    }

    free(data);
    return 0;
}