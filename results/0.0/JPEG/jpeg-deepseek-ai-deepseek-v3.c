#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *byte_parser;
HParser *uint16_parser;
HParser *marker_parser;
HParser *sof_parser;
HParser *dht_parser;
HParser *dqt_parser;
HParser *sos_parser;
HParser *app_parser;
HParser *com_parser;
HParser *jpeg_parser;

void init_parsers() {
    // Define parsers for basic types
    byte_parser = h_uint8();
    uint16_parser = h_uint16();

    // Define parser for JPEG markers
    marker_parser = h_sequence(h_uint8(), h_uint8(), NULL);

    // Define parser for SOF segment
    sof_parser = h_sequence(
        uint16_parser, // Length
        byte_parser,   // Precision
        uint16_parser, // Height
        uint16_parser, // Width
        byte_parser,   // Number of components
        h_many(h_sequence(byte_parser, byte_parser, byte_parser, NULL)), // Component parameters
        NULL
    );

    // Define parser for DHT segment
    dht_parser = h_sequence(
        uint16_parser, // Length
        byte_parser,   // Table class and destination identifier
        h_many(byte_parser), // Huffman code lengths
        h_many(byte_parser), // Huffman code values
        NULL
    );

    // Define parser for DQT segment
    dqt_parser = h_sequence(
        uint16_parser, // Length
        byte_parser,   // Table precision and destination identifier
        h_many(byte_parser), // Quantization table values
        NULL
    );

    // Define parser for SOS segment
    sos_parser = h_sequence(
        uint16_parser, // Length
        byte_parser,   // Number of components
        h_many(h_sequence(byte_parser, byte_parser, NULL)), // Component parameters
        byte_parser,   // Spectral selection start
        byte_parser,   // Spectral selection end
        byte_parser,   // Successive approximation
        NULL
    );

    // Define parser for APP segment
    app_parser = h_sequence(
        uint16_parser, // Length
        h_many(byte_parser), // Application-specific data
        NULL
    );

    // Define parser for COM segment
    com_parser = h_sequence(
        uint16_parser, // Length
        h_many(byte_parser), // Comment data
        NULL
    );

    // Define parser for JPEG file
    jpeg_parser = h_sequence(
        h_choice(marker_parser, NULL), // SOI marker
        h_many(h_choice(sof_parser, dht_parser, dqt_parser, sos_parser, app_parser, com_parser, NULL)), // Segments
        h_choice(marker_parser, NULL), // EOI marker
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
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

    init_parsers();
    HParseResult *result = h_parse(jpeg_parser, buffer, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}