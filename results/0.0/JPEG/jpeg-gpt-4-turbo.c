#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// JPEG Markers
#define SOI 0xFFD8
#define EOI 0xFFD9
#define SOF0 0xFFC0
#define SOS 0xFFDA
#define DQT 0xFFDB
#define DHT 0xFFC4
#define APP0 0xFFE0
#define COM 0xFFFE

// Parser declarations
HParser *byte;
HParser *two_bytes;
HParser *soi_parser;
HParser *eoi_parser;
HParser *sof0_parser;
HParser *sos_parser;
HParser *dqt_parser;
HParser *dht_parser;
HParser *app0_parser;
HParser *com_parser;
HParser *frame_component;
HParser *scan_component;

void init_parsers() {
    byte = h_uint8();
    two_bytes = h_bits(16, false);

    soi_parser = h_bits(16, false);
    eoi_parser = h_bits(16, false);

    frame_component = h_sequence(byte, byte, byte, NULL);
    scan_component = h_sequence(byte, byte, NULL);

    sof0_parser = h_sequence(
        h_bits(16, false),
        two_bytes, // Length
        byte,      // Precision
        two_bytes, // Height
        two_bytes, // Width
        h_length_value(byte, h_many(frame_component)), // Components
        NULL
    );

    sos_parser = h_sequence(
        h_bits(16, false),
        two_bytes, // Length
        h_length_value(byte, h_many(scan_component)), // Components
        byte, // Start of spectral selection
        byte, // End of spectral selection
        byte, // Successive approximation
        NULL
    );

    dqt_parser = h_sequence(
        h_bits(16, false),
        two_bytes, // Length
        h_many(h_sequence(byte, h_many1(byte), NULL)), // Quantization tables
        NULL
    );

    dht_parser = h_sequence(
        h_bits(16, false),
        two_bytes, // Length
        h_many(h_sequence(byte, h_many1(byte), h_many1(byte), NULL)), // Huffman tables
        NULL
    );

    app0_parser = h_sequence(
        h_bits(16, false),
        two_bytes, // Length
        h_many1(byte), // Data
        NULL
    );

    com_parser = h_sequence(
        h_bits(16, false),
        two_bytes, // Length
        h_many1(byte), // Comment data
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, size, file) != size) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    init_parsers();

    HParser *jpeg_parser = h_sequence(
        soi_parser,
        h_many(h_choice(sof0_parser, sos_parser, dqt_parser, dht_parser, app0_parser, com_parser, NULL)),
        eoi_parser,
        NULL
    );

    HParseResult *result = h_parse(jpeg_parser, data, size);
    if (result) {
        printf("JPEG parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse JPEG.\n");
    }

    h_parse_result_free(result);
    free(data);

    return 0;
}