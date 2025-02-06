#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// JPEG markers
#define SOI  0xFFD8
#define EOI  0xFFD9
#define SOS  0xFFDA
#define DQT  0xFFDB
#define DHT  0xFFC4
#define SOF0 0xFFC0
#define APP0 0xFFE0
#define COM  0xFFFE

// JPEG segment parsers
static HParser *jpeg_soi;
static HParser *jpeg_eoi;
static HParser *jpeg_sof0;
static HParser *jpeg_sos;
static HParser *jpeg_dqt;
static HParser *jpeg_dht;
static HParser *jpeg_app0;
static HParser *jpeg_com;
static HParser *jpeg_segment;

// Function to build parsers for different JPEG segments
void init_parsers() {
    HParser *marker = h_uint16();

    jpeg_soi = h_sequence(h_uint16(), h_end_p(), NULL);
    jpeg_eoi = h_sequence(h_uint16(), h_end_p(), NULL);

    jpeg_sof0 = h_sequence(
        marker,
        h_uint16(), // frame header length
        h_uint8(),  // precision
        h_uint16(), // image height
        h_uint16(), // image width
        h_uint8(),  // number of components
        h_many(h_sequence(
            h_uint8(), // component ID
            h_uint8(), // sampling factors
            h_uint8(), // quantization table ID
            NULL
        )),
        NULL
    );

    jpeg_sos = h_sequence(
        marker,
        h_uint16(), // scan header length
        h_uint8(),  // number of components in scan
        h_many(h_sequence(
            h_uint8(), // component ID
            h_uint8(), // Huffman table selector
            NULL
        )),
        h_bits(8, false), // start of spectral selection
        h_bits(8, false), // end of spectral selection
        h_bits(8, false), // successive approximation
        NULL
    );

    jpeg_dqt = h_sequence(
        marker,
        h_uint16(), // DQT length
        h_many(h_sequence(
            h_uint8(), // precision and identifier
            h_repeat_n(h_uint8(), 64), // quantization table
            NULL
        )),
        NULL
    );

    jpeg_dht = h_sequence(
        marker,
        h_uint16(), // DHT length
        h_many(h_sequence(
            h_uint8(), // class and identifier
            h_repeat_n(h_uint8(), 16), // number of symbols
            h_many1(h_uint8()), // symbols
            NULL
        )),
        NULL
    );

    jpeg_app0 = h_sequence(
        marker,
        h_uint16(), // APP0 length
        h_many(h_uint8()), // APP0 data
        NULL
    );

    jpeg_com = h_sequence(
        marker,
        h_uint16(), // COM length
        h_many(h_uint8()), // comment data
        NULL
    );

    jpeg_segment = h_choice(
        jpeg_soi, jpeg_eoi, jpeg_sof0, jpeg_sos, jpeg_dqt, jpeg_dht, jpeg_app0, jpeg_com,
        NULL
    );
}

int main(int argc, char *argv[]) {
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
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, length, file);
    fclose(file);

    init_parsers();

    HParseResult *result = h_parse(jpeg_segment, data, length);
    if (result) {
        printf("JPEG parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 4);
    } else {
        printf("Failed to parse JPEG.\n");
    }

    free(data);
    return 0;
}