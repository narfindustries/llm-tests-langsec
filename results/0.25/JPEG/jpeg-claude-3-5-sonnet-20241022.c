#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Marker definitions
static const uint8_t SOI_MARKER[] = {0xFF, 0xD8};
static const uint8_t EOI_MARKER[] = {0xFF, 0xD9};
static const uint8_t SOS_MARKER[] = {0xFF, 0xDA};
static const uint8_t DHT_MARKER[] = {0xFF, 0xC4};
static const uint8_t DQT_MARKER[] = {0xFF, 0xDB};
static const uint8_t DRI_MARKER[] = {0xFF, 0xDD};
static const uint8_t APP0_MARKER[] = {0xFF, 0xE0};
static const uint8_t JFIF_ID[] = {'J', 'F', 'I', 'F', 0x00};

// Helper parsers
static HParser* marker_parser(const uint8_t* marker) {
    return h_sequence(h_ch(marker[0]), h_ch(marker[1]), NULL);
}

static HParser* length_value_parser(HParser* value_parser) {
    return h_length_value(h_uint16(), value_parser);
}

// Component parsers
static HParser* component_spec_parser(void) {
    return h_sequence(
        h_uint8(), // Component ID
        h_bits(4, false), // Horizontal sampling
        h_bits(4, false), // Vertical sampling
        h_uint8(), // Quantization table selector
        NULL
    );
}

static HParser* scan_component_spec_parser(void) {
    return h_sequence(
        h_uint8(), // Component selector
        h_bits(4, false), // DC entropy coding selector
        h_bits(4, false), // AC entropy coding selector
        NULL
    );
}

// Table parsers
HParser* init_huffman_table_parser(void) {
    return h_sequence(
        marker_parser(DHT_MARKER),
        length_value_parser(
            h_many1(h_sequence(
                h_bits(4, false), // Table class
                h_bits(4, false), // Table destination ID
                h_repeat_n(h_uint8(), 16), // Number of symbols per length
                h_many1(h_uint8()), // Symbols
                NULL
            ))
        ),
        NULL
    );
}

HParser* init_quantization_table_parser(void) {
    return h_sequence(
        marker_parser(DQT_MARKER),
        length_value_parser(
            h_many1(h_sequence(
                h_bits(4, false), // Precision
                h_bits(4, false), // Table ID
                h_repeat_n(h_uint8(), 64), // Table values
                NULL
            ))
        ),
        NULL
    );
}

// Frame header parser
HParser* init_frame_header_parser(void) {
    return h_sequence(
        h_uint8(), // Precision
        h_uint16(), // Height
        h_uint16(), // Width
        h_uint8(), // Number of components
        h_many1(component_spec_parser()),
        NULL
    );
}

// Scan header parser
HParser* init_scan_header_parser(void) {
    return h_sequence(
        marker_parser(SOS_MARKER),
        length_value_parser(
            h_sequence(
                h_uint8(), // Number of components
                h_many1(scan_component_spec_parser()),
                h_uint8(), // Start of spectral selection
                h_uint8(), // End of spectral selection
                h_bits(4, false), // Successive approximation high
                h_bits(4, false), // Successive approximation low
                NULL
            )
        ),
        NULL
    );
}

// JFIF segment parser
HParser* init_jfif_parser(void) {
    return h_sequence(
        marker_parser(APP0_MARKER),
        length_value_parser(
            h_sequence(
                h_token(JFIF_ID, 5),
                h_uint8(), // Major version
                h_uint8(), // Minor version
                h_uint8(), // Units
                h_uint16(), // X density
                h_uint16(), // Y density
                h_uint8(), // Thumbnail width
                h_uint8(), // Thumbnail height
                h_many(h_uint8()), // Thumbnail data
                NULL
            )
        ),
        NULL
    );
}

// Main JPEG parser
HParser* init_jpeg_parser(void) {
    return h_sequence(
        marker_parser(SOI_MARKER),
        h_many(h_choice(
            init_jfif_parser(),
            init_huffman_table_parser(),
            init_quantization_table_parser(),
            init_scan_header_parser(),
            NULL
        )),
        marker_parser(EOI_MARKER),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, size, fp) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    HParser *jpeg_parser = init_jpeg_parser();
    HParseResult *result = h_parse(jpeg_parser, buffer, size);

    if (result) {
        printf("Successfully parsed JPEG file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse JPEG file\n");
    }

    free(buffer);
    fclose(fp);
    return 0;
}