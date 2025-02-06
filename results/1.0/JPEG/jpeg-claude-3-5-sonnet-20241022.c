#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// JPEG marker parsers
HParser* make_marker(uint8_t code) {
    uint8_t marker[] = {0xFF, code};
    return h_token(marker, 2);
}

// Component parser
HParser* make_component_parser() {
    return h_sequence(
        h_uint8(), // Component ID
        h_bits(4, false), // Horizontal sampling
        h_bits(4, false), // Vertical sampling
        h_uint8(), // Quantization table ID
        NULL
    );
}

// Quantization table parser
HParser* make_dqt_parser() {
    return h_sequence(
        make_marker(0xDB),
        h_length_value(
            h_uint16(), 
            h_many(
                h_sequence(
                    h_bits(4, false), // Precision
                    h_bits(4, false), // Table ID
                    h_repeat_n(h_uint8(), 64), // Table values
                    NULL
                )
            )
        ),
        NULL
    );
}

// Huffman table parser
HParser* make_dht_parser() {
    return h_sequence(
        make_marker(0xC4),
        h_length_value(
            h_uint16(),
            h_many(
                h_sequence(
                    h_bits(4, false), // Table class
                    h_bits(4, false), // Table ID
                    h_repeat_n(h_uint8(), 16), // Number of codes
                    h_many(h_uint8()), // Values
                    NULL
                )
            )
        ),
        NULL
    );
}

// Start of Frame parser
HParser* make_sof_parser(uint8_t type) {
    return h_sequence(
        make_marker(type),
        h_length_value(
            h_uint16(),
            h_sequence(
                h_uint8(),  // Precision
                h_uint16(), // Height
                h_uint16(), // Width
                h_uint8(),  // Number of components
                h_many(make_component_parser()),
                NULL
            )
        ),
        NULL
    );
}

// Start of Scan parser
HParser* make_sos_parser() {
    return h_sequence(
        make_marker(0xDA),
        h_length_value(
            h_uint16(),
            h_sequence(
                h_uint8(), // Number of components
                h_many(
                    h_sequence(
                        h_uint8(), // Component ID
                        h_bits(4, false), // DC table
                        h_bits(4, false), // AC table
                        NULL
                    )
                ),
                h_uint8(), // Start of spectral
                h_uint8(), // End of spectral
                h_bits(4, false), // Successive approx high
                h_bits(4, false), // Successive approx low
                NULL
            )
        ),
        NULL
    );
}

// APP segment parser
HParser* make_app_parser(uint8_t n) {
    return h_sequence(
        make_marker(0xE0 + n),
        h_length_value(
            h_uint16(),
            h_many(h_uint8())
        ),
        NULL
    );
}

// Comment parser
HParser* make_com_parser() {
    return h_sequence(
        make_marker(0xFE),
        h_length_value(
            h_uint16(),
            h_many(h_uint8())
        ),
        NULL
    );
}

// DRI parser
HParser* make_dri_parser() {
    return h_sequence(
        make_marker(0xDD),
        h_length_value(
            h_uint16(),
            h_uint16() // Restart interval
        ),
        NULL
    );
}

// Main JPEG parser
HParser* make_jpeg_parser() {
    return h_sequence(
        make_marker(0xD8), // SOI
        h_many(
            h_choice(
                make_dqt_parser(),
                make_dht_parser(),
                make_sof_parser(0xC0), // SOF0
                make_sof_parser(0xC1), // SOF1
                make_sof_parser(0xC2), // SOF2
                make_sos_parser(),
                make_app_parser(0),    // APP0
                make_app_parser(1),    // APP1
                make_app_parser(2),    // APP2
                make_com_parser(),
                make_dri_parser(),
                NULL
            )
        ),
        make_marker(0xD9), // EOI
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <jpeg_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
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

    HParser* jpeg_parser = make_jpeg_parser();
    HParseResult* result = h_parse(jpeg_parser, buffer, size);

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