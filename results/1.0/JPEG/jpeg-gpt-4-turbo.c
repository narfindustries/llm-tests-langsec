#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define JPEG markers
#define SOI_MARKER  0xFFD8
#define EOI_MARKER  0xFFD9
#define SOF0_MARKER 0xFFC0
#define DHT_MARKER  0xFFC4
#define DQT_MARKER  0xFFDB
#define SOS_MARKER  0xFFDA
#define DRI_MARKER  0xFFDD

// Define the parser for a JPEG file
static HParser *jpeg_parser = NULL;

// Function declarations for parsing specific structures
HParser *parse_marker() {
    return h_uint16();
}

HParser *parse_frame_header() {
    return h_sequence(
        h_uint16(), // Length of the header
        h_uint8(),  // Precision
        h_uint16(), // Height
        h_uint16(), // Width
        h_uint8(),  // Number of components
        NULL
    );
}

HParser *parse_huffman_table() {
    return h_length_value(
        h_uint16(),
        h_sequence(
            h_bits(4, false),   // Table class
            h_bits(4, false),   // Table ID
            h_many1(h_uint8()), // Number of symbols for each code length
            h_many1(h_uint8()), // Huffman Values
            NULL
        )
    );
}

HParser *parse_quantization_table() {
    return h_length_value(
        h_uint16(),
        h_sequence(
            h_bits(4, false),   // Precision
            h_bits(4, false),   // Table ID
            h_many1(h_uint8()), // Quantization values
            NULL
        )
    );
}

HParser *parse_scan_header() {
    return h_sequence(
        h_uint16(), // Length
        h_uint8(),  // Number of components in scan
        h_many1(h_sequence( // Component-specific information
            h_uint8(),        // Scan component selector
            h_bits(4, false), // DC entropy coding table selector
            h_bits(4, false), // AC entropy coding table selector
            NULL
        )),
        h_uint8(),  // Start of spectral or predictor selection
        h_uint8(),  // End of spectral selection
        h_uint8(),  // Successive approximation bit position high
        NULL
    );
}

void init_jpeg_parser() {
    jpeg_parser = h_sequence(
        parse_marker(), // SOI
        h_many(h_choice(
            h_sequence(h_ch_range(0xFF, 0xFF), h_ch_range(0xC0, 0xFE), parse_frame_header(), NULL),
            parse_huffman_table(),
            parse_quantization_table(),
            parse_scan_header(),
            h_end_p(),
            NULL
        )),
        parse_marker(), // EOI
        NULL
    );
}

int read_file(const char *filename, uint8_t **data, size_t *len) {
    FILE *file = fopen(filename, "rb");
    if (!file) return 0;
    fseek(file, 0, SEEK_END);
    *len = ftell(file);
    fseek(file, 0, SEEK_SET);
    *data = malloc(*len);
    if (!*data){
        fclose(file);
        return 0;
    }
    fread(*data, 1, *len, file);
    fclose(file);
    return 1;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <JPEG file>\n", argv[0]);
        return 1;
    }

    uint8_t *data;
    size_t data_len;
    if (!read_file(argv[1], &data, &data_len)) {
        fprintf(stderr, "Failed to read file %s\n", argv[1]);
        return 1;
    }

    init_jpeg_parser();

    HParseResult *result = h_parse(jpeg_parser, data, data_len);
    if (result) {
        printf("JPEG parsed successfully. Size: %zu bytes\n", data_len);
        h_pprint(stdout, result->ast, 0, 1);
    } else {
        printf("Failed to parse JPEG.\n");
    }

    free(data);
    h_parse_result_free(result);
    h_parser_free(jpeg_parser);

    return 0;
}