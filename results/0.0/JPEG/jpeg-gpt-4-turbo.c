#include <hammer/hammer.h>
#include <hammer/glue.h>

// JPEG Markers
#define SOI 0xD8
#define EOI 0xD9
#define SOS 0xDA
#define APP0 0xE0
#define DQT 0xDB
#define SOF0 0xC0
#define DHT 0xC4

// Helper to define a JPEG marker
static HParser *jpeg_marker(uint8_t marker) {
    return h_sequence(h_uint8(), h_ch(marker), NULL);
}

// Define Quantization Table (DQT)
static HParser *quantization_table() {
    return h_sequence(
        jpeg_marker(DQT),
        h_uint16(),
        h_many1(h_sequence(h_bits(4, false), h_bits(4, false), h_blob(64), NULL)),
        NULL
    );
}

// Define Huffman Table (DHT)
static HParser *huffman_table() {
    return h_sequence(
        jpeg_marker(DHT),
        h_uint16(),
        h_many1(h_sequence(h_bits(3, false), h_bits(1, false), h_bits(4, false), h_blob(16), h_greedy_bytes(), NULL)),
        NULL
    );
}

// Define Start of Frame (SOF0)
static HParser *start_of_frame() {
    return h_sequence(
        jpeg_marker(SOF0),
        h_uint16(),
        h_uint8(),
        h_uint16(),
        h_uint16(),
        h_uint8(),
        h_many1(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL)),
        NULL
    );
}

// Define Start of Scan (SOS)
static HParser *start_of_scan() {
    return h_sequence(
        jpeg_marker(SOS),
        h_uint16(),
        h_uint8(),
        h_many1(h_sequence(h_uint8(), h_uint8(), NULL)),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        NULL
    );
}

// Define Application Segment (APP0)
static HParser *app0_segment() {
    return h_sequence(
        jpeg_marker(APP0),
        h_uint16(),
        h_string("JFIF", 5),
        h_uint8(),
        h_uint16(),
        h_uint16(),
        h_uint8(),
        h_uint8(),
        h_greedy_bytes(),
        NULL
    );
}

// Define the structure of a JPEG file
static HParser *jpeg_file() {
    return h_sequence(
        jpeg_marker(SOI),
        h_many(h_choice(app0_segment(), quantization_table(), huffman_table(), start_of_frame(), NULL)),
        start_of_scan(),
        h_greedy_bytes(), // Image data
        jpeg_marker(EOI),
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *parser = jpeg_file();
    HParseResult *result = h_parse(parser, (const uint8_t *)input_data, input_length);
    if (result) {
        printf("JPEG parsed successfully.\n");
    } else {
        printf("Failed to parse JPEG.\n");
    }
    h_parse_result_free(result);
    h_parser_free(parser);
    return 0;
}