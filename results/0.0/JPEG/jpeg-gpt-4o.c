#include <hammer/hammer.h>

HParser *create_jpeg_parser() {
    // Define JPEG markers
    HParser *marker = h_choice(
        h_uint8_val(0xD8), // SOI
        h_uint8_val(0xD9), // EOI
        h_uint8_val(0xDA), // SOS
        h_uint8_val(0xDB), // DQT
        h_uint8_val(0xC0), // SOF0
        h_uint8_val(0xC4), // DHT
        h_uint8_val(0xDD), // DRI
        h_uint8_val(0xFE), // COM
        NULL
    );

    // Define length-prefixed segment
    HParser *length_prefixed_segment = h_sequence(
        h_uint16_be(), // Length of the segment
        h_data(h_length_value(h_uint16_be(), -2)), // Segment data
        NULL
    );

    // Define JPEG segment
    HParser *segment = h_sequence(
        h_uint8_val(0xFF), // Marker prefix
        marker,            // Marker type
        h_optional(length_prefixed_segment), // Optional length-prefixed segment
        NULL
    );

    // Define JPEG file structure
    HParser *jpeg_parser = h_sequence(
        h_uint8_val(0xFF), // Start of Image (SOI) marker prefix
        h_uint8_val(0xD8), // SOI marker
        h_many(segment),   // Multiple segments
        h_uint8_val(0xFF), // End of Image (EOI) marker prefix
        h_uint8_val(0xD9), // EOI marker
        NULL
    );

    return jpeg_parser;
}

int main(int argc, char **argv) {
    HParser *jpeg_parser = create_jpeg_parser();
    HParseResult *result;
    const uint8_t *data;
    size_t length;

    // Load JPEG data from file or other source
    // data = ...;
    // length = ...;

    // Parse the JPEG data
    result = h_parse(jpeg_parser, data, length);

    if (result) {
        printf("JPEG parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse JPEG.\n");
    }

    h_parser_free(jpeg_parser);
    return 0;
}