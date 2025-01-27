#include <hammer/hammer.h>
#include <hammer/glue.h>

// JPEG Header Parsing Specification
static HParser* jpeg_file;
static HParser* jpeg_header;
static HParser* jpeg_marker;
static HParser* jpeg_segments;

// Marker type definitions
static const uint8_t MARKER_SOI = 0xD8;   // Start of Image
static const uint8_t MARKER_EOI = 0xD9;   // End of Image
static const uint8_t MARKER_APP0 = 0xE0;  // JFIF Application Marker
static const uint8_t MARKER_SOF0 = 0xC0;  // Start of Frame (Baseline DCT)

static HParser* create_jpeg_parser() {
    // Marker parsing
    jpeg_marker = h_choice(
        h_bits(8, false),  // Generic marker byte
        h_end_p(),
        NULL
    );

    // JPEG Header Structure
    jpeg_header = h_sequence(
        h_token("\xFF\xD8", 2),  // SOI Marker
        h_many(h_choice(
            h_token("\xFF\xE0", 2),  // APP0 Marker 
            h_token("\xFF\xC0", 2),  // SOF0 Marker
            h_end_p(),
            NULL
        )),
        h_token("\xFF\xD9", 2),  // EOI Marker
        NULL
    );

    // Complete JPEG File Parser
    jpeg_file = h_sequence(
        jpeg_header,
        h_end_p(),
        NULL
    );

    return jpeg_file;
}

int main() {
    HParser* parser = create_jpeg_parser();
    return 0;
}