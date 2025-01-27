#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Hammer parsers for JPEG structures
static HParser *jpeg_soi;  // Start of Image
static HParser *jpeg_app0; // APP0 marker
static HParser *jpeg_dqt;  // Define Quantization Table
static HParser *jpeg_sof0; // Start of Frame (Baseline DCT)
static HParser *jpeg_dht;  // Define Huffman Table
static HParser *jpeg_sos;  // Start of Scan
static HParser *jpeg_eoi;  // End of Image
static HParser *jpeg_segment;

void init_parsers() {
    H_UINT16_BE(uint16);
    
    jpeg_soi = h_token_u16(0xFFD8);  // SOI marker
    jpeg_app0 = h_sequence(h_token_u16(0xFFE0), h_length_value(h_uint16(), h_any()), NULL);
    jpeg_dqt = h_sequence(h_token_u16(0xFFDB), h_length_value(h_uint16(), h_any()), NULL);
    jpeg_sof0 = h_sequence(h_token_u16(0xFFC0), h_length_value(h_uint16(), h_any()), NULL);
    jpeg_dht = h_sequence(h_token_u16(0xFFC4), h_length_value(h_uint16(), h_any()), NULL);
    jpeg_sos = h_sequence(h_token_u16(0xFFDA), h_length_value(h_uint16(), h_any()), NULL);
    jpeg_eoi = h_token_u16(0xFFD9);  // EOI marker
    
    jpeg_segment = h_choice(jpeg_soi, jpeg_app0, jpeg_dqt, jpeg_sof0, jpeg_dht, jpeg_sos, jpeg_eoi, NULL);
}

int main() {
    init_parsers();
    
    // Create input buffer with JPEG data
    uint8_t jpeg_data[] = {
        0xFF, 0xD8,       // SOI
        0xFF, 0xE0, 0x00, 0x10, 'J', 'F', 'I', 'F', 0x00, 0x01, 0x01, 0x00, 0x00, 0x01, 0x00, 0x01, 0x00, 0x00,  // APP0
        0xFF, 0xDB,       // DQT
        0x00, 0x43, 0x00, // DQT Length and QT Information
        0xFF, 0xC0, 0x00, 0x11, 0x08, 0x00, 0x10, 0x00, 0x10, 0x03, 0x01, 0x11, 0x00, 0x02, 0x11, 0x01, 0x03, 0x11, 0x01,  // SOF0
        0xFF, 0xC4,       // DHT
        0xFF, 0xDA,       // SOS
        0xFF, 0xD9        // EOI
    };

    size_t len = sizeof(jpeg_data)/sizeof(jpeg_data[0]);
    HParseResult *result = h_parse(jpeg_segment, jpeg_data, len);

    if (result) {
        printf("JPEG parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse JPEG.\n");
    }

    h_parse_result_free(result);
    h_shutdown_parsers();

    return 0;
}