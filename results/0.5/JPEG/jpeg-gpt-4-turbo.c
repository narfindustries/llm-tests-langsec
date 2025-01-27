#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>

// Define JPEG markers
static const uint8_t SOI_MARKER[] = {0xFF, 0xD8}; // Start of Image
static const uint8_t EOI_MARKER[] = {0xFF, 0xD9}; // End of Image
static const uint8_t SOF0_MARKER[] = {0xFF, 0xC0}; // Start of Frame (Baseline DCT)

// Define JPEG segment structure
typedef struct {
    uint8_t marker[2];
    uint16_t length;
    uint8_t *data;
} jpeg_segment;

// Hammer parsers for JPEG components
HParser *jpeg_marker;
HParser *jpeg_length;
HParser *jpeg_segment_data;
HParser *jpeg_segment;

void init_parsers() {
    jpeg_marker = h_bytes(SOI_MARKER, sizeof(SOI_MARKER));
    jpeg_length = h_uint16_be();
    jpeg_segment_data = h_length_value(jpeg_length, h_uint8());

    jpeg_segment = h_sequence(jpeg_marker,
                              jpeg_length,
                              jpeg_segment_data,
                              NULL);
}

int main(int argc, char *argv[]) {
    init_parsers();

    // Example JPEG data for testing
    uint8_t jpeg_data[] = {
        0xFF, 0xD8,                 // SOI
        0xFF, 0xC0, 0x00, 0x11,     // SOF0 with length
        0x01, 0x02, 0x03, 0x04,     // Arbitrary segment data
        0xFF, 0xD9                  // EOI
    };
    size_t jpeg_data_len = sizeof(jpeg_data);

    HParseResult *result = h_parse(jpeg_segment, jpeg_data, jpeg_data_len);
    if (result) {
        printf("JPEG parsing successful.\n");
        jpeg_segment *seg = (jpeg_segment*)result->ast;
        printf("Marker: %02X %02X\n", seg->marker[0], seg->marker[1]);
        printf("Length: %d\n", seg->length);
        printf("Data: ");
        for (int i = 0; i < seg->length - 2; i++) {
            printf("%02X ", seg->data[i]);
        }
        printf("\n");
    } else {
        printf("JPEG parsing failed.\n");
    }

    h_parse_result_free(result);
    h_free_parser(jpeg_segment);
    h_free_parser(jpeg_segment_data);
    h_free_parser(jpeg_length);
    h_free_parser(jpeg_marker);

    return 0;
}