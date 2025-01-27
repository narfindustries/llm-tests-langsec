#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <hammer/parser.h>
#include <hammer/stream.h>
#include <hammer/allocator.h>
#include <hammer/error.h>
#include <hammer/input.h>
#include <hammer/output.h>
#include <hammer/sequence.h>
#include <hammer/choice.h>
#include <hammer/action.h>
#include <hammer/and.h>
#include <hammer/not.h>
#include <hammer/whitespace.h>
#include <hammer/endian.h>
#include <hammer/bitwise.h>
#include <hammer/string.h>
#include <hammer/integer.h>
#include <hammer/float.h>
#include <hammer/array.h>
#include <hammer/struct.h>
#include <hammer/union.h>
#include <hammer/optional.h>
#include <hammer/repeat.h>
#include <hammer/reference.h>
#include <hammer/transform.h>
#include <hammer/error.h>
#include <hammer/stream.h>
#include <hammer/input.h>
#include <hammer/output.h>
#include <hammer/sequence.h>
#include <hammer/choice.h>
#include <hammer/action.h>
#include <hammer/and.h>
#include <hammer/not.h>
#include <hammer/whitespace.h>
#include <hammer/endian.h>
#include <hammer/bitwise.h>
#include <hammer/string.h>
#include <hammer/integer.h>
#include <hammer/float.h>
#include <hammer/array.h>
#include <hammer/struct.h>
#include <hammer/union.h>
#include <hammer/optional.h>
#include <hammer/repeat.h>
#include <hammer/reference.h>
#include <hammer/transform.h>

typedef struct {
    uint16_t soi_marker;
    uint16_t app0_marker;
    uint16_t app0_length;
    char identifier[5];
    uint16_t version;
    uint8_t units;
    uint16_t x_density;
    uint16_t y_density;
    uint8_t x_thumbnail;
    uint8_t y_thumbnail;
} jpeg_header;

HParser *jpeg_header_parser() {
    return h_sequence(
        h_uint16_be(), // SOI marker
        h_uint16_be(), // APP0 marker
        h_uint16_be(), // APP0 length
        h_string_constant("JFIF\0"), // Identifier
        h_uint16_be(), // Version
        h_uint8(), // Units
        h_uint16_be(), // X density
        h_uint16_be(), // Y density
        h_uint8(), // X thumbnail
        h_uint8(), // Y thumbnail
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *parser = jpeg_header_parser();
    HInputStream *input = h_file_input_stream_new("input.jpg");
    HParseResult *result = h_parse(parser, input);
    if (result) {
        jpeg_header *header = (jpeg_header *)result->ast;
        printf("SOI Marker: 0x%04X\n", header->soi_marker);
        printf("APP0 Marker: 0x%04X\n", header->app0_marker);
        printf("APP0 Length: %d\n", header->app0_length);
        printf("Identifier: %s\n", header->identifier);
        printf("Version: %d\n", header->version);
        printf("Units: %d\n", header->units);
        printf("X Density: %d\n", header->x_density);
        printf("Y Density: %d\n", header->y_density);
        printf("X Thumbnail: %d\n", header->x_thumbnail);
        printf("Y Thumbnail: %d\n", header->y_thumbnail);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse input.\n");
    }
    h_input_stream_free(input);
    h_parser_free(parser);
    return 0;
}