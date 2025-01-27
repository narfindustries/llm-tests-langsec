#include <hammer/hammer.h>

HParser *create_png_parser() {
    // PNG file signature
    HParser *png_signature = h_sequence(
        h_uint8_val(0x89),
        h_uint8_val(0x50), // P
        h_uint8_val(0x4E), // N
        h_uint8_val(0x47), // G
        h_uint8_val(0x0D), // CR
        h_uint8_val(0x0A), // LF
        h_uint8_val(0x1A), // EOF
        h_uint8_val(0x0A), // LF
        NULL
    );

    // Length: 4 bytes
    HParser *chunk_length = h_uint32();

    // Chunk type: 4 bytes
    HParser *chunk_type = h_bytes(4);

    // Chunk data: variable length
    HParser *chunk_data = h_length_value(chunk_length, h_uint8());

    // CRC: 4 bytes
    HParser *chunk_crc = h_uint32();

    // PNG chunk
    HParser *png_chunk = h_sequence(
        chunk_length,
        chunk_type,
        chunk_data,
        chunk_crc,
        NULL
    );

    // PNG file structure
    HParser *png_file = h_sequence(
        png_signature,
        h_many1(png_chunk),
        NULL
    );

    return png_file;
}

int main(int argc, char **argv) {
    HParser *png_parser = create_png_parser();
    // Use the parser with input data here
    // Example: HParseResult *result = h_parse(png_parser, input_data, input_length);

    // Clean up
    h_parser_free(png_parser);
    return 0;
}