#include <hammer/hammer.h>

HParser *create_png_parser() {
    // PNG file signature
    HParser *png_signature = h_sequence(
        h_uint8_val(0x89),
        h_ch('P'),
        h_ch('N'),
        h_ch('G'),
        h_uint8_val(0x0D),
        h_uint8_val(0x0A),
        h_uint8_val(0x1A),
        h_uint8_val(0x0A),
        NULL
    );

    // Length (4 bytes) and Chunk Type (4 bytes)
    HParser *chunk_length = h_uint32();
    HParser *chunk_type = h_string(4);

    // CRC (4 bytes)
    HParser *chunk_crc = h_uint32();

    // Chunk Data (variable length)
    HParser *chunk_data = h_length_value(chunk_length, h_uint8());

    // PNG Chunk
    HParser *png_chunk = h_sequence(
        chunk_length,
        chunk_type,
        chunk_data,
        chunk_crc,
        NULL
    );

    // PNG File Structure
    HParser *png_file = h_sequence(
        png_signature,
        h_many1(png_chunk),
        NULL
    );

    return png_file;
}

int main(int argc, char **argv) {
    HParser *png_parser = create_png_parser();
    // Use the parser with input data as needed
    // Remember to free the parser when done
    h_parser_free(png_parser);
    return 0;
}