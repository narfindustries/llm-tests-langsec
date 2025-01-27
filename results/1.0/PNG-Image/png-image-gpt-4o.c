#include <hammer/hammer.h>

static HParser *create_png_parser() {
    // PNG Signature: 8 bytes
    HParser *png_signature = h_sequence(
        h_uint8_val(0x89),
        h_uint8_val(0x50),
        h_uint8_val(0x4E),
        h_uint8_val(0x47),
        h_uint8_val(0x0D),
        h_uint8_val(0x0A),
        h_uint8_val(0x1A),
        h_uint8_val(0x0A),
        NULL
    );

    // Length: 4 bytes (big-endian)
    HParser *chunk_length = h_uint32_be();

    // Chunk Type: 4 bytes (ASCII)
    HParser *chunk_type = h_ascii_str_ci("IHDR", 4);
    
    // IHDR Data: 13 bytes fixed-size
    HParser *ihdr_data = h_sequence(
        h_uint32_be(), // Width: 4 bytes
        h_uint32_be(), // Height: 4 bytes
        h_uint8(),     // Bit depth: 1 byte
        h_uint8(),     // Color type: 1 byte
        h_uint8(),     // Compression method: 1 byte
        h_uint8(),     // Filter method: 1 byte
        h_uint8(),     // Interlace method: 1 byte
        NULL
    );

    // CRC: 4 bytes
    HParser *chunk_crc = h_uint32_be();

    // Chunk structure
    HParser *ihdr_chunk = h_sequence(
        chunk_length,
        chunk_type,
        ihdr_data,
        chunk_crc,
        NULL
    );

    // PNG File structure
    return h_sequence(
        png_signature,
        ihdr_chunk,
        NULL
    );
}

int main(int argc, char **argv) {
    HParser *png_parser = create_png_parser();

    // Use the parser to parse a PNG file here as required
    // (e.g., load a PNG file and pass its contents to the parser)

    // Clean up
    h_parser_free(png_parser);

    return 0;
}