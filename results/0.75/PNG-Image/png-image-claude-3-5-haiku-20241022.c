#include <hammer/hammer.h>
#include <hammer/glue.h>

static const HParser* png_signature;
static const HParser* png_ihdr_chunk;
static const HParser* png_chunk;
static const HParser* png_chunks;
static const HParser* png_file;

static const HParser* make_png_parser() {
    // PNG Signature: 8 fixed bytes
    png_signature = h_sequence(
        h_literal("\x89PNG\r\n\x1a\n"),
        NULL
    );

    // IHDR Chunk structure
    png_ihdr_chunk = h_sequence(
        h_length_value(h_int_range(0, 13), 13),  // Fixed 13-byte length
        h_literal("IHDR"),
        h_int_range(0, UINT32_MAX),  // Width
        h_int_range(0, UINT32_MAX),  // Height
        h_int_range(1, 8),           // Bit depth
        h_int_range(0, 6),           // Color type 
        h_int_range(0, 0),           // Compression method
        h_int_range(0, 0),           // Filter method
        h_int_range(0, 0),           // Interlace method
        h_int_range(0, UINT32_MAX),  // CRC
        NULL
    );

    // Generic chunk parser
    png_chunk = h_choice(
        png_ihdr_chunk,
        // Other chunk types can be added here
        NULL
    );

    // Multiple chunks
    png_chunks = h_many(png_chunk);

    // Complete PNG file parser
    png_file = h_sequence(
        png_signature,
        png_chunks,
        NULL
    );

    return png_file;
}

int main() {
    HParser* parser = make_png_parser();
    return 0;
}