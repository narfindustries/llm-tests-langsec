#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static HParser* png_parser(void) {
    // PNG file signature
    HParser* magic = h_sequence(
        h_literal("\x89PNG\r\n\x1a\n"), 
        NULL
    );

    // IHDR chunk parser
    HParser* width = h_uint32();
    HParser* height = h_uint32();
    HParser* bit_depth = h_uint8();
    HParser* color_type = h_uint8();
    HParser* compression = h_uint8();
    HParser* filter = h_uint8();
    HParser* interlace = h_uint8();

    HParser* ihdr_data = h_sequence(
        width, height, bit_depth, color_type, 
        compression, filter, interlace, 
        NULL
    );

    // Generic chunk structure
    HParser* chunk_length = h_uint32();
    HParser* chunk_type = h_length_value(h_uint32(), h_many(h_uint8()));
    HParser* chunk_crc = h_uint32();

    HParser* chunk = h_sequence(
        chunk_length,
        chunk_type,
        chunk_crc,
        NULL
    );

    // Full PNG parser
    HParser* png = h_sequence(
        magic,
        chunk,  // IHDR chunk
        h_many(chunk),  // Other chunks
        NULL
    );

    return png;
}

int main(int argc, char** argv) {
    HParser* parser = png_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to create parser\n");
        return 1;
    }

    return 0;
}