#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t PNG_SIGNATURE[] = {0x89, 'P', 'N', 'G', '\r', '\n', 0x1A, '\n'};

HParser* init_png_parser() {
    // PNG signature
    HParser* signature = h_token(PNG_SIGNATURE, sizeof(PNG_SIGNATURE));
    
    // Length (4 bytes)
    HParser* length = h_uint32();
    
    // Chunk type (4 bytes)
    HParser* chunk_type = h_token((const uint8_t*)"IHDR", 4);
    
    // Width (4 bytes)
    HParser* width = h_uint32();
    
    // Height (4 bytes)
    HParser* height = h_uint32();
    
    // Bit depth (1 byte)
    HParser* bit_depth = h_uint8();
    
    // Color type (1 byte)
    HParser* color_type = h_uint8();
    
    // Compression method (1 byte)
    HParser* compression = h_uint8();
    
    // Filter method (1 byte)
    HParser* filter = h_uint8();
    
    // Interlace method (1 byte)
    HParser* interlace = h_uint8();
    
    // CRC (4 bytes)
    HParser* crc = h_uint32();

    // Combine parsers for IHDR chunk
    HParser* ihdr_chunk = h_sequence(length, chunk_type, width, height, 
                                   bit_depth, color_type, compression,
                                   filter, interlace, crc, NULL);

    // Complete PNG parser
    return h_sequence(signature, ihdr_chunk, NULL);
}

int main() {
    HParser* png_parser = init_png_parser();
    
    if (!png_parser) {
        fprintf(stderr, "Failed to initialize PNG parser\n");
        return 1;
    }
    
    return 0;
}