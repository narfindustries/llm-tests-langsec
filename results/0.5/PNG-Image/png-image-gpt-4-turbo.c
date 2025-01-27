#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define basic building blocks
static HParser *uint8 = h_uint8();
static HParser *uint16 = h_uint16be();
static HParser *uint32 = h_uint32be();

// Chunk type
static HParser *chunk_type = h_bytes(4);

// Chunk data
static HParser *chunk_data = h_length_value(h_uint32(), h_bytes);

// Chunk CRC
static HParser *chunk_crc = h_uint32();

// Single chunk
static HParser *chunk = h_sequence(chunk_type, chunk_data, chunk_crc, NULL);

// PNG file signature
static HParser *png_signature = h_bytes("x89PNG\r\n\x1a\n", 8);

// PNG image
static HParser *png_image = h_sequence(png_signature, h_many(chunk), NULL);

static void init_png_grammar(void) __attribute__((constructor));
static void init_png_grammar(void) {
    H_RULE(png_signature, h_bytes("x89PNG\r\n\x1a\n", 8));
    H_RULE(uint8, h_uint8());
    H_RULE(uint16, h_uint16be());
    H_RULE(uint32, h_uint32be());
    H_RULE(chunk_type, h_bytes(4));
    H_RULE(chunk_data, h_length_value(h_uint32(), h_bytes));
    H_RULE(chunk_crc, h_uint32());
    H_RULE(chunk, h_sequence(chunk_type, chunk_data, chunk_crc, NULL));
    H_RULE(png_image, h_sequence(png_signature, h_many(chunk), NULL));
}

int main(int argc, char **argv) {
    HParser *parser = png_image;
    HParseResult *result = h_parse(parser, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("PNG parsing successful.\n");
    } else {
        printf("Failed to parse PNG.\n");
    }
    h_parse_result_free(result);
    return 0;
}