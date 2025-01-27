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

// CRC
static HParser *crc = h_uint32();

// PNG chunk
static HParser *png_chunk = h_sequence(chunk_type, chunk_data, crc, NULL);

// PNG file format
static HParser *png_file = h_sequence(
    h_chrs("\x89PNG\r\n\x1a\n", 8),  // PNG signature
    h_many(png_chunk),               // Multiple chunks
    NULL
);

int main(int argc, char **argv) {
    HParser *parser = png_file;
    HParseResult *result = h_parse(parser, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parse failed!\n");
    }
    h_parse_result_free(result);
    return 0;
}