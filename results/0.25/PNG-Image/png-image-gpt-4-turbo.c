#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the basic building blocks of a PNG file
static HParser *uint8 = h_uint8();
static HParser *uint32 = h_uint32();
static HParser *signature = h_bytes((const uint8_t *)"\x89\x50\x4e\x47\x0d\x0a\x1a\x0a", 8);

// Chunk structure: length, type, data, CRC
static HParser *chunk_type = h_bytes(4);
static HParser *chunk_data = h_length_value(h_uint32(), h_bytes__());
static HParser *crc = h_uint32();

static HParser *chunk = h_sequence(
    h_uint32(),
    chunk_type,
    chunk_data,
    crc,
    NULL
);

// A PNG file is a signature followed by one or more chunks
static HParser *png = h_sequence(
    signature,
    h_many1(chunk),
    NULL
);

int main(int argc, char **argv) {
    HParseResult *result = h_parse(png, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("PNG parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse PNG.\n");
    }
    return 0;
}