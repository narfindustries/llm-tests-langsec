#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

static HParser *uint32;
static HParser *uint8;
static HParser *chunk_type;
static HParser *chunk_data;
static HParser *chunk_crc;
static HParser *chunk;
static HParser *ihdr_chunk;
static HParser *plte_chunk;
static HParser *idat_chunk;
static HParser *iend_chunk;
static HParser *png_parser;

static const uint8_t png_signature[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};

void init_parsers() {
    uint32 = h_uint32();
    uint8 = h_uint8();
    chunk_type = h_bits(32, false);
    chunk_data = h_length_value(uint32, h_arbitrary_bytes());
    chunk_crc = h_bits(32, false);
    chunk = h_sequence(uint32, chunk_type, chunk_data, chunk_crc, NULL);

    ihdr_chunk = h_sequence(
        h_bits(104, false),  // IHDR fixed data length (13 bytes = 104 bits)
        uint32,              // width
        uint32,              // height
        uint8,               // bit depth
        uint8,               // color type
        uint8,               // compression method
        uint8,               // filter method
        uint8,               // interlace method
        NULL
    );

    plte_chunk = h_many1(h_sequence(uint8, uint8, uint8, NULL)); // RGB triplets
    idat_chunk = h_many1(chunk_data);  // Multiple IDAT chunks
    iend_chunk = h_bits(0, false);  // No data in IEND

    png_parser = h_sequence(
        h_bits(sizeof(png_signature) * 8, false),  // Signature
        ihdr_chunk,
        h_optional(h_many(plte_chunk)),  // Optional PLTE chunks
        h_optional(h_many(idat_chunk)),  // Multiple IDAT chunks
        iend_chunk,
        NULL
    );
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <png file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    long file_size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (data == NULL) {
        perror("Failed to allocate memory");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, fp);
    fclose(fp);

    init_parsers();

    HParseResult *result = h_parse(png_parser, data, file_size);
    if (result == NULL) {
        fprintf(stderr, "Failed to parse PNG file\n");
        free(data);
        return EXIT_FAILURE;
    }

    printf("PNG file parsed successfully.\n");
    free(data);
    return EXIT_SUCCESS;
}