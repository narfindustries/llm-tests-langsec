#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define PNG signature
static const uint8_t png_signature[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};

// Function prototypes
void parse_png(const char *filename);
HParser *build_png_parser();

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <png file>\n", argv[0]);
        return 1;
    }

    parse_png(argv[1]);
    return 0;
}

void parse_png(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    // Get file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read entire file into memory
    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(data, 1, file_size, file);
    fclose(file);

    // Build PNG parser
    HParser *png_parser = build_png_parser();

    // Parse the data
    HParseResult *result = h_parse(png_parser, data, file_size);
    if (result) {
        printf("PNG parsed successfully!\n");
    } else {
        fprintf(stderr, "Failed to parse PNG\n");
    }

    // Cleanup
    h_parser_free(png_parser);
    free(data);
}

HParser *build_png_parser() {
    // Basic parsers
    HParser *p_uint8 = h_uint8();
    HParser *p_uint16be = h_uint16_be();
    HParser *p_uint32be = h_uint32_be();
    HParser *p_bytes = h_any();

    // Signature parser
    HParser *p_signature = h_bits(sizeof(png_signature) * 8, false);

    // Chunk parsers
    HParser *p_chunk_type = h_bits(32, false);
    HParser *p_chunk_data = h_length_value(p_uint32be, p_bytes);
    HParser *p_crc = p_uint32be;

    HParser *p_chunk = h_sequence(p_uint32be, p_chunk_type, p_chunk_data, p_crc, NULL);

    // IHDR chunk
    HParser *p_ihdr = h_sequence(p_uint32be, p_uint32be, p_uint8, p_uint8, p_uint8, p_uint8, p_uint8, NULL);

    // PLTE chunk (optional)
    HParser *p_plte_entry = h_sequence(p_uint8, p_uint8, p_uint8, NULL);
    HParser *p_plte = h_many1(p_plte_entry);

    // IDAT chunk
    HParser *p_idat = h_greedy1(p_bytes);

    // IEND chunk
    HParser *p_iend = h_void();

    // PNG parser
    HParser *p_png = h_sequence(p_signature,
                                h_many(p_chunk),
                                p_ihdr,
                                h_optional(p_plte),
                                h_many1(p_idat),
                                p_iend,
                                NULL);

    return p_png;
}