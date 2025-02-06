#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

// PNG Signature Constants
const uint8_t PNG_SIGNATURE[8] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};

// Color Type Enums
typedef enum {
    GRAYSCALE = 0,
    RGB = 2,
    PALETTE = 3,
    GRAYSCALE_ALPHA = 4,
    RGB_ALPHA = 6
} ColorType;

// PNG Chunk Structure
typedef struct {
    uint32_t length;
    char type[5];
    void* data;
    uint32_t crc;
} PNGChunk;

// IHDR Chunk Data
typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    ColorType color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} IHDRData;

HParser* png_signature_parser() {
    return h_literal(h_mem_copy(PNG_SIGNATURE, sizeof(PNG_SIGNATURE)));
}

HParser* ihdr_parser() {
    return h_sequence(
        h_uint32(),
        h_uint32(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        NULL
    );
}

HParser* chunk_data_parser() {
    return h_many(h_ch_range(0, 255));
}

HParser* chunk_parser() {
    return h_sequence(
        h_uint32(),   // Length
        h_repeat_n(h_ch_range(0, 255), 4),  // Chunk Type
        chunk_data_parser(),  // Chunk Data
        h_uint32(),   // CRC
        NULL
    );
}

HParser* png_parser() {
    return h_sequence(
        png_signature_parser(),
        h_many1(chunk_parser()),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* parser = png_parser();
    HParseResult* result = h_parse(parser, buffer, read_size);

    if (result && result->ast) {
        printf("PNG file parsed successfully\n");
    } else {
        fprintf(stderr, "PNG parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}