#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef enum {
    COLOR_GRAYSCALE = 0,
    COLOR_RGB = 2,
    COLOR_PALETTE = 3,
    COLOR_GRAYSCALE_ALPHA = 4,
    COLOR_RGBA = 6
} ColorType;

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    ColorType color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} PngHeader;

typedef struct {
    uint32_t length;
    char type[5];
    HBytes* data;
    uint32_t crc;
} PngChunk;

static HParser* png_signature() {
    static const uint8_t signature[] = {0x89, 'P', 'N', 'G', '\r', '\n', 0x1A, '\n'};
    return h_literal_bytes((const char*)signature, sizeof(signature));
}

static HParser* png_header() {
    return h_sequence(
        h_uint32(),   // width
        h_uint32(),   // height
        h_uint8(),    // bit depth
        h_uint8(),    // color type
        h_uint8(),    // compression method
        h_uint8(),    // filter method
        h_uint8(),    // interlace method
        NULL
    );
}

static HParser* png_chunk() {
    return h_sequence(
        h_uint32(),           // length
        h_length_value(h_int_range(h_uint8(), 0, 4), h_token_bytes(4)),  // chunk type
        h_length_value(h_uint32(), h_token_bytes_res()),                // chunk data
        h_uint32(),           // CRC
        NULL
    );
}

static HParser* png_file() {
    return h_sequence(
        png_signature(),
        h_many(png_chunk()),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
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
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = png_file();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("PNG file parsed successfully\n");
    } else {
        printf("PNG file parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}