#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} IHDRChunk;

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
} PaletteEntry;

typedef struct {
    PaletteEntry* entries;
    size_t count;
} PLTEChunk;

typedef struct {
    uint8_t* data;
    size_t length;
} IDATChunk;

typedef struct {
    char* keyword;
    char* text;
} tEXtChunk;

typedef struct {
    IHDRChunk ihdr;
    PLTEChunk plte;
    IDATChunk idat;
    tEXtChunk* text_chunks;
    size_t text_chunk_count;
} PNGImage;

HParser* png_signature() {
    static const uint8_t signature[] = {0x89, 'P', 'N', 'G', 0x0D, 0x0A, 0x1A, 0x0A};
    return h_token(signature, sizeof(signature));
}

HParser* ihdr_chunk() {
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

HParser* palette_entry() {
    return h_sequence(
        h_uint8(),    // R
        h_uint8(),    // G
        h_uint8(),    // B
        NULL
    );
}

HParser* plte_chunk() {
    return h_many(palette_entry());
}

HParser* text_chunk() {
    return h_sequence(
        h_many1(h_ch_range('A', 'Z')),  // keyword
        h_many1(h_ch_range(' ', '~')),  // text
        NULL
    );
}

HParser* png_parser() {
    return h_sequence(
        png_signature(),
        ihdr_chunk(),
        h_optional(plte_chunk()),
        h_many(text_chunk()),
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

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser* parser = png_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("PNG file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("PNG parsing failed\n");
    }

    free(buffer);
    return 0;
}