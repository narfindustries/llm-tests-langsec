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
} IHDR;

typedef struct {
    uint8_t *data;
    uint32_t length;
} IDAT;

typedef struct {
    uint8_t red;
    uint8_t green;
    uint8_t blue;
} PLTE_Entry;

typedef struct {
    PLTE_Entry *entries;
    uint32_t num_entries;
} PLTE;

typedef struct {
    uint8_t *data;
    uint32_t length;
} tRNS;

typedef struct {
    uint32_t gamma;
} gAMA;

typedef struct {
    uint32_t white_x;
    uint32_t white_y;
    uint32_t red_x;
    uint32_t red_y;
    uint32_t green_x;
    uint32_t green_y;
    uint32_t blue_x;
    uint32_t blue_y;
} cHRM;

typedef struct {
    uint8_t rendering_intent;
} sRGB;

typedef struct {
    char *profile_name;
    uint8_t *profile_data;
    uint32_t profile_length;
} iCCP;

typedef struct {
    char *keyword;
    char *text;
} tEXt;

typedef struct {
    char *keyword;
    uint8_t *compressed_text;
    uint32_t text_length;
} zTXt;

typedef struct {
    char *keyword;
    uint8_t compression_flag;
    char *language_tag;
    char *translated_keyword;
    char *text;
} iTXt;

typedef struct {
    uint16_t year;
    uint8_t month;
    uint8_t day;
    uint8_t hour;
    uint8_t minute;
    uint8_t second;
} tIME;

typedef struct {
    uint8_t *data;
    uint32_t length;
} bKGD;

typedef struct {
    uint32_t x_pixels;
    uint32_t y_pixels;
    uint8_t unit_specifier;
} pHYs;

typedef struct {
    uint8_t *data;
    uint32_t length;
} sBIT;

typedef struct {
    uint16_t *histogram;
    uint32_t num_entries;
} hIST;

typedef struct {
    IHDR ihdr;
    PLTE plte;
    tRNS trns;
    gAMA gamma;
    cHRM chrm;
    sRGB srgb;
    iCCP iccp;
    tEXt text;
    zTXt ztxt;
    iTXt itxt;
    bKGD bkgd;
    pHYs phys;
    sBIT sbit;
    hIST hist;
    tIME time;
    IDAT *idat;
    uint32_t num_idat;
} PNG;

HParser *png_signature() {
    return h_sequence(h_bits(8, 0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A), h_end_p(), NULL);
}

HParser *chunk_length() {
    return h_uint32();
}

HParser *chunk_type() {
    return h_bits(32, 0, 0, 0, 0);
}

HParser *crc() {
    return h_uint32();
}

HParser *parse_ihdr() {
    return h_sequence(
        h_uint32(), // width
        h_uint32(), // height
        h_uint8(),  // bit_depth
        h_uint8(),  // color_type
        h_uint8(),  // compression_method
        h_uint8(),  // filter_method
        h_uint8(),  // interlace_method
        h_end_p(),
        NULL
    );
}

HParser *parse_idat() {
    return h_sequence(
        h_length_value(h_uint32(), h_bits(8, 0)),
        h_end_p(),
        NULL
    );
}

HParser *parse_png() {
    return h_sequence(
        png_signature(),
        h_many1(h_sequence(
            chunk_length(),
            chunk_type(),
            h_choice(
                h_sequence(h_bits(32, 0x49, 0x48, 0x44, 0x52), parse_ihdr()),
                h_sequence(h_bits(32, 0x49, 0x44, 0x41, 0x54), parse_idat()),
                h_sequence(h_bits(32, 0x49, 0x45, 0x4E, 0x44), h_end_p()),
                NULL
            ),
            crc(),
            NULL
        )),
        h_end_p(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <png_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = parse_png();
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse PNG file\n");
        free(buffer);
        return 1;
    }

    PNG png;
    memset(&png, 0, sizeof(PNG));

    const HParsedToken *chunks = result->ast;
    for (size_t i = 0; i < chunks->token_len; i++) {
        const HParsedToken *chunk = &chunks->token[i];
        const uint8_t *type = chunk->token[1].u.bytes;
        if (memcmp(type, "IHDR", 4) == 0) {
            png.ihdr = *(IHDR*)chunk->token[2].u.bytes;
        } else if (memcmp(type, "IDAT", 4) == 0) {
            png.idat = realloc(png.idat, (png.num_idat + 1) * sizeof(IDAT));
            png.idat[png.num_idat++] = *(IDAT*)chunk->token[2].u.bytes;
        }
    }

    printf("PNG Width: %u\n", png.ihdr.width);
    printf("PNG Height: %u\n", png.ihdr.height);
    printf("PNG Bit Depth: %u\n", png.ihdr.bit_depth);
    printf("PNG Color Type: %u\n", png.ihdr.color_type);
    printf("PNG Compression Method: %u\n", png.ihdr.compression_method);
    printf("PNG Filter Method: %u\n", png.ihdr.filter_method);
    printf("PNG Interlace Method: %u\n", png.ihdr.interlace_method);

    for (uint32_t i = 0; i < png.num_idat; i++) {
        printf("IDAT Length: %u\n", png.idat[i].length);
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}