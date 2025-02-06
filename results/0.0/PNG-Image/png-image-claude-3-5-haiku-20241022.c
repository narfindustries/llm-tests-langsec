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
    uint8_t r, g, b;
} RGBTriple;

typedef struct {
    RGBTriple* entries;
    size_t count;
} PLTEChunk;

typedef struct {
    uint16_t gray;
    uint16_t* rgb;
    uint8_t* indexed;
} tRNSChunk;

typedef struct {
    uint32_t gamma_value;
} gAMAChunk;

typedef struct {
    uint32_t white_x, white_y;
    uint32_t red_x, red_y;
    uint32_t green_x, green_y;
    uint32_t blue_x, blue_y;
} cHRMChunk;

typedef struct {
    uint8_t rendering_intent;
} sRGBChunk;

typedef struct {
    char* profile_name;
    uint8_t* compressed_profile;
    size_t profile_length;
} iCCPChunk;

typedef struct {
    char* keyword;
    char* text;
} tEXtChunk;

typedef struct {
    char* keyword;
    char* compressed_text;
} zTXtChunk;

typedef struct {
    char* keyword;
    char* language_tag;
    char* translated_keyword;
    char* text;
} iTXtChunk;

typedef struct {
    uint16_t red;
    uint16_t green;
    uint16_t blue;
} bKGDChunk;

typedef struct {
    uint32_t pixels_per_x;
    uint32_t pixels_per_y;
    uint8_t unit_specifier;
} pHYsChunk;

typedef struct {
    uint8_t significant_red;
    uint8_t significant_green;
    uint8_t significant_blue;
    uint8_t significant_gray;
    uint8_t significant_alpha;
} sBITChunk;

typedef struct {
    uint16_t year;
    uint8_t month;
    uint8_t day;
    uint8_t hour;
    uint8_t minute;
    uint8_t second;
} tIMEChunk;

typedef struct {
    IHDRChunk* ihdr;
    PLTEChunk* plte;
    tRNSChunk* trns;
    gAMAChunk* gama;
    cHRMChunk* chrm;
    sRGBChunk* srgb;
    iCCPChunk* iccp;
    tEXtChunk** text_chunks;
    zTXtChunk** ztxt_chunks;
    iTXtChunk** itxt_chunks;
    bKGDChunk* bkgd;
    pHYsChunk* phys;
    sBITChunk* sbit;
    tIMEChunk* time;
    uint8_t** idat_chunks;
    size_t idat_count;
} PNGImage;

static const uint8_t PNG_SIGNATURE[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};

static HParsedToken* parse_png_signature(const HParseResult* result, void* user_data) {
    const uint8_t* data = (const uint8_t*)result->data;
    if (memcmp(data, PNG_SIGNATURE, sizeof(PNG_SIGNATURE)) == 0) {
        return h_make_uint(1);
    }
    return NULL;
}

static HParsedToken* parse_ihdr_chunk(const HParseResult* result, void* user_data) {
    IHDRChunk* ihdr = malloc(sizeof(IHDRChunk));
    const uint8_t* data = (const uint8_t*)result->data;
    
    ihdr->width = (data[0] << 24) | (data[1] << 16) | (data[2] << 8) | data[3];
    ihdr->height = (data[4] << 24) | (data[5] << 16) | (data[6] << 8) | data[7];
    ihdr->bit_depth = data[8];
    ihdr->color_type = data[9];
    ihdr->compression_method = data[10];
    ihdr->filter_method = data[11];
    ihdr->interlace_method = data[12];
    
    return h_make_struct(ihdr);
}

HParser* png_parser() {
    HParser* signature = h_action(h_token(PNG_SIGNATURE, sizeof(PNG_SIGNATURE)), parse_png_signature, NULL);
    HParser* ihdr = h_action(h_token((const uint8_t*)"\x00\x00\x00\x0DIHDR", 8), parse_ihdr_chunk, NULL);
    HParser* plte = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x00PLTE", 8));
    HParser* trns = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x00tRNS", 8));
    HParser* gama = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x04gAMA", 8));
    HParser* chrm = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x20cHRM", 8));
    HParser* srgb = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x01sRGB", 8));
    HParser* iccp = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x00iCCP", 8));
    HParser* text = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x00tEXt", 8));
    HParser* ztxt = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x00zTXt", 8));
    HParser* itxt = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x00iTXt", 8));
    HParser* bkgd = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x06bKGD", 8));
    HParser* phys = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x09pHYs", 8));
    HParser* sbit = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x05sBIT", 8));
    HParser* time = h_optional(h_token((const uint8_t*)"\x00\x00\x00\x07tIME", 8));
    HParser* idat = h_many(h_token((const uint8_t*)"\x00\x00\x00\x00IDAT", 8));
    HParser* iend = h_token((const uint8_t*)"\x00\x00\x00\x00IEND", 8);

    return h_sequence(signature, ihdr, 
        plte, trns, gama, chrm, srgb, iccp, 
        text, ztxt, itxt, bkgd, phys, sbit, time, 
        idat, iend, NULL);
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
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
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
        printf("PNG file parsing failed\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    free(buffer);

    return 0;
}