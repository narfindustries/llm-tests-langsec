#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// PNG Signature parser
static HParser* png_signature() {
    uint8_t sig[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
    return h_token(sig, 8);
}

// Chunk length parser
static HParser* chunk_length() {
    return h_uint32();
}

// Chunk type parser
static HParser* chunk_type() {
    return h_repeat_n(h_ch_range(0x41, 0x7A), 4);
}

// IHDR chunk parser
static HParser* ihdr_chunk() {
    return h_sequence(
        h_uint32(), // width
        h_uint32(), // height
        h_uint8(),  // bit depth
        h_uint8(),  // color type
        h_uint8(),  // compression method
        h_uint8(),  // filter method
        h_uint8(),  // interlace method
        NULL
    );
}

// PLTE chunk parser
static HParser* plte_chunk() {
    return h_many1(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL));
}

// IDAT chunk parser
static HParser* idat_chunk() {
    return h_many1(h_uint8());
}

// tRNS chunk parser
static HParser* trns_chunk() {
    return h_many1(h_uint8());
}

// gAMA chunk parser
static HParser* gama_chunk() {
    return h_uint32();
}

// cHRM chunk parser
static HParser* chrm_chunk() {
    return h_sequence(
        h_uint32(), // white point x
        h_uint32(), // white point y
        h_uint32(), // red x
        h_uint32(), // red y
        h_uint32(), // green x
        h_uint32(), // green y
        h_uint32(), // blue x
        h_uint32(), // blue y
        NULL
    );
}

// sRGB chunk parser
static HParser* srgb_chunk() {
    return h_uint8();
}

// iCCP chunk parser
static HParser* iccp_chunk() {
    return h_sequence(
        h_repeat_n(h_ch_range(32, 126), 1), // profile name
        h_ch(0),                            // null separator
        h_uint8(),                          // compression method
        h_many1(h_uint8()),                 // compressed profile
        NULL
    );
}

// tEXt chunk parser
static HParser* text_chunk() {
    return h_sequence(
        h_repeat_n(h_ch_range(32, 126), 1), // keyword
        h_ch(0),                            // null separator
        h_many1(h_uint8()),                 // text
        NULL
    );
}

// zTXt chunk parser
static HParser* ztxt_chunk() {
    return h_sequence(
        h_repeat_n(h_ch_range(32, 126), 1), // keyword
        h_ch(0),                            // null separator
        h_uint8(),                          // compression method
        h_many1(h_uint8()),                 // compressed text
        NULL
    );
}

// iTXt chunk parser
static HParser* itxt_chunk() {
    return h_sequence(
        h_repeat_n(h_ch_range(32, 126), 1), // keyword
        h_ch(0),                            // null separator
        h_uint8(),                          // compression flag
        h_uint8(),                          // compression method
        h_many1(h_uint8()),                 // language tag
        h_ch(0),                            // null separator
        h_many1(h_uint8()),                 // translated keyword
        h_ch(0),                            // null separator
        h_many1(h_uint8()),                 // text
        NULL
    );
}

// bKGD chunk parser
static HParser* bkgd_chunk() {
    return h_choice(
        h_uint16(),                           // for color type 0,4
        h_sequence(h_uint16(), h_uint16(), h_uint16(), NULL), // for color type 2,6
        h_uint8(),                            // for color type 3
        NULL
    );
}

// pHYs chunk parser
static HParser* phys_chunk() {
    return h_sequence(
        h_uint32(), // pixels per unit X
        h_uint32(), // pixels per unit Y
        h_uint8(),  // unit specifier
        NULL
    );
}

// sBIT chunk parser
static HParser* sbit_chunk() {
    return h_many1(h_uint8());
}

// sPLT chunk parser
static HParser* splt_chunk() {
    return h_sequence(
        h_repeat_n(h_ch_range(32, 126), 1), // palette name
        h_ch(0),                            // null separator
        h_uint8(),                          // sample depth
        h_many1(h_uint8()),                 // palette entries
        NULL
    );
}

// hIST chunk parser
static HParser* hist_chunk() {
    return h_many1(h_uint16());
}

// tIME chunk parser
static HParser* time_chunk() {
    return h_sequence(
        h_uint16(), // year
        h_uint8(),  // month
        h_uint8(),  // day
        h_uint8(),  // hour
        h_uint8(),  // minute
        h_uint8(),  // second
        NULL
    );
}

// Generic chunk parser
static HParser* chunk() {
    return h_sequence(
        chunk_length(),
        chunk_type(),
        h_many1(h_uint8()),  // chunk data
        h_uint32(),          // CRC
        NULL
    );
}

// Complete PNG parser
static HParser* png_parser() {
    return h_sequence(
        png_signature(),
        h_many1(chunk()),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
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

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser *parser = png_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("PNG file parsed successfully\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse PNG file\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}