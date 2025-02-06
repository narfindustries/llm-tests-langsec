#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint32_t width;
    uint32_t height;
    uint8_t bit_depth;
    uint8_t color_type;
    uint8_t compression_method;
    uint8_t filter_method;
    uint8_t interlace_method;
} PNG_IHDR;

typedef struct {
    uint8_t r;
    uint8_t g;
    uint8_t b;
} RGB_Triplet;

typedef struct {
    PNG_IHDR header;
    RGB_Triplet* palette;
    uint8_t* transparency_data;
    double gamma;
    double white_x;
    double white_y;
    double red_x;
    double red_y;
    double green_x;
    double green_y;
    double blue_x;
    double blue_y;
    uint8_t srgb_intent;
    char* icc_profile_name;
    uint8_t* icc_profile_data;
    char** text_metadata;
} PNG_Image;

static HParser* png_signature_parser() {
    static const uint8_t PNG_SIG[] = {0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A};
    return h_token(PNG_SIG, sizeof(PNG_SIG));
}

static HParser* png_chunk_parser() {
    HParser* length = h_uint32();
    HParser* type = h_repeat_n(h_uint8(), 4);
    HParser* data = h_repeat_n(h_uint8(), (size_t)h_uint32());
    HParser* crc = h_uint32();
    
    return h_sequence(length, type, data, crc, NULL);
}

static HParser* png_ihdr_parser() {
    return h_sequence(
        h_uint32(),   // Width
        h_uint32(),   // Height
        h_uint8(),    // Bit Depth
        h_uint8(),    // Color Type
        h_uint8(),    // Compression Method
        h_uint8(),    // Filter Method
        h_uint8(),    // Interlace Method
        NULL
    );
}

static HParser* png_plte_parser(size_t palette_entries) {
    return h_repeat_n(h_sequence(
        h_uint8(), // Red
        h_uint8(), // Green
        h_uint8(), // Blue
        NULL
    ), palette_entries);
}

static HParser* png_parser() {
    return h_sequence(
        png_signature_parser(),
        png_chunk_parser(),  // IHDR
        png_chunk_parser(),  // PLTE (optional)
        png_chunk_parser(),  // Additional optional chunks
        png_chunk_parser(),  // IDAT
        png_chunk_parser(),  // IEND
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
        perror("Cannot open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
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

    HParser* png_parser_instance = png_parser();
    HParseResult* result = h_parse(png_parser_instance, buffer, file_size);

    if (result && result->ast) {
        printf("PNG file parsed successfully\n");
    } else {
        printf("PNG parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(png_parser_instance);
    free(buffer);

    return 0;
}