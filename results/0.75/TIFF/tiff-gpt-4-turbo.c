#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define TIFF tags as per TIFF 6.0 specification
#define NewSubfileType 254
#define ImageWidth 256
#define ImageLength 257
#define BitsPerSample 258
#define Compression 259
#define PhotometricInterpretation 262
#define DocumentName 269
#define ImageDescription 270
#define Make 271
#define Model 272

// TIFF data types
#define BYTE 1
#define ASCII 2
#define SHORT 3
#define LONG 4
#define RATIONAL 5

// Function prototypes for creating parsers
HParser *hp_byte();
HParser *hp_short();
HParser *hp_long();
HParser *hp_ascii();

// Function definitions for basic TIFF data types
HParser *hp_byte() {
    return h_uint8();
}

HParser *hp_short() {
    return h_uint16();
}

HParser *hp_long() {
    return h_uint32();
}

HParser *hp_ascii() {
    return h_sequence(h_uint8(), h_many(h_ch_range(0x20, 0x7E)), NULL);
}

// TIFF field parser based on type
HParser *tiff_field(uint16_t tag, HParser *type) {
    HParser *hp_tag = h_uint16();
    HParser *hp_type = h_uint16();
    HParser *hp_length = h_uint32();
    HParser *hp_value = h_length_value(hp_length, type);

    return h_sequence(hp_tag, hp_type, hp_length, hp_value, NULL);
}

// Main TIFF parser that includes all necessary fields
HParser *tiff_parser() {
    return h_many(h_choice(
        tiff_field(NewSubfileType, hp_long()),
        tiff_field(ImageWidth, hp_short()),
        tiff_field(ImageLength, hp_short()),
        tiff_field(BitsPerSample, hp_short()),
        tiff_field(Compression, hp_short()),
        tiff_field(PhotometricInterpretation, hp_short()),
        tiff_field(DocumentName, hp_ascii()),
        tiff_field(ImageDescription, hp_ascii()),
        tiff_field(Make, hp_ascii()),
        tiff_field(Model, hp_ascii()),
        NULL
    ));
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <TIFF file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(size);
    if (buffer == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, size, fp);
    fclose(fp);

    HParser *parser = tiff_parser();
    HParseResult *result = h_parse(parser, buffer, size);
    if (result == NULL) {
        fprintf(stderr, "Parsing failed\n");
        free(buffer);
        return EXIT_FAILURE;
    }

    h_pprint(stdout, result->ast, 0, 0);

    h_parse_result_free(result);
    free(buffer);
    return EXIT_SUCCESS;
}