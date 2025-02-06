#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Definitions for TIFF field types
#define BYTE      1
#define ASCII     2
#define SHORT     3
#define LONG      4
#define RATIONAL  5

// TIFF tag identifiers
#define ImageWidth                256
#define ImageLength               257
#define BitsPerSample             258
#define Compression               259
#define PhotometricInterpretation 262
#define StripOffsets              273
#define SamplesPerPixel           277
#define RowsPerStrip              278
#define StripByteCounts           279
#define XResolution               282
#define YResolution               283
#define PlanarConfiguration       284

// Prototype definitions for parsers
static HParser *build_tiff_parser();

// Parser implementations
static HParser *parse_byte() {
    return h_uint8();
}

static HParser *parse_short() {
    return h_uint16();
}

static HParser *parse_long() {
    return h_uint32();
}

static HParser *parse_rational() {
    return h_sequence(parse_long(), parse_long(), NULL);
}

static HParser *parse_ascii() {
    return h_many1(h_ch_range(0x20, 0x7E));  // Printable ASCII characters plus space
}

static HParser *parse_ifd_entry() {
    return h_sequence(parse_short(),  // Tag
                      parse_short(),  // Type
                      parse_long(),   // Length
                      h_choice(parse_byte(), parse_ascii(), parse_short(), parse_long(), parse_rational(), NULL),
                      NULL);
}

static HParser *parse_ifd() {
    return h_sequence(parse_short(),
                      h_many1(parse_ifd_entry()),
                      parse_long(),
                      NULL);
}

static HParser *parse_tiff_header() {
    return h_sequence(h_uint16(),  // Byte order (II or MM)
                      h_uint16(),  // Fixed value 0x002A
                      parse_long(), // Offset to the first IFD
                      NULL);
}

static HParser *build_tiff_parser() {
    return h_sequence(parse_tiff_header(), parse_ifd(), NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <TIFF file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t sz = ftell(fp);
    rewind(fp);

    char *buffer = malloc(sz);
    if (!buffer) {
        fclose(fp);
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    fread(buffer, 1, sz, fp);
    fclose(fp);

    HParser *tiff_parser = build_tiff_parser();
    HParseResult *result = h_parse(tiff_parser, (const uint8_t *)buffer, sz);
    if (result) {
        printf("TIFF parsing successful.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        fprintf(stderr, "TIFF parsing failed.\n");
    }

    h_parse_result_free(result);
    h_parser_cleanup(tiff_parser);
    free(buffer);

    return 0;
}