#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define TIFF data types
#define BYTE 1
#define SHORT 2
#define LONG 3
#define RATIONAL 4
#define ASCII 5

// TIFF Tag Definitions
#define ImageWidth 256
#define ImageLength 257
#define BitsPerSample 258
#define Compression 259
#define PhotometricInterpretation 262
#define StripOffsets 273
#define SamplesPerPixel 277
#define RowsPerStrip 278
#define StripByteCounts 279
#define XResolution 282
#define YResolution 283
#define PlanarConfiguration 284
#define ResolutionUnit 296
#define ColorMap 320
#define Software 305

// Helper function to parse a RATIONAL type
static HParser *parse_rational() {
    return h_sequence(h_uint32(), h_uint32(), NULL);
}

// Helper function to parse ASCII strings
static HParser *parse_ascii() {
    return h_length_value(h_uint32(), h_uint8());
}

// Parse a single TIFF tag
static HParser *parse_tag() {
    return h_sequence(
        h_uint16(), // Tag
        h_uint16(), // Type
        h_uint32(), // Length
        h_uint32(), // Value Offset (or Value if fits)
        NULL
    );
}

// Parse the TIFF header
static HParser *parse_header() {
    return h_sequence(
        h_uint16(), // Byte order (II or MM)
        h_uint16(), // TIFF magic number (42)
        h_uint32(), // Offset to first IFD
        NULL
    );
}

// Parse the Image File Directory (IFD)
static HParser *parse_ifd() {
    return h_sequence(
        h_uint16(), // Number of directory entries
        h_many(parse_tag()), // Tags
        h_uint32(), // Offset to next IFD
        NULL
    );
}

// Main parsing function for TIFF files
static HParser *parse_tiff() {
    return h_sequence(
        parse_header(),
        parse_ifd(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <TIFF file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    // Create the TIFF parser
    HParser *tiff_parser = parse_tiff();

    // Read the entire file into memory
    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    uint8_t *buf = malloc(fsize);
    fread(buf, fsize, 1, fp);
    fclose(fp);

    // Parse the buffer
    HParseResult *result = h_parse(tiff_parser, buf, fsize);
    if (result) {
        printf("TIFF parsed successfully.\n");
    } else {
        printf("Failed to parse TIFF.\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_parser_free(tiff_parser);
    free(buf);

    return 0;
}