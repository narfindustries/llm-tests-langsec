#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define TIFF tag types
#define TYPE_BYTE 1
#define TYPE_ASCII 2
#define TYPE_SHORT 3
#define TYPE_LONG 4
#define TYPE_RATIONAL 5

// Define TIFF tags
#define TAG_IMAGEWIDTH 256
#define TAG_IMAGELENGTH 257
#define TAG_BITSPERSAMPLE 258
#define TAG_COMPRESSION 259
#define TAG_PHOTOMETRICINTERPRETATION 262
#define TAG_STRIPOFFSETS 273
#define TAG_SAMPLESPERPIXEL 277
#define TAG_ROWSPERSTRIP 278
#define TAG_STRIPBYTECOUNTS 279
#define TAG_XRESOLUTION 282
#define TAG_YRESOLUTION 283
#define TAG_PLANARCONFIGURATION 284
#define TAG_RESOLUTIONUNIT 296

// Helper function to create a TIFF tag parser
HParser *tiff_tag() {
    HParser *tag_id = h_uint16();
    HParser *data_type = h_uint16();
    HParser *data_count = h_uint32();
    HParser *data_offset = h_uint32();

    return h_sequence(tag_id, data_type, data_count, data_offset, NULL);
}

// Main parser for TIFF format
HParser *tiff_parser() {
    HParser *little_endian = h_ch_range('I', 'I');
    HParser *big_endian = h_ch_range('M', 'M');
    HParser *endian = h_choice(little_endian, big_endian, NULL);
    HParser *version = h_uint16();
    HParser *offset = h_uint32();

    HParser *header = h_sequence(endian, endian, version, offset, NULL);

    HParser *tag = tiff_tag();
    HParser *tag_list = h_many(tag);

    return h_sequence(header, tag_list, NULL);
}

// Function to parse the TIFF file
void parse_tiff(const char *filename) {
    FILE *fp = fopen(filename, "rb");
    if (!fp) {
        fprintf(stderr, "Failed to open file %s\n", filename);
        return;
    }

    // Seek to the end of the file to find the file size
    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    rewind(fp);

    // Read the entire file into memory
    uint8_t *buffer = malloc(size);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(fp);
        return;
    }

    fread(buffer, 1, size, fp);
    fclose(fp);

    // Create the parser
    HParser *parser = tiff_parser();
    HParseResult *result = h_parse(parser, buffer, size);
    if (result) {
        printf("TIFF parsed successfully.\n");
        // Here you would normally do something with the result
    } else {
        printf("Failed to parse TIFF.\n");
    }

    free(buffer);
    h_parse_result_free(result);
    // h_parser_free is not needed as Hammer does not require explicit parser deallocation
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    parse_tiff(argv[1]);
    return 0;
}