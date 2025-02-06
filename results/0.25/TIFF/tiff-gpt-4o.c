#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define TIFF field parsers
HParser *tiff_header_parser, *ifd_entry_parser, *ifd_parser, *tiff_parser;

// Define TIFF field tags
#define TAG_IMAGE_WIDTH 256
#define TAG_IMAGE_LENGTH 257
#define TAG_BITS_PER_SAMPLE 258
#define TAG_COMPRESSION 259
#define TAG_PHOTOMETRIC_INTERPRETATION 262
#define TAG_STRIP_OFFSETS 273
#define TAG_SAMPLES_PER_PIXEL 277
#define TAG_ROWS_PER_STRIP 278
#define TAG_STRIP_BYTE_COUNTS 279
#define TAG_X_RESOLUTION 282
#define TAG_Y_RESOLUTION 283
#define TAG_PLANAR_CONFIGURATION 284
#define TAG_RESOLUTION_UNIT 296
#define TAG_SOFTWARE 305
#define TAG_DATE_TIME 306
#define TAG_ARTIST 315
#define TAG_HOST_COMPUTER 316
#define TAG_COLOR_MAP 320
#define TAG_TILE_WIDTH 322
#define TAG_TILE_LENGTH 323
#define TAG_TILE_OFFSETS 324
#define TAG_TILE_BYTE_COUNTS 325
#define TAG_EXTRA_SAMPLES 338
#define TAG_SAMPLE_FORMAT 339

// Function to initialize parsers
void init_parsers() {
    // TIFF Header: 2 bytes for byte order, 2 bytes for 42, 4 bytes for IFD offset
    tiff_header_parser = h_sequence(
        h_choice(h_token("\x49\x49", 2), h_token("\x4D\x4D", 2), NULL),
        h_token("\x2A\x00", 2),
        h_uint32(),
        NULL
    );

    // IFD Entry: 2 bytes for tag, 2 bytes for type, 4 bytes for count, 4 bytes for value/offset
    ifd_entry_parser = h_sequence(
        h_uint16(), // Tag
        h_uint16(), // Type
        h_uint32(), // Count
        h_uint32(), // Value/Offset
        NULL
    );

    // IFD: 2 bytes for entry count, followed by entries, followed by next IFD offset
    ifd_parser = h_sequence(
        h_uint16(), // Entry count
        h_many(ifd_entry_parser), // Entries
        h_uint32(), // Next IFD offset
        NULL
    );

    // Complete TIFF Parser
    tiff_parser = h_sequence(
        tiff_header_parser,
        ifd_parser,
        NULL
    );
}

// Function to parse TIFF file
void parse_tiff(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(tiff_parser, data, file_size);
    if (result) {
        printf("TIFF file parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse TIFF file.\n");
    }

    free(data);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    init_parsers();
    parse_tiff(argv[1]);

    h_parser_free(tiff_parser);
    h_parser_free(ifd_parser);
    h_parser_free(ifd_entry_parser);
    h_parser_free(tiff_header_parser);

    return EXIT_SUCCESS;
}