#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// TIFF Header
#define LITTLE_ENDIAN_MAGIC 0x4949
#define BIG_ENDIAN_MAGIC 0x4D4D
#define TIFF_MAGIC_NUMBER 0x002A

// TIFF Tags
#define WIDTH_TAG 0x0100
#define HEIGHT_TAG 0x0101
#define BITS_PER_SAMPLE_TAG 0x0102
#define COMPRESSION_TAG 0x0103
#define PHOTOMETRIC_INTERPRETATION_TAG 0x0106
#define STRIP_OFFSETS_TAG 0x0111
#define SAMPLES_PER_PIXEL_TAG 0x0115
#define ROWS_PER_STRIP_TAG 0x0116
#define STRIP_BYTE_COUNTS_TAG 0x0117
#define X_RESOLUTION_TAG 0x011A
#define Y_RESOLUTION_TAG 0x011B
#define PLANAR_CONFIGURATION_TAG 0x011C
#define RESOLUTION_UNIT_TAG 0x0128

// TIFF Data Types
#define BYTE 1
#define ASCII 2
#define SHORT 3
#define LONG 4
#define RATIONAL 5

// Parser declarations
HParser *tiff_file;
HParser *tiff_header;
HParser *ifd;
HParser *ifd_entry;
HParser *rational;

void init_parsers() {
    HParser *uint16 = h_uint16();
    HParser *uint32 = h_uint32();

    rational = h_sequence(uint32, uint32, NULL);

    ifd_entry = h_sequence(uint16, uint16, uint32, uint32, NULL);

    ifd = h_length_value(uint16, h_many(ifd_entry));

    tiff_header = h_sequence(uint16, uint16, uint32, NULL);

    tiff_file = h_sequence(tiff_header, ifd, NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tiff_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = malloc(size);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(fp);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, size, fp);
    fclose(fp);

    init_parsers();

    HParseResult *result = h_parse(tiff_file, buffer, size);
    if (result) {
        printf("TIFF file parsed successfully.\n");
    } else {
        printf("Failed to parse TIFF file.\n");
    }

    free(buffer);
    return 0;
}