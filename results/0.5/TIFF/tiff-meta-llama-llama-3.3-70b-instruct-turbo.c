#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define SHORT_SIZE 2
#define LONG_SIZE 4
#define RATIONAL_SIZE 8
#define ASCII_SIZE 256

typedef enum {
    PHOTOMETRIC_INTERPRETATION_WHITE_IS_ZERO,
    PHOTOMETRIC_INTERPRETATION_BLACK_IS_ZERO,
    PHOTOMETRIC_INTERPRETATION_RGB,
    PHOTOMETRIC_INTERPRETATION_PALETTE_COLOR,
    PHOTOMETRIC_INTERPRETATION_TRANSPARENCY_MASK,
    PHOTOMETRIC_INTERPRETATION_CMYK,
    PHOTOMETRIC_INTERPRETATION_YCBCR
} photometric_interpretation_t;

typedef enum {
    COMPRESSION_NONE,
    COMPRESSION_CCITT_GROUP_3,
    COMPRESSION_CCITT_GROUP_4,
    COMPRESSION_LZW,
    COMPRESSION_PACKBITS,
    COMPRESSION_JPEG_OLD_STYLE,
    COMPRESSION_JPEG_NEW_STYLE,
    COMPRESSION_DEFLATE_ADOBE_STYLE
} compression_t;

typedef enum {
    ORIENTATION_TOP_LEFT,
    ORIENTATION_TOP_RIGHT,
    ORIENTATION_BOTTOM_RIGHT,
    ORIENTATION_BOTTOM_LEFT,
    ORIENTATION_LEFT_TOP,
    ORIENTATION_RIGHT_TOP,
    ORIENTATION_RIGHT_BOTTOM,
    ORIENTATION_LEFT_BOTTOM
} orientation_t;

typedef enum {
    PLANAR_CONFIGURATION_CHUNKY,
    PLANAR_CONFIGURATION_PLANAR
} planar_configuration_t;

typedef enum {
    RESOLUTION_UNIT_NONE,
    RESOLUTION_UNIT_INCH,
    RESOLUTION_UNIT_CENTIMETER
} resolution_unit_t;

typedef enum {
    YCBCR_POSITIONING_CO_SITED,
    YCBCR_POSITIONING_CENTERED
} ycbcr_positioning_t;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_offset;
} ifd_entry_t;

typedef struct {
    uint16_t image_width;
    uint16_t image_length;
    uint16_t bits_per_sample;
    uint16_t compression;
    uint16_t photometric_interpretation;
    uint16_t orientation;
    uint16_t samples_per_pixel;
    uint16_t rows_per_strip;
    uint32_t strip_offsets;
    uint32_t strip_byte_counts;
    uint16_t planar_configuration;
    char page_name[ASCII_SIZE];
    uint32_t x_resolution_numerator;
    uint32_t x_resolution_denominator;
    uint32_t y_resolution_numerator;
    uint32_t y_resolution_denominator;
    uint16_t resolution_unit;
    char date_time[ASCII_SIZE];
    char artist[ASCII_SIZE];
    char host_computer[ASCII_SIZE];
    char image_description[ASCII_SIZE];
    char make[ASCII_SIZE];
    char model[ASCII_SIZE];
    char software[ASCII_SIZE];
    char date_time_original[ASCII_SIZE];
    char date_time_digitized[ASCII_SIZE];
    uint16_t color_map[3 * 256];
    uint16_t halftone_hints[2];
    uint16_t tile_width;
    uint16_t tile_length;
    uint32_t tile_offsets;
    uint32_t tile_byte_counts;
    uint32_t sub_ifds;
    uint16_t transfer_function[256];
    uint32_t white_point[2];
    uint32_t primary_chromaticities[6];
    uint32_t ycbcr_coefficients[3];
    uint16_t ycbcr_sub_sampling;
    uint16_t ycbcr_positioning;
} tiff_header_t;

ifd_entry_t ifd_entry_parser(parser_t *parser) {
    uint16_t tag = parser_uint16(parser);
    uint16_t type = parser_uint16(parser);
    uint32_t count = parser_uint32(parser);
    uint32_t value_offset = parser_uint32(parser);
    ifd_entry_t entry;
    entry.tag = tag;
    entry.type = type;
    entry.count = count;
    entry.value_offset = value_offset;
    return entry;
}

uint16_t short_parser(parser_t *parser) {
    return parser_uint16(parser);
}

uint32_t long_parser(parser_t *parser) {
    return parser_uint32(parser);
}

uint64_t rational_parser(parser_t *parser) {
    uint32_t numerator = parser_uint32(parser);
    uint32_t denominator = parser_uint32(parser);
    return (numerator << 32) | denominator;
}

char* ascii_parser(parser_t *parser) {
    char str[ASCII_SIZE];
    parser_bytes(parser, str, ASCII_SIZE);
    return str;
}

photometric_interpretation_t photometric_interpretation_parser(parser_t *parser) {
    uint16_t value = parser_uint16(parser);
    switch (value) {
        case 0:
            return PHOTOMETRIC_INTERPRETATION_WHITE_IS_ZERO;
        case 1:
            return PHOTOMETRIC_INTERPRETATION_BLACK_IS_ZERO;
        case 2:
            return PHOTOMETRIC_INTERPRETATION_RGB;
        case 3:
            return PHOTOMETRIC_INTERPRETATION_PALETTE_COLOR;
        case 4:
            return PHOTOMETRIC_INTERPRETATION_TRANSPARENCY_MASK;
        case 5:
            return PHOTOMETRIC_INTERPRETATION_CMYK;
        case 6:
            return PHOTOMETRIC_INTERPRETATION_YCBCR;
        default:
            return -1;
    }
}

compression_t compression_parser(parser_t *parser) {
    uint16_t value = parser_uint16(parser);
    switch (value) {
        case 1:
            return COMPRESSION_NONE;
        case 2:
            return COMPRESSION_CCITT_GROUP_3;
        case 3:
            return COMPRESSION_CCITT_GROUP_4;
        case 4:
            return COMPRESSION_LZW;
        case 5:
            return COMPRESSION_PACKBITS;
        case 6:
            return COMPRESSION_JPEG_OLD_STYLE;
        case 7:
            return COMPRESSION_JPEG_NEW_STYLE;
        case 8:
            return COMPRESSION_DEFLATE_ADOBE_STYLE;
        default:
            return -1;
    }
}

orientation_t orientation_parser(parser_t *parser) {
    uint16_t value = parser_uint16(parser);
    switch (value) {
        case 1:
            return ORIENTATION_TOP_LEFT;
        case 2:
            return ORIENTATION_TOP_RIGHT;
        case 3:
            return ORIENTATION_BOTTOM_RIGHT;
        case 4:
            return ORIENTATION_BOTTOM_LEFT;
        case 5:
            return ORIENTATION_LEFT_TOP;
        case 6:
            return ORIENTATION_RIGHT_TOP;
        case 7:
            return ORIENTATION_RIGHT_BOTTOM;
        case 8:
            return ORIENTATION_LEFT_BOTTOM;
        default:
            return -1;
    }
}

planar_configuration_t planar_configuration_parser(parser_t *parser) {
    uint16_t value = parser_uint16(parser);
    switch (value) {
        case 1:
            return PLANAR_CONFIGURATION_CHUNKY;
        case 2:
            return PLANAR_CONFIGURATION_PLANAR;
        default:
            return -1;
    }
}

resolution_unit_t resolution_unit_parser(parser_t *parser) {
    uint16_t value = parser_uint16(parser);
    switch (value) {
        case 1:
            return RESOLUTION_UNIT_NONE;
        case 2:
            return RESOLUTION_UNIT_INCH;
        case 3:
            return RESOLUTION_UNIT_CENTIMETER;
        default:
            return -1;
    }
}

ycbcr_positioning_t ycbcr_positioning_parser(parser_t *parser) {
    uint16_t value = parser_uint16(parser);
    switch (value) {
        case 1:
            return YCBCR_POSITIONING_CO_SITED;
        case 2:
            return YCBCR_POSITIONING_CENTERED;
        default:
            return -1;
    }
}

tiff_header_t tiff_header_parser(parser_t *parser) {
    tiff_header_t header;
    header.image_width = parser_uint16(parser);
    header.image_length = parser_uint16(parser);
    header.bits_per_sample = parser_uint16(parser);
    header.compression = compression_parser(parser);
    header.photometric_interpretation = photometric_interpretation_parser(parser);
    header.orientation = orientation_parser(parser);
    header.samples_per_pixel = parser_uint16(parser);
    header.rows_per_strip = parser_uint16(parser);
    header.strip_offsets = parser_uint32(parser);
    header.strip_byte_counts = parser_uint32(parser);
    header.planar_configuration = planar_configuration_parser(parser);
    parser_bytes(parser, header.page_name, ASCII_SIZE);
    header.x_resolution_numerator = parser_uint32(parser);
    header.x_resolution_denominator = parser_uint32(parser);
    header.y_resolution_numerator = parser_uint32(parser);
    header.y_resolution_denominator = parser_uint32(parser);
    header.resolution_unit = resolution_unit_parser(parser);
    parser_bytes(parser, header.date_time, ASCII_SIZE);
    parser_bytes(parser, header.artist, ASCII_SIZE);
    parser_bytes(parser, header.host_computer, ASCII_SIZE);
    parser_bytes(parser, header.image_description, ASCII_SIZE);
    parser_bytes(parser, header.make, ASCII_SIZE);
    parser_bytes(parser, header.model, ASCII_SIZE);
    parser_bytes(parser, header.software, ASCII_SIZE);
    parser_bytes(parser, header.date_time_original, ASCII_SIZE);
    parser_bytes(parser, header.date_time_digitized, ASCII_SIZE);
    for (int i = 0; i < 3 * 256; i++) {
        header.color_map[i] = parser_uint16(parser);
    }
    for (int i = 0; i < 2; i++) {
        header.halftone_hints[i] = parser_uint16(parser);
    }
    header.tile_width = parser_uint16(parser);
    header.tile_length = parser_uint16(parser);
    header.tile_offsets = parser_uint32(parser);
    header.tile_byte_counts = parser_uint32(parser);
    header.sub_ifds = parser_uint32(parser);
    for (int i = 0; i < 256; i++) {
        header.transfer_function[i] = parser_uint16(parser);
    }
    for (int i = 0; i < 2; i++) {
        header.white_point[i] = parser_uint32(parser);
    }
    for (int i = 0; i < 6; i++) {
        header.primary_chromaticities[i] = parser_uint32(parser);
    }
    for (int i = 0; i < 3; i++) {
        header.ycbcr_coefficients[i] = parser_uint32(parser);
    }
    header.ycbcr_sub_sampling = parser_uint16(parser);
    header.ycbcr_positioning = ycbcr_positioning_parser(parser);
    return header;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }
    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file %s\n", argv[1]);
        return 1;
    }
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }
    fread(data, 1, file_size, file);
    fclose(file);
    parser_t *parser = parser_init(data, file_size);
    tiff_header_t header = tiff_header_parser(parser);
    printf("Image Width: %u\n", header.image_width);
    printf("Image Length: %u\n", header.image_length);
    printf("Bits Per Sample: %u\n", header.bits_per_sample);
    printf("Compression: %u\n", header.compression);
    printf("Photometric Interpretation: %u\n", header.photometric_interpretation);
    printf("Orientation: %u\n", header.orientation);
    printf("Samples Per Pixel: %u\n", header.samples_per_pixel);
    printf("Rows Per Strip: %u\n", header.rows_per_strip);
    printf("Strip Offsets: %u\n", header.strip_offsets);
    printf("Strip Byte Counts: %u\n", header.strip_byte_counts);
    printf("Planar Configuration: %u\n", header.planar_configuration);
    printf("Page Name: %s\n", header.page_name);
    printf("X Resolution: %u/%u\n", header.x_resolution_numerator, header.x_resolution_denominator);
    printf("Y Resolution: %u/%u\n", header.y_resolution_numerator, header.y_resolution_denominator);
    printf("Resolution Unit: %u\n", header.resolution_unit);
    printf("Date Time: %s\n", header.date_time);
    printf("Artist: %s\n", header.artist);
    printf("Host Computer: %s\n", header.host_computer);
    printf("Image Description: %s\n", header.image_description);
    printf("Make: %s\n", header.make);
    printf("Model: %s\n", header.model);
    printf("Software: %s\n", header.software);
    printf("Date Time Original: %s\n", header.date_time_original);
    printf("Date Time Digitized: %s\n", header.date_time_digitized);
    for (int i = 0; i < 3 * 256; i++) {
        printf("Color Map %d: %u\n", i, header.color_map[i]);
    }
    for (int i = 0; i < 2; i++) {
        printf("Halftone Hints %d: %u\n", i, header.halftone_hints[i]);
    }
    printf("Tile Width: %u\n", header.tile_width);
    printf("Tile Length: %u\n", header.tile_length);
    printf("Tile Offsets: %u\n", header.tile_offsets);
    printf("Tile Byte Counts: %u\n", header.tile_byte_counts);
    printf("Sub IFDs: %u\n", header.sub_ifds);
    for (int i = 0; i < 256; i++) {
        printf("Transfer Function %d: %u\n", i, header.transfer_function[i]);
    }
    for (int i = 0; i < 2; i++) {
        printf("White Point %d: %u\n", i, header.white_point[i]);
    }
    for (int i = 0; i < 6; i++) {
        printf("Primary Chromaticities %d: %u\n", i, header.primary_chromaticities[i]);
    }
    for (int i = 0; i < 3; i++) {
        printf("YCbCr Coefficients %d: %u\n", i, header.ycbcr_coefficients[i]);
    }
    printf("YCbCr Sub Sampling: %u\n", header.ycbcr_sub_sampling);
    printf("YCbCr Positioning: %u\n", header.ycbcr_positioning);
    free(data);
    return 0;
}