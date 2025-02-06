#include <hammer/hammer.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    COMPRESSION_NONE = 1,
    COMPRESSION_CCITT_GROUP_3 = 2,
    COMPRESSION_CCITT_GROUP_4 = 3,
    COMPRESSION_LZW = 4,
    COMPRESSION_PACKBITS = 5
} compression_t;

typedef enum {
    PHOTOMETRIC_INTERPRETATION_WHITE_IS_ZERO = 0,
    PHOTOMETRIC_INTERPRETATION_BLACK_IS_ZERO = 1,
    PHOTOMETRIC_INTERPRETATION_RGB = 2,
    PHOTOMETRIC_INTERPRETATION_PALETTE_COLOR = 3,
    PHOTOMETRIC_INTERPRETATION_TRANSPARENCY_MASK = 4,
    PHOTOMETRIC_INTERPRETATION_CMYK = 5,
    PHOTOMETRIC_INTERPRETATION_YCBCR = 6
} photometric_interpretation_t;

typedef enum {
    ORIENTATION_TOP_LEFT = 1,
    ORIENTATION_TOP_RIGHT = 2,
    ORIENTATION_BOTTOM_RIGHT = 3,
    ORIENTATION_BOTTOM_LEFT = 4,
    ORIENTATION_LEFT_TOP = 5,
    ORIENTATION_RIGHT_TOP = 6,
    ORIENTATION_RIGHT_BOTTOM = 7,
    ORIENTATION_LEFT_BOTTOM = 8
} orientation_t;

typedef enum {
    SAMPLES_PER_PIXEL_GRAYSCALE = 1,
    SAMPLES_PER_PIXEL_RGB = 3,
    SAMPLES_PER_PIXEL_CMYK = 4
} samples_per_pixel_t;

typedef enum {
    PLANAR_CONFIGURATION_CHUNKY = 1,
    PLANAR_CONFIGURATION_PLANAR = 2
} planar_configuration_t;

typedef enum {
    RESOLUTION_UNIT_NONE = 1,
    RESOLUTION_UNIT_INCH = 2,
    RESOLUTION_UNIT_CENTIMETER = 3
} resolution_unit_t;

typedef enum {
    PREDICTOR_NO_PREDICTION = 1,
    PREDICTOR_HORIZONTAL_DIFFERENCING = 2
} predictor_t;

typedef enum {
    INK_SET_CMYK = 1,
    INK_SET_NOT_CMYK = 2
} ink_set_t;

typedef enum {
    SAMPLE_FORMAT_UNSIGNED_INTEGER = 1,
    SAMPLE_FORMAT_SIGNED_INTEGER = 2,
    SAMPLE_FORMAT_IEEE_FLOATING_POINT = 3,
    SAMPLE_FORMAT_UNDEFINED = 4
} sample_format_t;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t offset;
} ifd_entry_t;

typedef struct {
    uint16_t magic;
    uint32_t ifd_offset;
} tiff_header_t;

typedef struct {
    uint16_t num_entries;
    ifd_entry_t entries[64];
    uint32_t next_ifd_offset;
} ifd_t;

typedef struct {
    uint32_t width;
    uint32_t length;
    compression_t compression;
    photometric_interpretation_t photometric_interpretation;
    orientation_t orientation;
    samples_per_pixel_t samples_per_pixel;
    planar_configuration_t planar_configuration;
    uint32_t rows_per_strip;
    uint32_t strip_offsets[64];
    uint32_t strip_byte_counts[64];
    uint16_t min_sample_value;
    uint16_t max_sample_value;
    uint32_t x_resolution;
    uint32_t y_resolution;
    resolution_unit_t resolution_unit;
    uint32_t page_name_offset;
    uint32_t x_position;
    uint32_t y_position;
    uint32_t free_offsets[64];
    uint32_t free_byte_counts[64];
    uint16_t gray_response_unit;
    uint16_t gray_response_curve[64];
    uint32_t t4_options;
    uint32_t t6_options;
    uint32_t transfer_function[64];
    uint32_t software_offset;
    uint32_t datetime_offset;
    uint32_t artist_offset;
    uint32_t host_computer_offset;
    predictor_t predictor;
    uint32_t white_point[3];
    uint32_t primary_chromaticities[6];
    uint16_t color_map[64];
    uint8_t halftone_hints[64];
    uint32_t tile_width;
    uint32_t tile_length;
    uint32_t tile_offsets[64];
    uint32_t tile_byte_counts[64];
    uint32_t sub_ifds_offset;
    ink_set_t ink_set;
    uint32_t ink_names_offset;
    uint16_t num_inks;
    uint8_t dot_range[64];
    uint32_t target_printer_offset;
    uint16_t extra_samples;
    sample_format_t sample_format;
    uint32_t s_min_sample_value;
    uint32_t s_max_sample_value;
    uint32_t transfer_range[64];
    uint32_t clip_path_offset;
    uint16_t x_clip_path_units;
    uint16_t y_clip_path_units;
    uint16_t indexed;
    uint32_t jpeg_tables_offset;
    uint32_t opi_proxy_offset;
    uint32_t global_parameters_ifd_offset;
    uint32_t profile_data_offset;
    uint32_t icc_profile_offset;
    uint32_t default_crop_size[4];
} tiff_image_t;

#define HAMMER_PARSER_TIFF_HEADER(name) \
    hammer_uint16_be(0x4d4d) | hammer_uint16_be(0x4949) | \
    hammer_uint32_be(hammer_capture(tiff_header_offset))

#define HAMMER_PARSER_IFD_ENTRY(name) \
    hammer_uint16_be(hammer_capture(ifd_entry_tag)) | \
    hammer_uint16_be(hammer_capture(ifd_entry_type)) | \
    hammer_uint32_be(hammer_capture(ifd_entry_count)) | \
    (hammer_uint32_be(hammer_capture(ifd_entry_offset)) | \
     hammer_uint32_be(hammer_capture(ifd_entry_value)))

#define HAMMER_PARSER_IFD(name) \
    hammer_uint16_be(hammer_capture(ifd_num_entries)) | \
    hammer_repeat(HAMMER_PARSER_IFD_ENTRY(name), ifd_num_entries) | \
    hammer_uint32_be(hammer_capture(ifd_next_ifd_offset))

#define HAMMER_PARSER_TIFF(name) \
    HAMMER_PARSER_TIFF_HEADER(name) | \
    HAMMER_PARSER_IFD(name) | \
    hammer_uint32_be(hammer_capture(image_width)) | \
    hammer_uint32_be(hammer_capture(image_length)) | \
    hammer_uint16_be(hammer_capture(compression)) | \
    hammer_uint16_be(hammer_capture(photometric_interpretation)) | \
    hammer_uint16_be(hammer_capture(orientation)) | \
    hammer_uint16_be(hammer_capture(samples_per_pixel)) | \
    hammer_uint16_be(hammer_capture(planar_configuration)) | \
    hammer_uint32_be(hammer_capture(rows_per_strip)) | \
    hammer_repeat(hammer_uint32_be, 64) | \
    hammer_repeat(hammer_uint32_be, 64) | \
    hammer_uint16_be(hammer_capture(min_sample_value)) | \
    hammer_uint16_be(hammer_capture(max_sample_value)) | \
    hammer_uint32_be(hammer_capture(x_resolution)) | \
    hammer_uint32_be(hammer_capture(y_resolution)) | \
    hammer_uint16_be(hammer_capture(resolution_unit)) | \
    hammer_uint32_be(hammer_capture(page_name_offset)) | \
    hammer_uint32_be(hammer_capture(x_position)) | \
    hammer_uint32_be(hammer_capture(y_position)) | \
    hammer_repeat(hammer_uint32_be, 64) | \
    hammer_repeat(hammer_uint32_be, 64) | \
    hammer_uint16_be(hammer_capture(gray_response_unit)) | \
    hammer_repeat(hammer_uint16_be, 64) | \
    hammer_uint32_be(hammer_capture(t4_options)) | \
    hammer_uint32_be(hammer_capture(t6_options)) | \
    hammer_repeat(hammer_uint32_be, 64) | \
    hammer_uint32_be(hammer_capture(software_offset)) | \
    hammer_uint32_be(hammer_capture(datetime_offset)) | \
    hammer_uint32_be(hammer_capture(artist_offset)) | \
    hammer_uint32_be(hammer_capture(host_computer_offset)) | \
    hammer_uint16_be(hammer_capture(predictor)) | \
    hammer_repeat(hammer_uint32_be, 3) | \
    hammer_repeat(hammer_uint32_be, 6) | \
    hammer_repeat(hammer_uint16_be, 64) | \
    hammer_repeat(hammer_uint8, 64) | \
    hammer_uint32_be(hammer_capture(tile_width)) | \
    hammer_uint32_be(hammer_capture(tile_length)) | \
    hammer_repeat(hammer_uint32_be, 64) | \
    hammer_repeat(hammer_uint32_be, 64) | \
    hammer_uint32_be(hammer_capture(sub_ifds_offset)) | \
    hammer_uint16_be(hammer_capture(ink_set)) | \
    hammer_uint32_be(hammer_capture(ink_names_offset)) | \
    hammer_uint16_be(hammer_capture(num_inks)) | \
    hammer_repeat(hammer_uint8, 64) | \
    hammer_uint32_be(hammer_capture(target_printer_offset)) | \
    hammer_uint16_be(hammer_capture(extra_samples)) | \
    hammer_uint16_be(hammer_capture(sample_format)) | \
    hammer_uint32_be(hammer_capture(s_min_sample_value)) | \
    hammer_uint32_be(hammer_capture(s_max_sample_value)) | \
    hammer_repeat(hammer_uint32_be, 64) | \
    hammer_uint32_be(hammer_capture(clip_path_offset)) | \
    hammer_uint16_be(hammer_capture(x_clip_path_units)) | \
    hammer_uint16_be(hammer_capture(y_clip_path_units)) | \
    hammer_uint16_be(hammer_capture(indexed)) | \
    hammer_uint32_be(hammer_capture(jpeg_tables_offset)) | \
    hammer_uint32_be(hammer_capture(opi_proxy_offset)) | \
    hammer_uint32_be(hammer_capture(global_parameters_ifd_offset)) | \
    hammer_uint32_be(hammer_capture(profile_data_offset)) | \
    hammer_uint32_be(hammer_capture(icc_profile_offset)) | \
    hammer_repeat(hammer_uint32_be, 4)

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    tiff_image_t image;
    struct hammer_state *state = malloc(sizeof(struct hammer_state));
    hammer_init(state, file);

    if (!hammer_parse(state, HAMMER_PARSER_TIFF(image), &image)) {
        printf("Error parsing TIFF file\n");
        return 1;
    }

    printf("Image width: %u\n", image.width);
    printf("Image length: %u\n", image.length);
    printf("Compression: %u\n", image.compression);
    printf("Photometric interpretation: %u\n", image.photometric_interpretation);
    printf("Orientation: %u\n", image.orientation);
    printf("Samples per pixel: %u\n", image.samples_per_pixel);
    printf("Planar configuration: %u\n", image.planar_configuration);

    fclose(file);
    free(state);
    return 0;
}