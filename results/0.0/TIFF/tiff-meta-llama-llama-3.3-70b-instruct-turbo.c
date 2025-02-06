#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define SHORT 2
#define LONG 4
#define RATIONAL 8
#define BYTE 1
#define ASCII 1
#define UNDEFINED 1

typedef enum {
    II,
    MM
} ByteOrder;

typedef enum {
    WhiteIsZero,
    BlackIsZero,
    RGB,
    PaletteColor,
    TransparencyMask,
    CMYK,
    YCbCr
} PhotometricInterpretation;

typedef enum {
    TopLeft,
    TopRight,
    BottomRight,
    BottomLeft,
    LeftTop,
    RightTop,
    RightBottom,
    LeftBottom
} Orientation;

typedef enum {
    None,
    Inch,
    Centimeter
} ResolutionUnit;

typedef enum {
    Unspecified,
    InputDevice,
    OutputDevice,
    ColorSpaceConversion
} ProfileType;

typedef enum {
    UnsignedInteger,
    SignedInteger,
    IEEEFloatingPoint
} SampleFormat;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_offset;
} IFD_Entry;

typedef struct {
    ByteOrder byte_order;
    uint16_t version;
    uint32_t ifd_offset;
} TIFF_Header;

typedef struct {
    uint16_t number_of_entries;
    IFD_Entry entries[64];
    uint32_t next_ifd_offset;
} IFD;

typedef struct {
    uint32_t width;
    uint32_t length;
    uint16_t bits_per_sample;
    uint16_t compression;
    PhotometricInterpretation photometric_interpretation;
    uint16_t samples_per_pixel;
    uint16_t planar_configuration;
    uint32_t x_resolution;
    uint32_t y_resolution;
    ResolutionUnit resolution_unit;
    char* datetime;
    char* artist;
    char* image_description;
    char* make;
    char* model;
    char* software;
    char* datetime_original;
    char* datetime_digitized;
    uint32_t sub_ifd_offset;
    uint16_t ink_set;
    char* ink_names;
    uint16_t number_of_inks;
    uint8_t dot_range[4];
    char* target_printer;
    uint8_t extra_samples;
    SampleFormat sample_format;
    uint32_t s_min_sample_value;
    uint32_t s_max_sample_value;
    uint32_t transfer_range[2];
    uint8_t clip_path[4];
    uint16_t x_clip_path_units;
    uint16_t y_clip_path_units;
    uint16_t indexed;
    uint8_t jpeg_tables[64];
    uint8_t opi_proxy[64];
    uint32_t global_parameters_ifd;
    ProfileType profile_type;
    uint32_t fax_t4_options;
    uint32_t fax_t6_options;
    uint32_t dacs;
    uint32_t image_layer;
    char* geo_tiff_client;
    uint32_t geo_tiff_directory;
    uint16_t transparency;
    char* copyright;
    uint32_t exif_ifd_pointer;
    uint32_t gps_info_ifd_pointer;
    uint32_t interoperability_ifd_pointer;
    char* fda_time_stamp;
    char* fdu;
    uint16_t sub_file;
} TIFF_Image;

void parse_tiff_header(void* ctx, TIFF_Header* header) {
    header->byte_order = *(uint16_t*)ctx;
    ctx += SHORT;
    header->version = *(uint16_t*)ctx;
    ctx += SHORT;
    header->ifd_offset = *(uint32_t*)ctx;
}

void parse_ifd_entry(void* ctx, IFD_Entry* entry) {
    entry->tag = *(uint16_t*)ctx;
    ctx += SHORT;
    entry->type = *(uint16_t*)ctx;
    ctx += SHORT;
    entry->count = *(uint32_t*)ctx;
    ctx += LONG;
    entry->value_offset = *(uint32_t*)ctx;
}

void parse_ifd(void* ctx, IFD* ifd) {
    ifd->number_of_entries = *(uint16_t*)ctx;
    ctx += SHORT;
    for (int i = 0; i < ifd->number_of_entries; i++) {
        parse_ifd_entry(ctx, &ifd->entries[i]);
        ctx += 12;
    }
    ifd->next_ifd_offset = *(uint32_t*)ctx;
}

void parse_tiff_image(void* ctx, TIFF_Image* image) {
    image->width = *(uint32_t*)ctx;
    ctx += LONG;
    image->length = *(uint32_t*)ctx;
    ctx += LONG;
    image->bits_per_sample = *(uint16_t*)ctx;
    ctx += SHORT;
    image->compression = *(uint16_t*)ctx;
    ctx += SHORT;
    image->photometric_interpretation = *(uint16_t*)ctx;
    ctx += SHORT;
    image->samples_per_pixel = *(uint16_t*)ctx;
    ctx += SHORT;
    image->planar_configuration = *(uint16_t*)ctx;
    ctx += SHORT;
    image->x_resolution = *(uint32_t*)ctx;
    ctx += LONG;
    image->y_resolution = *(uint32_t*)ctx;
    ctx += LONG;
    image->resolution_unit = *(uint16_t*)ctx;
    ctx += SHORT;
    image->datetime = (char*)ctx;
    ctx += 20;
    image->artist = (char*)ctx;
    ctx += 64;
    image->image_description = (char*)ctx;
    ctx += 64;
    image->make = (char*)ctx;
    ctx += 64;
    image->model = (char*)ctx;
    ctx += 64;
    image->software = (char*)ctx;
    ctx += 64;
    image->datetime_original = (char*)ctx;
    ctx += 20;
    image->datetime_digitized = (char*)ctx;
    ctx += 20;
    image->sub_ifd_offset = *(uint32_t*)ctx;
    ctx += LONG;
    image->ink_set = *(uint16_t*)ctx;
    ctx += SHORT;
    image->ink_names = (char*)ctx;
    ctx += 64;
    image->number_of_inks = *(uint16_t*)ctx;
    ctx += SHORT;
    for (int i = 0; i < 4; i++) {
        image->dot_range[i] = *(uint8_t*)ctx;
        ctx += BYTE;
    }
    image->target_printer = (char*)ctx;
    ctx += 64;
    image->extra_samples = *(uint8_t*)ctx;
    ctx += BYTE;
    image->sample_format = *(uint16_t*)ctx;
    ctx += SHORT;
    image->s_min_sample_value = *(uint32_t*)ctx;
    ctx += LONG;
    image->s_max_sample_value = *(uint32_t*)ctx;
    ctx += LONG;
    for (int i = 0; i < 2; i++) {
        image->transfer_range[i] = *(uint32_t*)ctx;
        ctx += LONG;
    }
    for (int i = 0; i < 4; i++) {
        image->clip_path[i] = *(uint8_t*)ctx;
        ctx += BYTE;
    }
    image->x_clip_path_units = *(uint16_t*)ctx;
    ctx += SHORT;
    image->y_clip_path_units = *(uint16_t*)ctx;
    ctx += SHORT;
    image->indexed = *(uint16_t*)ctx;
    ctx += SHORT;
    for (int i = 0; i < 64; i++) {
        image->jpeg_tables[i] = *(uint8_t*)ctx;
        ctx += BYTE;
    }
    for (int i = 0; i < 64; i++) {
        image->opi_proxy[i] = *(uint8_t*)ctx;
        ctx += BYTE;
    }
    image->global_parameters_ifd = *(uint32_t*)ctx;
    ctx += LONG;
    image->profile_type = *(uint16_t*)ctx;
    ctx += SHORT;
    image->fax_t4_options = *(uint32_t*)ctx;
    ctx += LONG;
    image->fax_t6_options = *(uint32_t*)ctx;
    ctx += LONG;
    image->dacs = *(uint32_t*)ctx;
    ctx += LONG;
    image->image_layer = *(uint32_t*)ctx;
    ctx += LONG;
    image->geo_tiff_client = (char*)ctx;
    ctx += 64;
    image->geo_tiff_directory = *(uint32_t*)ctx;
    ctx += LONG;
    image->transparency = *(uint16_t*)ctx;
    ctx += SHORT;
    image->copyright = (char*)ctx;
    ctx += 64;
    image->exif_ifd_pointer = *(uint32_t*)ctx;
    ctx += LONG;
    image->gps_info_ifd_pointer = *(uint32_t*)ctx;
    ctx += LONG;
    image->interoperability_ifd_pointer = *(uint32_t*)ctx;
    ctx += LONG;
    image->fda_time_stamp = (char*)ctx;
    ctx += 20;
    image->fdu = (char*)ctx;
    ctx += 64;
    image->sub_file = *(uint16_t*)ctx;
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Error opening file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        printf("Error allocating memory\n");
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    TIFF_Header header;
    parse_tiff_header(buffer, &header);

    IFD ifd;
    parse_ifd(buffer + header.ifd_offset, &ifd);

    TIFF_Image image;
    parse_tiff_image(buffer + header.ifd_offset + 2 + ifd.number_of_entries * 12, &image);

    printf("TIFF Header:\n");
    printf("  Byte Order: %d\n", header.byte_order);
    printf("  Version: %d\n", header.version);
    printf("  IFD Offset: %d\n", header.ifd_offset);

    printf("IFD:\n");
    printf("  Number of Entries: %d\n", ifd.number_of_entries);
    for (int i = 0; i < ifd.number_of_entries; i++) {
        printf("  Entry %d:\n", i);
        printf("    Tag: %d\n", ifd.entries[i].tag);
        printf("    Type: %d\n", ifd.entries[i].type);
        printf("    Count: %d\n", ifd.entries[i].count);
        printf("    Value Offset: %d\n", ifd.entries[i].value_offset);
    }
    printf("  Next IFD Offset: %d\n", ifd.next_ifd_offset);

    printf("TIFF Image:\n");
    printf("  Width: %d\n", image.width);
    printf("  Length: %d\n", image.length);
    printf("  Bits per Sample: %d\n", image.bits_per_sample);
    printf("  Compression: %d\n", image.compression);
    printf("  Photometric Interpretation: %d\n", image.photometric_interpretation);
    printf("  Samples per Pixel: %d\n", image.samples_per_pixel);
    printf("  Planar Configuration: %d\n", image.planar_configuration);
    printf("  X Resolution: %d\n", image.x_resolution);
    printf("  Y Resolution: %d\n", image.y_resolution);
    printf("  Resolution Unit: %d\n", image.resolution_unit);
    printf("  DateTime: %s\n", image.datetime);
    printf("  Artist: %s\n", image.artist);
    printf("  Image Description: %s\n", image.image_description);
    printf("  Make: %s\n", image.make);
    printf("  Model: %s\n", image.model);
    printf("  Software: %s\n", image.software);
    printf("  DateTime Original: %s\n", image.datetime_original);
    printf("  DateTime Digitized: %s\n", image.datetime_digitized);
    printf("  Sub IFD Offset: %d\n", image.sub_ifd_offset);
    printf("  Ink Set: %d\n", image.ink_set);
    printf("  Ink Names: %s\n", image.ink_names);
    printf("  Number of Inks: %d\n", image.number_of_inks);
    printf("  Dot Range: ");
    for (int i = 0; i < 4; i++) {
        printf("%d ", image.dot_range[i]);
    }
    printf("\n");
    printf("  Target Printer: %s\n", image.target_printer);
    printf("  Extra Samples: %d\n", image.extra_samples);
    printf("  Sample Format: %d\n", image.sample_format);
    printf("  S Min Sample Value: %d\n", image.s_min_sample_value);
    printf("  S Max Sample Value: %d\n", image.s_max_sample_value);
    printf("  Transfer Range: ");
    for (int i = 0; i < 2; i++) {
        printf("%d ", image.transfer_range[i]);
    }
    printf("\n");
    printf("  Clip Path: ");
    for (int i = 0; i < 4; i++) {
        printf("%d ", image.clip_path[i]);
    }
    printf("\n");
    printf("  X Clip Path Units: %d\n", image.x_clip_path_units);
    printf("  Y Clip Path Units: %d\n", image.y_clip_path_units);
    printf("  Indexed: %d\n", image.indexed);
    printf("  JPEG Tables: ");
    for (int i = 0; i < 64; i++) {
        printf("%d ", image.jpeg_tables[i]);
    }
    printf("\n");
    printf("  OPI Proxy: ");
    for (int i = 0; i < 64; i++) {
        printf("%d ", image.opi_proxy[i]);
    }
    printf("\n");
    printf("  Global Parameters IFD: %d\n", image.global_parameters_ifd);
    printf("  Profile Type: %d\n", image.profile_type);
    printf("  FAX T4 Options: %d\n", image.fax_t4_options);
    printf("  FAX T6 Options: %d\n", image.fax_t6_options);
    printf("  DACS: %d\n", image.dacs);
    printf("  Image Layer: %d\n", image.image_layer);
    printf("  GeoTIFF Client: %s\n", image.geo_tiff_client);
    printf("  GeoTIFF Directory: %d\n", image.geo_tiff_directory);
    printf("  Transparency: %d\n", image.transparency);
    printf("  Copyright: %s\n", image.copyright);
    printf("  Exif IFD Pointer: %d\n", image.exif_ifd_pointer);
    printf("  GPS Info IFD Pointer: %d\n", image.gps_info_ifd_pointer);
    printf("  Interoperability IFD Pointer: %d\n", image.interoperability_ifd_pointer);
    printf("  FDA Time Stamp: %s\n", image.fda_time_stamp);
    printf("  FDU: %s\n", image.fdu);
    printf("  Sub File: %d\n", image.sub_file);

    free(buffer);
    return 0;
}