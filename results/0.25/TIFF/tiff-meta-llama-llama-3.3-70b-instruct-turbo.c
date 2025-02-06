#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#define HAMMER_NO_MAIN
#define HAMMER_NO_PARSE_ERROR

typedef enum {
    BYTE_ORDER_LITTLE_ENDIAN = 0x4949,
    BYTE_ORDER_BIG_ENDIAN = 0x4D4D
} byte_order_t;

typedef enum {
    TIFF_VERSION = 0x2A
} tiff_version_t;

typedef enum {
    DATA_TYPE_BYTE = 1,
    DATA_TYPE_ASCII = 2,
    DATA_TYPE_SHORT = 3,
    DATA_TYPE_LONG = 4,
    DATA_TYPE_RATIONAL = 5
} data_type_t;

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
    RESOLUTION_UNIT_NONE = 1,
    RESOLUTION_UNIT_INCH = 2,
    RESOLUTION_UNIT_CENTIMETER = 3
} resolution_unit_t;

typedef struct {
    uint16_t tag;
    uint16_t type;
    uint32_t count;
    uint32_t value_offset;
} ifd_entry_t;

typedef struct {
    byte_order_t byte_order;
    tiff_version_t version;
    uint32_t ifd_offset;
    uint16_t num_ifd_entries;
    ifd_entry_t* ifd_entries;
} tiff_header_t;

typedef struct {
    uint16_t image_width;
    uint16_t image_length;
    uint16_t bits_per_sample;
    uint16_t compression;
    uint16_t photometric_interpretation;
    uint16_t orientation;
    uint16_t samples_per_pixel;
    uint32_t rows_per_strip;
    uint32_t* strip_offsets;
    uint32_t* strip_byte_counts;
    uint32_t x_resolution_numerator;
    uint32_t x_resolution_denominator;
    uint32_t y_resolution_numerator;
    uint32_t y_resolution_denominator;
    uint16_t resolution_unit;
    char* date_time;
    char* artist;
    char* image_description;
    char* make;
    char* model;
    char* software;
    char* copyright;
} tiff_image_t;

void* byte_order_parser(void* input) {
    uint16_t* value = (uint16_t*)input;
    if (*value == BYTE_ORDER_LITTLE_ENDIAN || *value == BYTE_ORDER_BIG_ENDIAN) {
        return (void*)value + 2;
    } else {
        return NULL;
    }
}

void* version_parser(void* input) {
    uint16_t* value = (uint16_t*)input;
    if (*value == TIFF_VERSION) {
        return (void*)value + 2;
    } else {
        return NULL;
    }
}

void* ifd_offset_parser(void* input) {
    uint32_t* value = (uint32_t*)input;
    return (void*)value + 4;
}

void* data_type_parser(void* input) {
    uint16_t* value = (uint16_t*)input;
    if (*value >= DATA_TYPE_BYTE && *value <= DATA_TYPE_RATIONAL) {
        return (void*)value + 2;
    } else {
        return NULL;
    }
}

void* compression_parser(void* input) {
    uint16_t* value = (uint16_t*)input;
    if (*value >= COMPRESSION_NONE && *value <= COMPRESSION_PACKBITS) {
        return (void*)value + 2;
    } else {
        return NULL;
    }
}

void* photometric_interpretation_parser(void* input) {
    uint16_t* value = (uint16_t*)input;
    if (*value >= PHOTOMETRIC_INTERPRETATION_WHITE_IS_ZERO && *value <= PHOTOMETRIC_INTERPRETATION_YCBCR) {
        return (void*)value + 2;
    } else {
        return NULL;
    }
}

void* orientation_parser(void* input) {
    uint16_t* value = (uint16_t*)input;
    if (*value >= ORIENTATION_TOP_LEFT && *value <= ORIENTATION_LEFT_BOTTOM) {
        return (void*)value + 2;
    } else {
        return NULL;
    }
}

void* resolution_unit_parser(void* input) {
    uint16_t* value = (uint16_t*)input;
    if (*value >= RESOLUTION_UNIT_NONE && *value <= RESOLUTION_UNIT_CENTIMETER) {
        return (void*)value + 2;
    } else {
        return NULL;
    }
}

void* ifd_entry_parser(void* input) {
    ifd_entry_t* entry = (ifd_entry_t*)input;
    return (void*)entry + sizeof(ifd_entry_t);
}

void* ifd_parser(void* input) {
    tiff_header_t* header = (tiff_header_t*)input;
    return (void*)header + sizeof(tiff_header_t);
}

void* tiff_parser(void* input) {
    return ifd_parser(input);
}

int main(int argc, char** argv) {
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
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t bytes_read = fread(buffer, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    void* parser = tiff_parser(buffer);
    if (parser == NULL) {
        printf("Error parsing file\n");
        return 1;
    }

    tiff_header_t* header = (tiff_header_t*)parser;

    printf("Byte order: 0x%x\n", header->byte_order);
    printf("Version: 0x%x\n", header->version);
    printf("IFD offset: 0x%x\n", header->ifd_offset);
    printf("Number of IFD entries: %u\n", header->num_ifd_entries);

    for (uint16_t i = 0; i < header->num_ifd_entries; i++) {
        ifd_entry_t* entry = &header->ifd_entries[i];
        printf("Tag: 0x%x, Type: 0x%x, Count: 0x%x, Value offset: 0x%x\n", entry->tag, entry->type, entry->count, entry->value_offset);
    }

    free(buffer);
    return 0;
}