#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define HAMMER_NO_ERROR_MESSAGES

typedef struct {
    uint8_t marker;
    uint8_t marker_length[2];
    uint8_t data[];
} marker_segment_t;

typedef struct {
    uint8_t identifier[5];
    uint8_t version[2];
    uint8_t units;
    uint16_t x_density;
    uint16_t y_density;
    uint8_t thumbnail_width;
    uint8_t thumbnail_height;
} app0_segment_t;

typedef struct {
    uint8_t identifier[6];
    uint32_t version;
    uint16_t tiff_header;
    uint32_t tiff_offset;
} app1_segment_t;

typedef struct {
    uint8_t quantization_table_number;
    uint8_t quantization_table_precision;
    uint8_t quantization_table_data[64];
} dqt_segment_t;

typedef struct {
    uint8_t sample_precision;
    uint16_t image_height;
    uint16_t image_width;
    uint8_t number_of_components;
} sof0_segment_t;

typedef struct component_s {
    uint8_t identifier;
    uint8_t horizontal_sampling_factor;
    uint8_t vertical_sampling_factor;
    uint8_t quantization_table_number;
} component_s;

typedef struct {
    uint8_t huffman_table_class;
    uint8_t huffman_table_identifier;
    uint8_t huffman_table_data[];
} dht_segment_t;

typedef struct {
    uint8_t number_of_components;
} sos_segment_t;

typedef struct sos_component_s {
    uint8_t identifier;
    uint8_t dc_huffman_table_number;
    uint8_t ac_huffman_table_number;
} sos_component_s;

void* marker_segment() {
    return (void*)hammer_seq(
        (void*)hammer_uint8(),
        (void*)hammer_array(2, (void*)hammer_uint8()),
        (void*)hammer_remaining()
    );
}

void* app0_segment() {
    return (void*)hammer_seq(
        (void*)hammer_string("JFIF\0", 5),
        (void*)hammer_uint8_array(2),
        (void*)hammer_uint8(),
        (void*)hammer_uint16(),
        (void*)hammer_uint16(),
        (void*)hammer_uint8(),
        (void*)hammer_uint8()
    );
}

void* app1_segment() {
    return (void*)hammer_seq(
        (void*)hammer_string("Exif\0\0", 6),
        (void*)hammer_uint32(),
        (void*)hammer_uint16(),
        (void*)hammer_uint32()
    );
}

void* dqt_segment() {
    return (void*)hammer_seq(
        (void*)hammer_uint8(),
        (void*)hammer_uint8(),
        (void*)hammer_uint8_array(64)
    );
}

void* sof0_segment() {
    return (void*)hammer_seq(
        (void*)hammer_uint8(),
        (void*)hammer_uint16(),
        (void*)hammer_uint16(),
        (void*)hammer_uint8()
    );
}

void* dht_segment() {
    return (void*)hammer_seq(
        (void*)hammer_uint8(),
        (void*)hammer_uint8(),
        (void*)hammer_remaining()
    );
}

void* sos_segment() {
    return (void*)hammer_seq(
        (void*)hammer_uint8()
    );
}

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        printf("Failed to open input file\n");
        return 1;
    }

    void* jpeg_parser = hammer_choice(
        (void*)marker_segment(),
        (void*)app0_segment(),
        (void*)app1_segment(),
        (void*)dqt_segment(),
        (void*)sof0_segment(),
        (void*)dht_segment(),
        (void*)sos_segment()
    );

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);
    uint8_t* data = malloc(file_size);
    fread(data, 1, file_size, file);
    fclose(file);

    void* result = hammer_parse(jpeg_parser, data, file_size);
    if (result) {
        printf("Successfully parsed JPEG file\n");
    } else {
        printf("Failed to parse JPEG file\n");
    }

    free(data);
    return 0;
}