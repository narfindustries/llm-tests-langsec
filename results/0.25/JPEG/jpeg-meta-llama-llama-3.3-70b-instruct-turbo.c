#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define HAMMER_HEADER
#define HAMMER_TAIL

typedef struct {
    uint8_t marker;
    uint8_t marker_size;
} marker_segment_t;

typedef struct {
    uint8_t length[2];
    uint8_t identifier[5];
    uint8_t version[2];
    uint8_t units;
    uint8_t x_density[2];
    uint8_t y_density[2];
    uint8_t x_thumbnail;
    uint8_t y_thumbnail;
} app0_segment_t;

typedef struct {
    uint8_t length[2];
    uint8_t identifier[5];
    uint8_t data[1]; // Flexible array member
} app1_segment_t;

typedef struct {
    uint8_t length[2];
    uint8_t table_number;
    uint8_t precision;
    uint8_t quantization_table[64];
} dqt_segment_t;

typedef struct {
    uint8_t length[2];
    uint8_t table_class;
    uint8_t table_number;
    uint8_t huffman_table[1]; // Flexible array member
} dht_segment_t;

typedef struct {
    uint8_t length[2];
    uint8_t precision;
    uint8_t image_height[2];
    uint8_t image_width[2];
    uint8_t number_of_components;
    struct {
        uint8_t component_identifier;
        uint8_t horizontal_sampling_factor;
        uint8_t vertical_sampling_factor;
        uint8_t quantization_table_number;
    } components[1]; // Flexible array member
} sof0_segment_t;

typedef struct {
    uint8_t length[2];
    uint8_t precision;
    uint8_t image_height[2];
    uint8_t image_width[2];
    uint8_t number_of_components;
    struct {
        uint8_t component_identifier;
        uint8_t horizontal_sampling_factor;
        uint8_t vertical_sampling_factor;
        uint8_t quantization_table_number;
    } components[1]; // Flexible array member
} sof2_segment_t;

typedef struct {
    uint8_t length[2];
    uint8_t restart_interval[2];
} dri_segment_t;

typedef struct {
    uint8_t length[2];
    uint8_t number_of_components;
    struct {
        uint8_t component_identifier;
        uint8_t dc_entropy_coding;
        uint8_t ac_entropy_coding;
    } components[1]; // Flexible array member
} sos_segment_t;

void jpeg_parser(void) {
    hammer_parser_t *parser = hammer_parser_create();
    hammer_rule_t *rule = hammer_rule_sequence(
        hammer_rule_literal(0xFF),
        hammer_rule_choice(
            hammer_rule_literal(0xD8),  // SOI
            hammer_rule_literal(0xE0),  // APP0
            hammer_rule_literal(0xE1),  // APP1
            hammer_rule_literal(0xDB),  // DQT
            hammer_rule_literal(0xC4),  // DHT
            hammer_rule_literal(0xC0),  // SOF0
            hammer_rule_literal(0xC2),  // SOF2
            hammer_rule_literal(0xDD),  // DRI
            hammer_rule_literal(0xDA),  // SOS
            hammer_rule_literal(0xD0),  // RST0
            hammer_rule_literal(0xD1),  // RST1
            hammer_rule_literal(0xD2),  // RST2
            hammer_rule_literal(0xD3),  // RST3
            hammer_rule_literal(0xD4),  // RST4
            hammer_rule_literal(0xD5),  // RST5
            hammer_rule_literal(0xD6),  // RST6
            hammer_rule_literal(0xD7),  // RST7
            hammer_rule_literal(0xD9)   // EOI
        ),
        hammer_rule_choice(
            hammer_rule_sequence(
                hammer_rule_literal(2),
                hammer_rule_literal(5),
                hammer_rule_literal(2),
                hammer_rule_literal(1),
                hammer_rule_literal(2),
                hammer_rule_literal(2),
                hammer_rule_literal(1),
                hammer_rule_literal(1)
            ),
            hammer_rule_sequence(
                hammer_rule_literal(2),
                hammer_rule_literal(5),
                hammer_rule_zero_or_more(hammer_rule_any())
            ),
            hammer_rule_sequence(
                hammer_rule_literal(2),
                hammer_rule_literal(1),
                hammer_rule_literal(1),
                hammer_rule_literal(64)
            ),
            hammer_rule_sequence(
                hammer_rule_literal(2),
                hammer_rule_literal(1),
                hammer_rule_literal(1),
                hammer_rule_zero_or_more(hammer_rule_any())
            ),
            hammer_rule_sequence(
                hammer_rule_literal(2),
                hammer_rule_literal(1),
                hammer_rule_literal(2),
                hammer_rule_literal(2),
                hammer_rule_literal(1),
                hammer_rule_zero_or_more(
                    hammer_rule_sequence(
                        hammer_rule_any(),
                        hammer_rule_any(),
                        hammer_rule_any(),
                        hammer_rule_any()
                    )
                )
            ),
            hammer_rule_sequence(
                hammer_rule_literal(2),
                hammer_rule_literal(1),
                hammer_rule_literal(2),
                hammer_rule_literal(2),
                hammer_rule_literal(1),
                hammer_rule_zero_or_more(
                    hammer_rule_sequence(
                        hammer_rule_any(),
                        hammer_rule_any(),
                        hammer_rule_any(),
                        hammer_rule_any()
                    )
                )
            ),
            hammer_rule_sequence(
                hammer_rule_literal(2),
                hammer_rule_literal(2)
            ),
            hammer_rule_sequence(
                hammer_rule_literal(2),
                hammer_rule_literal(1),
                hammer_rule_zero_or_more(
                    hammer_rule_sequence(
                        hammer_rule_any(),
                        hammer_rule_any(),
                        hammer_rule_any()
                    )
                )
            ),
            hammer_rule_literal(0)
        )
    );
    hammer_parser_set_rule(parser, rule);
}

int main(int argc, char **argv) {
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
    rewind(file);

    uint8_t *data = malloc(file_size);
    if (!data) {
        printf("Error allocating memory\n");
        return 1;
    }

    size_t bytes_read = fread(data, 1, file_size, file);
    if (bytes_read != file_size) {
        printf("Error reading file\n");
        return 1;
    }

    fclose(file);

    jpeg_parser();

    free(data);

    return 0;
}