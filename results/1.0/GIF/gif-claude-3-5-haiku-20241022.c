#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
    HParser* global_color_table;
} LogicalScreenDescriptor;

typedef struct {
    uint16_t left;
    uint16_t top;
    uint16_t width;
    uint16_t height;
    uint8_t packed_fields;
    HParser* local_color_table;
} ImageDescriptor;

typedef struct {
    uint8_t disposal_method;
    uint8_t user_input_flag;
    uint8_t transparent_color_flag;
    uint16_t delay_time;
    uint8_t transparent_color_index;
} GraphicControlExtension;

HParser* gif_signature() {
    return h_literal(h_ch('G'), h_ch('I'), h_ch('F'));
}

HParser* gif_version() {
    return h_choice(
        h_literal(h_ch('8'), h_ch('7'), h_ch('a')),
        h_literal(h_ch('8'), h_ch('9'), h_ch('a')),
        NULL
    );
}

HParser* rgb_color() {
    return h_sequence(
        h_uint8(),
        h_uint8(),
        h_uint8(),
        NULL
    );
}

HParser* global_color_table(int size) {
    return h_repeat_n(rgb_color(), 1 << (size + 1));
}

HParser* logical_screen_descriptor() {
    return h_sequence(
        h_uint16(),
        h_uint16(),
        h_uint8(),
        h_uint8(),
        h_uint8(),
        NULL
    );
}

HParser* image_descriptor() {
    return h_sequence(
        h_literal_char(0x2C),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint8(),
        NULL
    );
}

HParser* graphic_control_extension() {
    return h_sequence(
        h_literal_char(0x21),
        h_literal_char(0xF9),
        h_literal_char(0x04),
        h_uint8(),
        h_uint16(),
        h_uint8(),
        h_literal_char(0x00),
        NULL
    );
}

HParser* gif_parser() {
    return h_sequence(
        gif_signature(),
        gif_version(),
        logical_screen_descriptor(),
        h_optional(global_color_table(3)),
        h_many1(h_choice(
            image_descriptor(),
            graphic_control_extension(),
            NULL
        )),
        h_literal_char(0x3B),
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Cannot open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser* parser = gif_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("GIF parsing successful\n");
    } else {
        printf("GIF parsing failed\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    free(buffer);

    return 0;
}