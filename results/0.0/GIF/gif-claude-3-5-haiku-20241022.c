#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    HParser* signature;
    HParser* version;
    uint16_t screen_width;
    uint16_t screen_height;
    uint8_t global_color_table_flag;
    uint8_t color_resolution;
    uint8_t sort_flag;
    uint8_t global_color_table_size;
    uint8_t background_color_index;
    uint8_t pixel_aspect_ratio;
    HParser* global_color_table;
} GifHeader;

typedef struct {
    uint16_t left_position;
    uint16_t top_position;
    uint16_t width;
    uint16_t height;
    uint8_t local_color_table_flag;
    uint8_t interlace_flag;
    uint8_t sort_flag;
    uint8_t local_color_table_size;
    HParser* local_color_table;
    HParser* image_data;
} ImageDescriptor;

HParser* gif_signature() {
    return h_literal("GIF");
}

HParser* gif_version() {
    return h_choice(
        h_literal("87a"),
        h_literal("89a"),
        NULL
    );
}

HParser* color_table_entry() {
    return h_sequence(
        h_uint8(),  // Red
        h_uint8(),  // Green
        h_uint8(),  // Blue
        NULL
    );
}

HParser* global_color_table(uint8_t size) {
    return h_repeat_n(color_table_entry(), 1 << (size + 1));
}

HParser* image_descriptor() {
    return h_sequence(
        h_ch(0x2C),  // Image Separator
        h_uint16(),  // Left Position
        h_uint16(),  // Top Position
        h_uint16(),  // Width
        h_uint16(),  // Height
        h_uint8(),   // Packed Fields
        NULL
    );
}

HParser* extension_block() {
    return h_sequence(
        h_ch(0x21),  // Extension Introducer
        h_uint8(),   // Extension Type
        h_many(h_sequence(
            h_uint8(),  // Block Size
            h_repeat_n(h_uint8(), h_get_input_length()),  // Block Data
            NULL
        )),
        NULL
    );
}

HParser* lzw_data() {
    return h_sequence(
        h_uint8(),   // LZW Minimum Code Size
        h_many(h_sequence(
            h_uint8(),  // Block Size
            h_repeat_n(h_uint8(), h_get_input_length()),  // Block Data
            NULL
        )),
        NULL
    );
}

HParser* gif_parser() {
    return h_sequence(
        gif_signature(),
        gif_version(),
        h_uint16(),  // Screen Width
        h_uint16(),  // Screen Height
        h_uint8(),   // Packed Fields
        h_uint8(),   // Background Color Index
        h_uint8(),   // Pixel Aspect Ratio
        h_optional(global_color_table(7)),
        h_many(h_choice(
            h_sequence(
                image_descriptor(),
                h_optional(global_color_table(7)),
                lzw_data(),
                NULL
            ),
            extension_block(),
            NULL
        )),
        h_ch(0x3B),  // Trailer
        NULL
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("File read error");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = gif_parser();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("GIF file parsed successfully\n");
    } else {
        printf("GIF parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}