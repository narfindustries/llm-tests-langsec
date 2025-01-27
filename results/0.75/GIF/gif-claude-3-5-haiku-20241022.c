#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static HParser* gif_header;
static HParser* gif_logical_screen_descriptor;
static HParser* gif_color_table;
static HParser* gif_image_descriptor;
static HParser* gif_image_data;
static HParser* gif_trailer;
static HParser* gif_parser;

static HParser* make_gif_header() {
    return h_sequence(
        h_literal("GIF"),
        h_choice(
            h_literal("87a"),
            h_literal("89a"),
            NULL
        ),
        NULL
    );
}

static HParser* make_gif_logical_screen_descriptor() {
    return h_sequence(
        h_uint16le(),   // Logical Screen Width
        h_uint16le(),   // Logical Screen Height
        h_bits(3, false),  // Global Color Table Flag + Color Resolution
        h_bits(3, false),  // Sort Flag + Global Color Table Size
        h_uint8(),      // Background Color Index
        h_uint8(),      // Pixel Aspect Ratio
        NULL
    );
}

static HParser* make_gif_color_table(bool global) {
    return h_repeat_n(
        h_sequence(
            h_uint8(),  // Red
            h_uint8(),  // Green
            h_uint8(),  // Blue
            NULL
        ),
        global ? 256 : 1
    );
}

static HParser* make_gif_image_descriptor() {
    return h_sequence(
        h_literal(","),
        h_uint16le(),   // Image Left Position
        h_uint16le(),   // Image Top Position
        h_uint16le(),   // Image Width
        h_uint16le(),   // Image Height
        h_bits(3, false),  // Local Color Table Flag + Interlace Flag
        h_bits(3, false),  // Sort Flag + Local Color Table Size
        NULL
    );
}

static HParser* make_gif_image_data() {
    return h_sequence(
        h_uint8(),      // LZW Minimum Code Size
        h_length_value(h_uint8(), h_many(h_uint8())),
        NULL
    );
}

static HParser* make_gif_trailer() {
    return h_literal(";");
}

static void init_gif_parser() {
    gif_header = make_gif_header();
    gif_logical_screen_descriptor = make_gif_logical_screen_descriptor();
    gif_color_table = make_gif_color_table(true);
    gif_image_descriptor = make_gif_image_descriptor();
    gif_image_data = make_gif_image_data();
    gif_trailer = make_gif_trailer();

    gif_parser = h_sequence(
        gif_header,
        gif_logical_screen_descriptor,
        gif_color_table,
        h_many(
            h_sequence(
                gif_image_descriptor,
                h_optional(make_gif_color_table(false)),
                gif_image_data,
                NULL
            )
        ),
        gif_trailer,
        NULL
    );
}

int main(int argc, char** argv) {
    hammock_init();
    init_gif_parser();

    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Could not open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Could not read file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParseResult* result = h_parse(gif_parser, buffer, file_size);
    free(buffer);

    if (!result) {
        fprintf(stderr, "Parsing failed\n");
        return 1;
    }

    h_parse_result_free(result);
    return 0;
}