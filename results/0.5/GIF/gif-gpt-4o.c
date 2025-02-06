#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_gif_parser() {
    HParser *header = h_sequence(
        h_token("GIF", 3),
        h_choice(h_token("89a", 3), h_token("87a", 3), NULL),
        NULL
    );

    HParser *logical_screen_descriptor = h_sequence(
        h_uint16(), // Logical Screen Width
        h_uint16(), // Logical Screen Height
        h_bits(8, false),  // Packed Fields
        h_uint8(),  // Background Color Index
        h_uint8(),  // Pixel Aspect Ratio
        NULL
    );

    HParser *color_table_entry = h_repeat_n(h_uint8(), 3);
    HParser *global_color_table = h_repeat_n(color_table_entry, 1 << 3);

    HParser *image_descriptor = h_sequence(
        h_uint8(), // Image Separator (0x2C)
        h_uint16(),    // Image Left Position
        h_uint16(),    // Image Top Position
        h_uint16(),    // Image Width
        h_uint16(),    // Image Height
        h_bits(8, false), // Packed Fields
        NULL
    );

    HParser *local_color_table = h_repeat_n(color_table_entry, 1 << 3);

    HParser *image_data = h_sequence(
        h_uint8(), // LZW Minimum Code Size
        h_many(h_sequence(
            h_uint8(), // Block Size
            h_data(h_uint8()), // Block Data
            NULL
        )),
        NULL
    );

    HParser *trailer = h_uint8();

    HParser *extension_block = h_sequence(
        h_uint8(), // Extension Introducer (0x21)
        h_uint8(),     // Extension Label
        h_many(h_sequence(
            h_uint8(), // Block Size
            h_data(h_uint8()), // Block Data
            NULL
        )),
        NULL
    );

    HParser *gif_parser = h_sequence(
        header,
        logical_screen_descriptor,
        h_optional(global_color_table),
        h_many(h_choice(
            h_sequence(image_descriptor, h_optional(local_color_table), image_data, NULL),
            extension_block,
            NULL
        )),
        trailer,
        NULL
    );

    return gif_parser;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *gif_parser = create_gif_parser();
    HParseResult *result = h_parse(gif_parser, buffer, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(buffer);
    h_parser_free(gif_parser);

    return EXIT_SUCCESS;
}