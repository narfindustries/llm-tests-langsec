#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_gif_parser() {
    // Header
    HParser *signature = h_sequence(h_ch('G'), h_ch('I'), h_ch('F'), NULL);
    HParser *version = h_choice(h_token("87a", 3), h_token("89a", 3), NULL);
    HParser *header = h_sequence(signature, version, NULL);

    // Logical Screen Descriptor
    HParser *logical_screen_width = h_uint16();
    HParser *logical_screen_height = h_uint16();
    HParser *packed_fields = h_bits(8, 0);
    HParser *background_color_index = h_uint8();
    HParser *pixel_aspect_ratio = h_uint8();
    HParser *logical_screen_descriptor = h_sequence(
        logical_screen_width,
        logical_screen_height,
        packed_fields,
        background_color_index,
        pixel_aspect_ratio,
        NULL
    );

    // Global Color Table
    HParser *color_table_entry = h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL);
    HParser *global_color_table = h_many(color_table_entry);

    // Image Descriptor
    HParser *image_separator = h_ch(0x2C);
    HParser *image_left_position = h_uint16();
    HParser *image_top_position = h_uint16();
    HParser *image_width = h_uint16();
    HParser *image_height = h_uint16();
    HParser *image_packed_fields = h_bits(8, 0);
    HParser *image_descriptor = h_sequence(
        image_separator,
        image_left_position,
        image_top_position,
        image_width,
        image_height,
        image_packed_fields,
        NULL
    );

    // Local Color Table
    HParser *local_color_table = h_many(color_table_entry);

    // Image Data
    HParser *lzw_minimum_code_size = h_uint8();
    HParser *image_data_subblock = h_sequence(h_uint8(), h_many(h_uint8()), NULL);
    HParser *image_data = h_sequence(lzw_minimum_code_size, h_many(image_data_subblock), h_ch(0), NULL);

    // Trailer
    HParser *trailer = h_ch(0x3B);

    // Extension Blocks
    HParser *extension_introducer = h_ch(0x21);
    HParser *extension_label = h_uint8();
    HParser *block_size = h_uint8();
    HParser *extension_data = h_many(image_data_subblock);
    HParser *extension_block = h_sequence(
        extension_introducer,
        extension_label,
        block_size,
        extension_data,
        h_ch(0),
        NULL
    );

    // Full GIF Parser
    HParser *gif_parser = h_sequence(
        header,
        logical_screen_descriptor,
        h_optional(global_color_table),
        h_many(h_choice(image_descriptor, extension_block, NULL)),
        trailer,
        NULL
    );

    return gif_parser;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *gif_parser = create_gif_parser();
    HParseResult *result = h_parse(gif_parser, data, file_size);

    if (result) {
        printf("GIF parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GIF.\n");
    }

    free(data);
    h_parser_destroy(gif_parser);

    return EXIT_SUCCESS;
}