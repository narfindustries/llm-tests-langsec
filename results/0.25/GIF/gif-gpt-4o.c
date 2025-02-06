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
    HParser *packed_fields = h_bits(8, false);
    HParser *background_color_index = h_uint8();
    HParser *pixel_aspect_ratio = h_uint8();
    HParser *logical_screen_descriptor = h_sequence(logical_screen_width, logical_screen_height, packed_fields, background_color_index, pixel_aspect_ratio, NULL);

    // Global Color Table
    HParser *color_table_entry = h_repeat_n(h_uint8(), 3);
    HParser *global_color_table = h_many(color_table_entry);

    // Image Descriptor
    HParser *image_separator = h_ch(0x2C);
    HParser *image_left_position = h_uint16();
    HParser *image_top_position = h_uint16();
    HParser *image_width = h_uint16();
    HParser *image_height = h_uint16();
    HParser *image_packed_fields = h_bits(8, false);
    HParser *local_color_table = h_many(color_table_entry);
    HParser *image_descriptor = h_sequence(image_separator, image_left_position, image_top_position, image_width, image_height, image_packed_fields, local_color_table, NULL);

    // Image Data
    HParser *lzw_minimum_code_size = h_uint8();
    HParser *block_size = h_uint8();
    HParser *block_data = h_repeat_n(h_uint8(), 255);
    HParser *image_data_blocks = h_many(h_sequence(block_size, block_data, NULL));
    HParser *image_data = h_sequence(lzw_minimum_code_size, image_data_blocks, NULL);

    // Trailer
    HParser *trailer = h_ch(0x3B);

    // Extensions
    HParser *extension_introducer = h_ch(0x21);
    HParser *extension_label = h_uint8();
    HParser *block_terminator = h_ch(0x00);

    // Graphic Control Extension
    HParser *graphic_control_extension = h_sequence(
        h_ch(0xF9), h_ch(0x04), h_bits(8, false), h_uint16(), h_uint8(), block_terminator, NULL);

    // Plain Text Extension
    HParser *plain_text_extension = h_sequence(
        h_ch(0x01), h_ch(0x0C), h_repeat_n(h_uint16(), 4), h_repeat_n(h_uint8(), 4), block_terminator, NULL);

    // Application Extension
    HParser *application_extension = h_sequence(
        h_ch(0xFF), h_ch(0x0B), h_repeat_n(h_uint8(), 11), block_terminator, NULL);

    // Comment Extension
    HParser *comment_extension = h_sequence(
        h_ch(0xFE), h_many(h_sequence(block_size, block_data, NULL)), block_terminator, NULL);

    HParser *extensions = h_many(h_sequence(extension_introducer, h_choice(graphic_control_extension, plain_text_extension, application_extension, comment_extension, NULL), NULL));

    // Complete GIF Parser
    HParser *gif_parser = h_sequence(header, logical_screen_descriptor, h_optional(global_color_table), h_many(h_sequence(image_descriptor, image_data, NULL)), h_optional(extensions), trailer, NULL);

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

    if (result != NULL) {
        printf("GIF parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GIF.\n");
    }

    h_parser_free(gif_parser);
    free(data);

    return EXIT_SUCCESS;
}