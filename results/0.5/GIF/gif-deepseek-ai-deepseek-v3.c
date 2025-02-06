#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

HParser *gif_parser() {
    HParser *signature = h_token("GIF", 3);
    HParser *version = h_choice(h_token("87a", 3), h_token("89a", 3), NULL);
    HParser *header = h_sequence(signature, version, NULL);

    HParser *logical_screen_width = h_int16();
    HParser *logical_screen_height = h_int16();
    HParser *packed_fields = h_uint8();
    HParser *background_color_index = h_uint8();
    HParser *pixel_aspect_ratio = h_uint8();
    HParser *logical_screen_descriptor = h_sequence(logical_screen_width, logical_screen_height, packed_fields, background_color_index, pixel_aspect_ratio, NULL);

    HParser *global_color_table = h_many(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL));

    HParser *image_separator = h_token("\x2C", 1);
    HParser *image_left_position = h_int16();
    HParser *image_top_position = h_int16();
    HParser *image_width = h_int16();
    HParser *image_height = h_int16();
    HParser *image_packed_fields = h_uint8();
    HParser *image_descriptor = h_sequence(image_separator, image_left_position, image_top_position, image_width, image_height, image_packed_fields, NULL);

    HParser *local_color_table = h_many(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL));

    HParser *lzw_minimum_code_size = h_uint8();
    HParser *compressed_data_blocks = h_many(h_sequence(h_uint8(), h_many(h_uint8()), NULL));
    HParser *table_based_image_data = h_sequence(lzw_minimum_code_size, compressed_data_blocks, NULL);

    HParser *graphic_control_extension = h_sequence(h_token("\x21\xF9\x04", 3), h_uint8(), h_int16(), h_uint8(), h_token("\x00", 1), NULL);

    HParser *plain_text_extension = h_sequence(h_token("\x21\x01\x0C", 3), h_int16(), h_int16(), h_int16(), h_int16(), h_uint8(), h_uint8(), h_uint8(), h_uint8(), h_many(h_sequence(h_uint8(), h_many(h_uint8()), NULL)), h_token("\x00", 1), NULL);

    HParser *application_extension = h_sequence(h_token("\x21\xFF\x0B", 3), h_token("NETSCAPE", 8), h_token("2.0", 3), h_many(h_sequence(h_uint8(), h_many(h_uint8()), NULL)), h_token("\x00", 1), NULL);

    HParser *comment_extension = h_sequence(h_token("\x21\xFE", 2), h_many(h_sequence(h_uint8(), h_many(h_uint8()), NULL)), h_token("\x00", 1), NULL);

    HParser *trailer = h_token("\x3B", 1);

    HParser *gif = h_sequence(header, logical_screen_descriptor, h_optional(global_color_table), h_many(h_sequence(h_optional(graphic_control_extension), image_descriptor, h_optional(local_color_table), table_based_image_data, NULL)), h_many(h_choice(plain_text_extension, application_extension, comment_extension, NULL)), trailer, NULL);

    return gif;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = gif_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        printf("Parsing succeeded!\n");
    } else {
        printf("Parsing failed.\n");
    }

    free(buffer);
    return 0;
}