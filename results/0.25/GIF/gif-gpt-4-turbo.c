#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define the structure of a GIF file according to the GIF specification
HParser *gif_signature;
HParser *gif_version;
HParser *logical_screen_width;
HParser *logical_screen_height;
HParser *packed_fields;
HParser *bg_color_index;
HParser *pixel_aspect_ratio;
HParser *color_table_entry;
HParser *global_color_table;
HParser *gif_data_block;
HParser *gif_image_descriptor;
HParser *gif_extension;
HParser *gif_content;
HParser *gif_file;

void init_parsers() {
    gif_signature = h_token("GIF", 3);
    gif_version = h_choice(h_token("87a", 3), h_token("89a", 3), NULL);

    logical_screen_width = h_uint16();
    logical_screen_height = h_uint16();
    packed_fields = h_uint8();
    bg_color_index = h_uint8();
    pixel_aspect_ratio = h_uint8();

    color_table_entry = h_repeat_n(h_uint8(), 3);
    global_color_table = h_length_value(h_bits(3, false), color_table_entry);

    gif_data_block = h_length_value(h_uint8(), h_uint8());
    gif_image_descriptor = h_sequence(h_uint8(), h_uint16(), h_uint16(), h_uint8(), NULL);
    gif_extension = h_sequence(h_uint8(), gif_data_block, NULL);

    gif_content = h_many(h_choice(gif_image_descriptor, gif_extension, NULL));
    gif_file = h_sequence(gif_signature, gif_version, logical_screen_width, logical_screen_height,
                          packed_fields, bg_color_index, pixel_aspect_ratio, global_color_table,
                          gif_content, NULL);
}

int parse_gif(const uint8_t *input, size_t length) {
    HParseResult *result = h_parse(gif_file, input, length);
    if (result) {
        printf("GIF parsed successfully.\n");
        return 0;
    } else {
        fprintf(stderr, "Failed to parse GIF.\n");
        return 1;
    }
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
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
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    init_parsers();
    int result = parse_gif(buffer, file_size);
    free(buffer);

    return result;
}