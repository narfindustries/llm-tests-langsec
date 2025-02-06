#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
HParser* gif_parser;
HParser* header_parser;
HParser* logical_screen_descriptor_parser;
HParser* global_color_table_parser;
HParser* image_block_parser;
HParser* local_color_table_parser;
HParser* image_data_parser;
HParser* extension_block_parser;
HParser* graphic_control_extension_parser;
HParser* comment_extension_parser;
HParser* plain_text_extension_parser;
HParser* application_extension_parser;
HParser* data_sub_blocks_parser;

void init_parsers() {
    // Header Block
    HParser* signature = h_token((uint8_t*)"GIF", 3);
    HParser* version = h_choice(h_token((uint8_t*)"87a", 3), h_token((uint8_t*)"89a", 3), NULL);
    header_parser = h_sequence(signature, version, NULL);

    // Logical Screen Descriptor
    HParser* screen_width = h_uint16();
    HParser* screen_height = h_uint16();
    HParser* packed_field = h_bits(8, false);
    HParser* bg_color_index = h_uint8();
    HParser* pixel_aspect_ratio = h_uint8();
    logical_screen_descriptor_parser = h_sequence(screen_width, screen_height, packed_field,
                                                bg_color_index, pixel_aspect_ratio, NULL);

    // Global Color Table
    HParser* rgb_triplet = h_repeat_n(h_uint8(), 3);
    global_color_table_parser = h_many(rgb_triplet);

    // Image Data Sub-blocks
    HParser* block_size = h_uint8();
    HParser* data_block = h_length_value(block_size, h_uint8());
    data_sub_blocks_parser = h_many(data_block);

    // Image Data
    HParser* lzw_min_code_size = h_uint8();
    image_data_parser = h_sequence(lzw_min_code_size, data_sub_blocks_parser,
                                 h_uint8(), NULL); // terminator

    // Local Color Table
    local_color_table_parser = h_many(rgb_triplet);

    // Image Block
    HParser* image_separator = h_ch(0x2C);
    HParser* image_left = h_uint16();
    HParser* image_top = h_uint16();
    HParser* image_width = h_uint16();
    HParser* image_height = h_uint16();
    HParser* image_packed_field = h_bits(8, false);
    image_block_parser = h_sequence(image_separator, image_left, image_top,
                                  image_width, image_height, image_packed_field,
                                  h_optional(local_color_table_parser),
                                  image_data_parser, NULL);

    // Graphic Control Extension
    HParser* gce_introducer = h_ch(0x21);
    HParser* gce_label = h_ch(0xF9);
    HParser* gce_block_size = h_ch(0x04);
    HParser* gce_packed_field = h_bits(8, false);
    HParser* delay_time = h_uint16();
    HParser* transparent_color_index = h_uint8();
    graphic_control_extension_parser = h_sequence(gce_introducer, gce_label,
                                                gce_block_size, gce_packed_field,
                                                delay_time, transparent_color_index,
                                                h_ch(0x00), NULL);

    // Comment Extension
    HParser* comment_label = h_ch(0xFE);
    comment_extension_parser = h_sequence(gce_introducer, comment_label,
                                        data_sub_blocks_parser,
                                        h_ch(0x00), NULL);

    // Plain Text Extension
    HParser* pte_label = h_ch(0x01);
    HParser* pte_block_size = h_ch(0x0C);
    plain_text_extension_parser = h_sequence(gce_introducer, pte_label,
                                           pte_block_size,
                                           h_repeat_n(h_uint16(), 4),
                                           h_repeat_n(h_uint8(), 4),
                                           data_sub_blocks_parser,
                                           h_ch(0x00), NULL);

    // Application Extension
    HParser* app_label = h_ch(0xFF);
    HParser* app_block_size = h_ch(0x0B);
    HParser* app_id = h_repeat_n(h_uint8(), 8);
    HParser* app_auth_code = h_repeat_n(h_uint8(), 3);
    application_extension_parser = h_sequence(gce_introducer, app_label,
                                            app_block_size, app_id, app_auth_code,
                                            data_sub_blocks_parser,
                                            h_ch(0x00), NULL);

    // Extension Block
    extension_block_parser = h_choice(graphic_control_extension_parser,
                                    comment_extension_parser,
                                    plain_text_extension_parser,
                                    application_extension_parser,
                                    NULL);

    // Complete GIF
    HParser* blocks = h_many(h_choice(image_block_parser, extension_block_parser, NULL));
    HParser* trailer = h_ch(0x3B);

    gif_parser = h_sequence(header_parser,
                          logical_screen_descriptor_parser,
                          h_optional(global_color_table_parser),
                          blocks,
                          trailer,
                          NULL);
}

int main(int argc, char *argv[]) {
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
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, size, file) != size) {
        perror("Failed to read file");
        free(data);
        fclose(file);
        return 1;
    }

    init_parsers();

    HParseResult *result = h_parse(gif_parser, data, size);
    if (result) {
        printf("Successfully parsed GIF file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GIF file\n");
    }

    free(data);
    fclose(file);
    return 0;
}