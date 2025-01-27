#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// GIF Parser Combinators
HParser* create_gif_parser() {
    // Header
    HParser* signature = h_token((const uint8_t*)"GIF", 3);
    HParser* version = h_token((const uint8_t*)"87a", 3);
    HParser* header = h_sequence(signature, version, NULL);

    // Logical Screen Descriptor
    HParser* screen_width = h_uint16();
    HParser* screen_height = h_uint16();
    HParser* packed_field = h_bits(8, false);
    HParser* bg_color_index = h_uint8();
    HParser* pixel_aspect_ratio = h_uint8();
    HParser* lsd = h_sequence(screen_width, screen_height, packed_field,
                            bg_color_index, pixel_aspect_ratio, NULL);

    // Global Color Table
    HParser* color = h_repeat_n(h_uint8(), 3);  // RGB
    HParser* gct = h_many(color);

    // Image Descriptor
    HParser* separator = h_ch(',');
    HParser* image_left = h_uint16();
    HParser* image_top = h_uint16();
    HParser* image_width = h_uint16();
    HParser* image_height = h_uint16();
    HParser* image_packed = h_bits(8, false);
    HParser* image_descriptor = h_sequence(separator, image_left, image_top,
                                         image_width, image_height, image_packed, NULL);

    // Local Color Table
    HParser* lct = h_many(color);

    // Image Data
    HParser* lzw_min_code_size = h_uint8();
    HParser* block_size = h_uint8();
    HParser* data_sub_block = h_length_value(block_size, h_uint8());
    HParser* data_sub_blocks = h_many(data_sub_block);
    HParser* image_data = h_sequence(lzw_min_code_size, data_sub_blocks, NULL);

    // Extension Blocks
    HParser* extension_introducer = h_ch('!');
    HParser* extension_label = h_uint8();
    HParser* extension_data = h_many(data_sub_block);
    HParser* extension = h_sequence(extension_introducer, extension_label,
                                  extension_data, NULL);

    // Trailer
    HParser* trailer = h_ch(';');

    // Complete GIF
    return h_sequence(header, lsd, gct,
                     h_many(h_choice(image_descriptor,
                                   extension,
                                   NULL)),
                     trailer,
                     NULL);
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, size, file) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser* gif_parser = create_gif_parser();
    HParseResult* result = h_parse(gif_parser, buffer, size);

    if (result) {
        printf("Successfully parsed GIF file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GIF file\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}