#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    HParser *signature;
    HParser *version;
    HParser *header;
    HParser *screen_desc;
    HParser *global_color_table;
    HParser *extension;
    HParser *image_block;
    HParser *trailer;
    HParser *gif;
} GIFParsers;

GIFParsers init_gif_parsers() {
    GIFParsers p;
    
    // Header Block
    p.signature = h_token((uint8_t*)"GIF", 3);
    p.version = h_choice(h_token((uint8_t*)"87a", 3),
                        h_token((uint8_t*)"89a", 3),
                        NULL);
    p.header = h_sequence(p.signature, p.version, NULL);

    // Logical Screen Descriptor
    HParser *screen_width = h_uint16();
    HParser *screen_height = h_uint16();
    HParser *packed_field = h_bits(8, false);
    HParser *bg_color_index = h_uint8();
    HParser *pixel_aspect_ratio = h_uint8();
    p.screen_desc = h_sequence(screen_width, screen_height, packed_field,
                              bg_color_index, pixel_aspect_ratio, NULL);

    // Global Color Table
    HParser *rgb = h_repeat_n(h_uint8(), 3);
    p.global_color_table = h_many(rgb);

    // Extension Blocks
    HParser *extension_introducer = h_ch(0x21);
    
    // Graphics Control Extension
    HParser *gce_label = h_ch(0xF9);
    HParser *gce_block_size = h_ch(4);
    HParser *gce_packed = h_bits(8, false);
    HParser *delay_time = h_uint16();
    HParser *transparent_idx = h_uint8();
    HParser *gce = h_sequence(extension_introducer, gce_label, gce_block_size,
                             gce_packed, delay_time, transparent_idx, h_ch(0), NULL);

    // Comment Extension
    HParser *comment_label = h_ch(0xFE);
    HParser *comment_data = h_many1(h_sequence(h_uint8(), h_many(h_uint8()), NULL));
    HParser *comment = h_sequence(extension_introducer, comment_label, comment_data,
                                 h_ch(0), NULL);

    // Plain Text Extension
    HParser *text_label = h_ch(0x01);
    HParser *text_block_size = h_ch(12);
    HParser *text_grid = h_repeat_n(h_uint16(), 4);
    HParser *cell_size = h_repeat_n(h_uint8(), 2);
    HParser *text_colors = h_repeat_n(h_uint8(), 2);
    HParser *text_data = h_many1(h_sequence(h_uint8(), h_many(h_uint8()), NULL));
    HParser *plain_text = h_sequence(extension_introducer, text_label, text_block_size,
                                    text_grid, cell_size, text_colors, text_data,
                                    h_ch(0), NULL);

    // Application Extension
    HParser *app_label = h_ch(0xFF);
    HParser *app_block_size = h_ch(11);
    HParser *app_id = h_repeat_n(h_uint8(), 8);
    HParser *auth_code = h_repeat_n(h_uint8(), 3);
    HParser *app_data = h_many1(h_sequence(h_uint8(), h_many(h_uint8()), NULL));
    HParser *application = h_sequence(extension_introducer, app_label, app_block_size,
                                     app_id, auth_code, app_data, h_ch(0), NULL);

    p.extension = h_choice(gce, comment, plain_text, application, NULL);

    // Image Block
    HParser *image_separator = h_ch(0x2C);
    HParser *position = h_repeat_n(h_uint16(), 2);
    HParser *dimensions = h_repeat_n(h_uint16(), 2);
    HParser *image_packed = h_bits(8, false);
    HParser *local_color_table = h_many(rgb);
    HParser *lzw_min_code = h_uint8();
    HParser *image_data = h_many1(h_sequence(h_uint8(), h_many(h_uint8()), NULL));
    p.image_block = h_sequence(image_separator, position, dimensions, image_packed,
                              local_color_table, lzw_min_code, image_data,
                              h_ch(0), NULL);

    // Trailer
    p.trailer = h_ch(0x3B);

    // Complete GIF
    p.gif = h_sequence(p.header, p.screen_desc, p.global_color_table,
                      h_many(h_choice(p.extension, p.image_block, NULL)),
                      p.trailer, NULL);

    return p;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <gif_file>\n", argv[0]);
        return 1;
    }

    FILE *f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(f);
        return 1;
    }

    if (fread(data, 1, size, f) != size) {
        perror("Failed to read file");
        free(data);
        fclose(f);
        return 1;
    }

    GIFParsers parsers = init_gif_parsers();
    HParseResult *result = h_parse(parsers.gif, data, size);

    if (result) {
        printf("Successfully parsed GIF file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GIF file\n");
    }

    free(data);
    fclose(f);
    return 0;
}