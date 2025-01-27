#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the basic building blocks of the GIF format
static HParser *g_block_size;
static HParser *g_sub_block;
static HParser *g_extension;
static HParser *g_image_descriptor;
static HParser *g_image_data;

// Initialize parsers for various components
static void init_parsers() {
    g_block_size = h_uint8();
    g_sub_block = h_length_value(g_block_size, h_bytes(g_block_size));

    g_extension = h_sequence(h_uint8(), // Extension Introducer
                             h_uint8(), // Label
                             h_many(g_sub_block), // Data Sub-blocks
                             NULL);

    g_image_descriptor = h_sequence(h_uint8(), // Image Separator
                                    h_uint16(), // Image Left Position
                                    h_uint16(), // Image Top Position
                                    h_uint16(), // Image Width
                                    h_uint16(), // Image Height
                                    h_uint8(), // Packed Fields
                                    NULL);

    g_image_data = h_sequence(h_uint8(), // LZW Minimum Code Size
                              h_many(g_sub_block), // Image Data Sub-blocks
                              NULL);
}

// Define the main GIF parser
static HParser *gif_parser() {
    init_parsers();

    HParser *header = h_sequence(h_ch_range('G', 'G'), h_ch_range('I', 'I'), h_ch_range('F', 'F'),
                                 h_ch_range('8', '9'), h_ch_range('7', '9'), h_ch_range('a', 'a'),
                                 NULL);

    HParser *logical_screen_descriptor = h_sequence(h_uint16(), // Logical Screen Width
                                                    h_uint16(), // Logical Screen Height
                                                    h_uint8(), // Packed Fields
                                                    h_uint8(), // Background Color Index
                                                    h_uint8(), // Pixel Aspect Ratio
                                                    NULL);

    HParser *global_color_table = h_optional(h_repeat_n(h_bits(24), 256)); // Optional Global Color Table

    HParser *data = h_sequence(h_many(h_choice(g_extension, g_image_descriptor, NULL)),
                               h_end_p(), // Terminator
                               NULL);

    return h_sequence(header,
                      logical_screen_descriptor,
                      global_color_table,
                      data,
                      NULL);
}

int main(int argc, char **argv) {
    HParser *parser = gif_parser();
    HParseResult *result = h_parse(parser, (const uint8_t *)argv[1], strlen(argv[1]));
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Parse failed!\n");
    }
    h_parse_result_free(result);
    h_parser_free(parser);
    return 0;
}