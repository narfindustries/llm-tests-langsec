#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>
#include <hammer/glue.h>

// Parsing rules for the various components of the GIF format
static HParser *g_block;
static HParser *g_extension;
static HParser *g_header;
static HParser *g_logical_screen_descriptor;
static HParser *g_image_descriptor;
static HParser *g_color_table;
static HParser *g_image_data;
static HParser *g_trailer;

// Helper function to parse a color table with specified size
static HParsedToken *act_make_color_table(const HParseResult *p, void *user_data) {
    size_t size = (size_t)user_data;
    return H_MAKE_BYTES(p->ast->bytes.token, size);
}

void init_parser() {
    // Definitions for components of the GIF format
    H_UINT8 = h_uint8();
    H_UINT16 = h_uint16();
    H_BITS = h_bits(3, false);

    g_color_table = h_repeat_n(h_bits(8, false), h_bind(H_UINT8, act_make_color_table, h_length_value(g_logical_screen_descriptor, 3)));
    g_image_descriptor = h_sequence(h_ch(','), h_uint16(), h_uint16(), h_uint16(), h_uint8(), NULL);
    g_logical_screen_descriptor = h_sequence(
        h_uint16(), // Width
        h_uint16(), // Height
        h_bits(8, false), // Packed fields
        h_uint8(),   // Background color index
        h_uint8(),   // Pixel aspect ratio
        NULL);

    g_header = h_sequence(
        h_token("GIF", 3),  // Signature
        h_choice(h_token("87a", 3), h_token("89a", 3), NULL), // Version
        g_logical_screen_descriptor,
        NULL);

    g_image_data = h_sequence(
        h_uint8(), // LZW minimum code size
        h_length_value(h_uint8(), h_bytes()), // Data sub-blocks
        NULL);

    g_extension = h_sequence(
        h_ch('!'), // Introducer
        h_uint8(), // Label
        h_length_value(h_uint8(), h_bytes()), // Data sub-blocks
        NULL);

    g_block = h_choice(
        g_image_descriptor,
        g_extension,
        NULL);

    g_trailer = h_ch(';');

    // Main GIF file parser
    g_gif = h_sequence(
        g_header,
        h_many(g_block),
        g_trailer,
        NULL);
}

int main(int argc, char **argv) {
    HParser *gif_parser;
    init_parser();
    gif_parser = g_gif;

    // Read input
    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        fprintf(stderr, "Failed to open file\n");
        return EXIT_FAILURE;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    uint8_t *data = malloc(size);
    if (!data) {
        fprintf(stderr, "Failed to allocate memory\n");
        return EXIT_FAILURE;
    }
    fread(data, 1, size, fp);
    fclose(fp);

    // Parse the GIF data
    HParseResult *result = h_parse(gif_parser, data, size);
    if (result) {
        printf("Parse successful!\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        fprintf(stderr, "Parse failed!\n");
        return EXIT_FAILURE;
    }

    h_parse_result_free(result);
    free(data);
    return EXIT_SUCCESS;
}