#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
HParser* init_gif_parser(void);
HParser* create_header_parser(void);
HParser* create_logical_screen_descriptor_parser(void);
HParser* create_global_color_table_parser(void);
HParser* create_extension_parser(void);
HParser* create_image_block_parser(void);
HParser* create_graphics_control_extension_parser(void);
HParser* create_comment_extension_parser(void);
HParser* create_plain_text_extension_parser(void);
HParser* create_application_extension_parser(void);

HParser* create_header_parser(void) {
    return h_sequence(
        h_token((uint8_t*)"GIF", 3),
        h_choice(h_token((uint8_t*)"87a", 3),
                h_token((uint8_t*)"89a", 3),
                NULL),
        NULL);
}

HParser* create_logical_screen_descriptor_parser(void) {
    return h_sequence(
        h_uint16(),  // width
        h_uint16(),  // height
        h_bits(8, false),   // packed field
        h_uint8(),   // background color index
        h_uint8(),   // pixel aspect ratio
        NULL);
}

HParser* create_global_color_table_parser(void) {
    return h_many(h_sequence(
        h_uint8(),  // R
        h_uint8(),  // G
        h_uint8(),  // B
        NULL));
}

HParser* create_graphics_control_extension_parser(void) {
    return h_sequence(
        h_ch(0xF9),     // extension label
        h_ch(0x04),     // block size
        h_bits(8, false),      // packed field
        h_uint16(),     // delay time
        h_uint8(),      // transparent color index
        h_ch(0x00),     // terminator
        NULL);
}

HParser* create_comment_extension_parser(void) {
    return h_sequence(
        h_ch(0xFE),     // extension label
        h_many1(h_sequence(
            h_uint8(),  // block size
            h_many(h_uint8()),  // data
            NULL)),
        h_ch(0x00),     // terminator
        NULL);
}

HParser* create_plain_text_extension_parser(void) {
    return h_sequence(
        h_ch(0x01),     // extension label
        h_ch(0x0C),     // block size
        h_uint16(),     // text grid left position
        h_uint16(),     // text grid top position
        h_uint16(),     // text grid width
        h_uint16(),     // text grid height
        h_uint8(),      // character cell width
        h_uint8(),      // character cell height
        h_uint8(),      // text foreground color index
        h_uint8(),      // text background color index
        h_many1(h_sequence(
            h_uint8(),  // block size
            h_many(h_uint8()),  // data
            NULL)),
        h_ch(0x00),     // terminator
        NULL);
}

HParser* create_application_extension_parser(void) {
    return h_sequence(
        h_ch(0xFF),     // extension label
        h_ch(0x0B),     // block size
        h_repeat_n(h_uint8(), 8),  // application identifier
        h_repeat_n(h_uint8(), 3),  // authentication code
        h_many1(h_sequence(
            h_uint8(),  // block size
            h_many(h_uint8()),  // data
            NULL)),
        h_ch(0x00),     // terminator
        NULL);
}

HParser* create_extension_parser(void) {
    return h_sequence(
        h_ch(0x21),     // extension introducer
        h_choice(
            create_graphics_control_extension_parser(),
            create_comment_extension_parser(),
            create_plain_text_extension_parser(),
            create_application_extension_parser(),
            NULL),
        NULL);
}

HParser* create_image_block_parser(void) {
    return h_sequence(
        h_ch(0x2C),     // image separator
        h_uint16(),     // left position
        h_uint16(),     // top position
        h_uint16(),     // width
        h_uint16(),     // height
        h_bits(8, false),      // packed field
        h_optional(create_global_color_table_parser()),  // local color table
        h_uint8(),      // LZW minimum code size
        h_many1(h_sequence(
            h_uint8(),  // block size
            h_many(h_uint8()),  // data
            NULL)),
        h_ch(0x00),     // block terminator
        NULL);
}

HParser* init_gif_parser(void) {
    return h_sequence(
        create_header_parser(),
        create_logical_screen_descriptor_parser(),
        h_optional(create_global_color_table_parser()),
        h_many(h_choice(
            create_extension_parser(),
            create_image_block_parser(),
            NULL)),
        h_ch(0x3B),     // trailer
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
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(buffer);
        fclose(file);
        return 1;
    }

    HParser *gif_parser = init_gif_parser();
    HParseResult *result = h_parse(gif_parser, buffer, file_size);

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