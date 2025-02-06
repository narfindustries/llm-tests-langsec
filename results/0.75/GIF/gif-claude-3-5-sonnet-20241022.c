#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Forward declarations
HParser* gif_parser();
HParser* header_parser();
HParser* logical_screen_descriptor_parser();
HParser* global_color_table_parser();
HParser* image_block_parser();
HParser* extension_block_parser();
HParser* graphics_control_extension_parser();
HParser* comment_extension_parser();
HParser* plain_text_extension_parser();
HParser* application_extension_parser();
HParser* image_data_parser();

HParser* header_parser() {
    return h_sequence(h_token((uint8_t*)"GIF", 3),
                     h_token((uint8_t*)"89a", 3),
                     NULL);
}

HParser* logical_screen_descriptor_parser() {
    return h_sequence(
        h_uint16(),  // width
        h_uint16(),  // height
        h_bits(8, false),  // packed field
        h_uint8(),   // background color index
        h_uint8(),   // pixel aspect ratio
        NULL
    );
}

HParser* global_color_table_parser() {
    return h_repeat_n(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL), 256);
}

HParser* image_data_parser() {
    return h_sequence(
        h_uint8(),  // LZW minimum code size
        h_many(h_sequence(
            h_uint8(),  // block size
            h_length_value(h_uint8(), h_uint8()),  // data
            NULL
        )),
        NULL
    );
}

HParser* image_block_parser() {
    return h_sequence(
        h_ch(0x2C),  // separator
        h_uint16(),  // left position
        h_uint16(),  // top position
        h_uint16(),  // width
        h_uint16(),  // height
        h_bits(8, false),  // packed field
        h_optional(global_color_table_parser()),  // local color table
        image_data_parser(),
        NULL
    );
}

HParser* graphics_control_extension_parser() {
    return h_sequence(
        h_ch(0xF9),  // extension label
        h_ch(4),     // block size
        h_bits(8, false),  // packed field
        h_uint16(),  // delay time
        h_uint8(),   // transparent color index
        h_ch(0),     // block terminator
        NULL
    );
}

HParser* comment_extension_parser() {
    return h_sequence(
        h_ch(0xFE),  // extension label
        h_many(h_sequence(
            h_uint8(),  // block size
            h_length_value(h_uint8(), h_uint8()),  // comment data
            NULL
        )),
        h_ch(0),  // block terminator
        NULL
    );
}

HParser* plain_text_extension_parser() {
    return h_sequence(
        h_ch(0x01),  // extension label
        h_ch(12),    // block size
        h_uint16(),  // text grid left position
        h_uint16(),  // text grid top position
        h_uint16(),  // text grid width
        h_uint16(),  // text grid height
        h_uint8(),   // character cell width
        h_uint8(),   // character cell height
        h_uint8(),   // text foreground color index
        h_uint8(),   // text background color index
        h_many(h_sequence(
            h_uint8(),  // block size
            h_length_value(h_uint8(), h_uint8()),  // text data
            NULL
        )),
        h_ch(0),  // block terminator
        NULL
    );
}

HParser* application_extension_parser() {
    return h_sequence(
        h_ch(0xFF),  // extension label
        h_ch(11),    // block size
        h_length_value(h_ch(8), h_uint8()),  // application identifier
        h_length_value(h_ch(3), h_uint8()),  // authentication code
        h_many(h_sequence(
            h_uint8(),  // block size
            h_length_value(h_uint8(), h_uint8()),  // application data
            NULL
        )),
        h_ch(0),  // block terminator
        NULL
    );
}

HParser* extension_block_parser() {
    return h_sequence(
        h_ch(0x21),  // extension introducer
        h_choice(graphics_control_extension_parser(),
                comment_extension_parser(),
                plain_text_extension_parser(),
                application_extension_parser(),
                NULL),
        NULL
    );
}

HParser* gif_parser() {
    return h_sequence(
        header_parser(),
        logical_screen_descriptor_parser(),
        h_optional(global_color_table_parser()),
        h_many(h_choice(image_block_parser(),
                       extension_block_parser(),
                       NULL)),
        h_ch(0x3B),  // trailer
        NULL
    );
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

    uint8_t *input = malloc(file_size);
    if (!input) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(input, 1, file_size, file) != file_size) {
        perror("Failed to read file");
        free(input);
        fclose(file);
        return 1;
    }

    HParser *parser = gif_parser();
    HParseResult *result = h_parse(parser, input, file_size);

    if (result) {
        printf("Successfully parsed GIF file\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GIF file\n");
    }

    free(input);
    fclose(file);
    return 0;
}