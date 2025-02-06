#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParsedToken *parse_gif(const HParseResult *p, void *user_data) {
    printf("Parsed GIF successfully\n");
    return h_token_value(p->ast);
}

HParser *create_gif_parser() {
    HParser *header = h_sequence(
        h_literal("GIF", 3, true),  // Signature
        h_choice(
            h_literal("89a", 3, true),
            h_literal("87a", 3, true)
        ),  // Version
        NULL
    );

    HParser *logical_screen_descriptor = h_sequence(
        h_uint16(),  // Logical Screen Width
        h_uint16(),  // Logical Screen Height
        h_bits(8, 0),  // Packed Fields
        h_uint8(),  // Background Color Index
        h_uint8(),  // Pixel Aspect Ratio
        NULL
    );

    HParser *color_table = h_repeat_n(h_sequence(
        h_uint8(), h_uint8(), h_uint8()
    ), 1 << 3);  // Table size is determined from packed fields

    HParser *image_descriptor = h_sequence(
        h_ch(0x2C),  // Image Separator
        h_uint16(),  // Image Left Position
        h_uint16(),  // Image Top Position
        h_uint16(),  // Image Width
        h_uint16(),  // Image Height
        h_bits(8, 0),  // Packed Fields
        NULL
    );

    HParser *sub_block = h_sequence(
        h_uint8(),  // Block Size
        h_repeat(h_uint8(), h_last_uint8),
        NULL
    );

    HParser *image_data = h_sequence(
        h_uint8(),  // LZW Minimum Code Size
        h_many(sub_block),
        NULL
    );

    HParser *graphic_control_extension = h_sequence(
        h_ch(0x21),  // Extension Introducer
        h_ch(0xF9),  // Graphic Control Label
        h_uint8(),   // Block Size
        h_bits(8, 0),  // Packed Fields
        h_uint16(),  // Delay Time
        h_uint8(),   // Transparent Color Index
        h_ch(0x00),  // Block Terminator
        NULL
    );

    HParser *comment_extension = h_sequence(
        h_ch(0x21),  // Extension Introducer
        h_ch(0xFE),  // Comment Label
        h_many(sub_block),
        h_ch(0x00),  // Block Terminator
        NULL
    );

    HParser *plain_text_extension = h_sequence(
        h_ch(0x21),  // Extension Introducer
        h_ch(0x01),  // Plain Text Label
        h_uint8(),   // Block Size
        h_uint16(),  // Text Grid Left Position
        h_uint16(),  // Text Grid Top Position
        h_uint16(),  // Text Grid Width
        h_uint16(),  // Text Grid Height
        h_uint8(),   // Character Cell Width
        h_uint8(),   // Character Cell Height
        h_uint8(),   // Text Foreground Color Index
        h_uint8(),   // Text Background Color Index
        h_many(sub_block),
        h_ch(0x00),  // Block Terminator
        NULL
    );

    HParser *application_extension = h_sequence(
        h_ch(0x21),  // Extension Introducer
        h_ch(0xFF),  // Application Extension Label
        h_uint8(),   // Block Size
        h_exactly(8, h_uint8()),  // Application Identifier
        h_exactly(3, h_uint8()),  // Application Authentication Code
        h_many(sub_block),
        h_ch(0x00),  // Block Terminator
        NULL
    );

    HParser *trailer = h_ch(0x3B);

    HParser *gif_file = h_sequence(
        header,
        logical_screen_descriptor,
        h_optional(color_table),
        h_many(h_choice(
            graphic_control_extension,
            comment_extension,
            plain_text_extension,
            application_extension,
            h_sequence(
                image_descriptor,
                h_optional(color_table),
                image_data
            )
        )),
        trailer,
        NULL
    );

    return gif_file;
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <file_path>\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    const char *file_path = argv[1];
    FILE *file = fopen(file_path, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc((size_t)file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(data, 1, (size_t)file_size, file);
    fclose(file);

    HParser *gif_parser = create_gif_parser();
    HParseResult *result = h_parse(gif_parser, data, (size_t)file_size);

    if (result) {
        parse_gif(result, NULL);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse GIF file\n");
    }

    free(data);
    return 0;
}