#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function declarations for parsing each component
HParser *gif_parser(void);
HParser *logical_screen_descriptor_parser(void);
HParser *color_table_parser(void);
HParser *image_descriptor_parser(void);
HParser *graphics_control_extension_parser(void);
HParser *comment_extension_parser(void);
HParser *plain_text_extension_parser(void);
HParser *application_extension_parser(void);
HParser *image_data_parser(void);
HParser *gif_block_parser(void);

HParser *gif_parser(void) {
    return h_sequence(
        h_ch('G'), h_ch('I'), h_ch('F'), // Header Signature
        h_choice(h_ch('8'), h_ch('9')), h_ch('a'), // Version
        logical_screen_descriptor_parser(), // Logical Screen Descriptor
        h_optional(color_table_parser()), // Global Color Table
        h_many(gif_block_parser()), // Blocks
        h_ch(';'), // Trailer
        NULL
    );
}

HParser *logical_screen_descriptor_parser(void) {
    return h_sequence(
        h_uint16(), // Logical Screen Width
        h_uint16(), // Logical Screen Height
        h_bits(8, false), // Packed Fields
        h_uint8(), // Background Color Index
        h_uint8(), // Pixel Aspect Ratio
        NULL
    );
}

HParser *color_table_parser(void) {
    return h_many(h_sequence(
        h_uint8(), // Red
        h_uint8(), // Green
        h_uint8(), // Blue
        NULL
    ));
}

HParser *image_descriptor_parser(void) {
    return h_sequence(
        h_ch(','), // Image Separator
        h_uint16(), // Image Left Position
        h_uint16(), // Image Top Position
        h_uint16(), // Image Width
        h_uint16(), // Image Height
        h_bits(8, false), // Packed Fields
        h_optional(color_table_parser()), // Local Color Table
        image_data_parser(), // Image Data
        NULL
    );
}

HParser *graphics_control_extension_parser(void) {
    return h_sequence(
        h_ch(0x21), h_ch(0xF9), // Extension Introducer and Label
        h_ch(0x04), // Block Size
        h_bits(8, false), // Packed Fields
        h_uint16(), // Delay Time
        h_uint8(), // Transparent Color Index
        h_ch(0x00), // Block Terminator
        NULL
    );
}

HParser *comment_extension_parser(void) {
    return h_sequence(
        h_ch(0x21), h_ch(0xFE), // Extension Introducer and Label
        h_many(h_sequence(
            h_uint8(), // Block Size
            h_repeat_n(h_uint8(), 255), // Comment Data
            NULL
        )),
        h_ch(0x00), // Block Terminator
        NULL
    );
}

HParser *plain_text_extension_parser(void) {
    return h_sequence(
        h_ch(0x21), h_ch(0x01), // Extension Introducer and Label
        h_ch(0x0C), // Block Size
        h_uint16(), h_uint16(), // Text Grid Left and Top Position
        h_uint16(), h_uint16(), // Text Grid Width and Height
        h_uint8(), h_uint8(), // Character Cell Width and Height
        h_uint8(), h_uint8(), // Text Foreground and Background Color Index
        h_many(h_sequence(
            h_uint8(), // Block Size
            h_repeat_n(h_uint8(), 255), // Plain Text Data
            NULL
        )),
        h_ch(0x00), // Block Terminator
        NULL
    );
}

HParser *application_extension_parser(void) {
    return h_sequence(
        h_ch(0x21), h_ch(0xFF), // Extension Introducer and Label
        h_ch(0x0B), // Block Size
        h_repeat_n(h_uint8(), 11), // Application Identifier and Authentication Code
        h_many(h_sequence(
            h_uint8(), // Block Size
            h_repeat_n(h_uint8(), 255), // Application Data
            NULL
        )),
        h_ch(0x00), // Block Terminator
        NULL
    );
}

HParser *image_data_parser(void) {
    return h_sequence(
        h_uint8(), // LZW Minimum Code Size
        h_many(h_sequence(
            h_uint8(), // Block Size
            h_repeat_n(h_uint8(), 255), // Image Data
            NULL
        )),
        h_ch(0x00), // Block Terminator
        NULL
    );
}

HParser *gif_block_parser(void) {
    return h_choice(
        graphics_control_extension_parser(),
        comment_extension_parser(),
        plain_text_extension_parser(),
        application_extension_parser(),
        image_descriptor_parser(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(length);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, length, file);
    fclose(file);

    HParser *parser = gif_parser();
    HParseResult *result = h_parse(parser, data, length);

    if (result) {
        printf("GIF file successfully parsed.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse GIF file.\n");
    }

    free(data);
    return EXIT_SUCCESS;
}