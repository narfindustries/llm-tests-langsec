#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Forward declarations
HParser* gif_parser();
HParser* header_parser();
HParser* logical_screen_descriptor_parser();
HParser* global_color_table_parser();
HParser* image_descriptor_parser();
HParser* local_color_table_parser();
HParser* image_data_parser();
HParser* extension_block_parser();
HParser* graphic_control_extension_parser();
HParser* comment_extension_parser();
HParser* plain_text_extension_parser();
HParser* application_extension_parser();
HParser* data_sub_blocks_parser();

HParser* gif_parser() {
    return h_sequence(
        header_parser(),
        logical_screen_descriptor_parser(),
        h_optional(global_color_table_parser()),
        h_many(h_choice(
            image_descriptor_parser(),
            extension_block_parser(),
            NULL
        )),
        h_ch(0x3B),  // Trailer
        NULL
    );
}

HParser* header_parser() {
    return h_sequence(
        h_token((const uint8_t*)"GIF", 3),
        h_choice(
            h_token((const uint8_t*)"87a", 3),
            h_token((const uint8_t*)"89a", 3),
            NULL
        ),
        NULL
    );
}

HParser* logical_screen_descriptor_parser() {
    return h_sequence(
        h_uint16(),  // Width
        h_uint16(),  // Height
        h_uint8(),   // Packed Field
        h_uint8(),   // Background Color Index
        h_uint8(),   // Pixel Aspect Ratio
        NULL
    );
}

HParser* global_color_table_parser() {
    return h_many1(h_sequence(
        h_uint8(),  // R
        h_uint8(),  // G
        h_uint8(),  // B
        NULL
    ));
}

HParser* image_descriptor_parser() {
    return h_sequence(
        h_ch(0x2C),  // Image Separator
        h_uint16(),  // Left Position
        h_uint16(),  // Top Position
        h_uint16(),  // Width
        h_uint16(),  // Height
        h_uint8(),   // Packed Field
        h_optional(local_color_table_parser()),
        image_data_parser(),
        NULL
    );
}

HParser* local_color_table_parser() {
    return h_many1(h_sequence(
        h_uint8(),  // R
        h_uint8(),  // G
        h_uint8(),  // B
        NULL
    ));
}

HParser* image_data_parser() {
    return h_sequence(
        h_uint8(),  // LZW Minimum Code Size
        data_sub_blocks_parser(),
        NULL
    );
}

HParser* data_sub_blocks_parser() {
    return h_many(h_sequence(
        h_uint8(),  // Block Size
        h_length_value(h_uint8(), h_uint8()),  // Data
        NULL
    ));
}

HParser* extension_block_parser() {
    return h_choice(
        graphic_control_extension_parser(),
        comment_extension_parser(),
        plain_text_extension_parser(),
        application_extension_parser(),
        NULL
    );
}

HParser* graphic_control_extension_parser() {
    return h_sequence(
        h_ch(0x21),  // Extension Introducer
        h_ch(0xF9),  // Graphic Control Label
        h_ch(0x04),  // Block Size
        h_uint8(),   // Packed Field
        h_uint16(),  // Delay Time
        h_uint8(),   // Transparent Color Index
        h_ch(0x00),  // Block Terminator
        NULL
    );
}

HParser* comment_extension_parser() {
    return h_sequence(
        h_ch(0x21),  // Extension Introducer
        h_ch(0xFE),  // Comment Label
        data_sub_blocks_parser(),
        NULL
    );
}

HParser* plain_text_extension_parser() {
    return h_sequence(
        h_ch(0x21),  // Extension Introducer
        h_ch(0x01),  // Plain Text Label
        h_ch(0x0C),  // Block Size
        h_uint16(),  // Text Grid Left Position
        h_uint16(),  // Text Grid Top Position
        h_uint16(),  // Text Grid Width
        h_uint16(),  // Text Grid Height
        h_uint8(),   // Character Cell Width
        h_uint8(),   // Character Cell Height
        h_uint8(),   // Text Foreground Color Index
        h_uint8(),   // Text Background Color Index
        data_sub_blocks_parser(),
        NULL
    );
}

HParser* application_extension_parser() {
    return h_sequence(
        h_ch(0x21),  // Extension Introducer
        h_ch(0xFF),  // Application Label
        h_ch(0x0B),  // Block Size
        h_length_value(h_ch(0x08), h_uint8()),  // Application Identifier
        h_length_value(h_ch(0x03), h_uint8()),  // Application Authentication Code
        data_sub_blocks_parser(),
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
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(size);
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

    HParser *parser = gif_parser();
    HParseResult *result = h_parse(parser, buffer, size);

    if (result) {
        printf("GIF parsing successful\n");
        h_parse_result_free(result);
    } else {
        printf("GIF parsing failed\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}