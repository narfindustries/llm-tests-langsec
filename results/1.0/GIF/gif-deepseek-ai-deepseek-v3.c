#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for basic types
HParser *gif_signature_parser() {
    return h_sequence(h_token((uint8_t*)"GIF", 3), NULL);
}

HParser *gif_version_parser() {
    return h_sequence(h_token((uint8_t*)"89a", 3), NULL);
}

HParser *uint16_parser() {
    return h_uint16();
}

HParser *uint8_parser() {
    return h_uint8();
}

HParser *rgb_color_parser() {
    return h_sequence(uint8_parser(), uint8_parser(), uint8_parser(), NULL);
}

HParser *global_color_table_parser(int size) {
    return h_repeat_n(rgb_color_parser(), size);
}

HParser *local_color_table_parser(int size) {
    return h_repeat_n(rgb_color_parser(), size);
}

HParser *lzw_min_code_size_parser() {
    return uint8_parser();
}

HParser *compressed_data_blocks_parser() {
    return h_many(h_sequence(uint8_parser(), h_length_value(uint8_parser(), h_uint8()), NULL));
}

HParser *graphic_control_extension_parser() {
    return h_sequence(h_uint8(), h_uint8(), uint8_parser(), uint8_parser(), uint16_parser(), uint8_parser(), NULL);
}

HParser *plain_text_extension_parser() {
    return h_sequence(h_uint8(), h_uint8(), uint8_parser(), uint16_parser(), uint16_parser(), uint16_parser(), uint16_parser(), uint8_parser(), uint8_parser(), uint8_parser(), uint8_parser(), h_length_value(uint8_parser(), h_uint8()), NULL);
}

HParser *application_extension_parser() {
    return h_sequence(h_uint8(), h_uint8(), uint8_parser(), h_length_value(uint8_parser(), h_uint8()), NULL);
}

HParser *comment_extension_parser() {
    return h_sequence(h_uint8(), h_uint8(), h_length_value(uint8_parser(), h_uint8()), NULL);
}

HParser *gif_parser() {
    return h_sequence(
        gif_signature_parser(),
        gif_version_parser(),
        uint16_parser(), uint16_parser(), uint8_parser(), uint8_parser(), uint8_parser(),
        h_optional(global_color_table_parser(256)),
        h_many(h_sequence(
            h_token((uint8_t*)"\x2C", 1), uint16_parser(), uint16_parser(), uint16_parser(), uint16_parser(), uint8_parser(),
            h_optional(local_color_table_parser(256)),
            lzw_min_code_size_parser(),
            compressed_data_blocks_parser(),
            NULL
        )),
        h_optional(graphic_control_extension_parser()),
        h_optional(plain_text_extension_parser()),
        h_optional(application_extension_parser()),
        h_optional(comment_extension_parser()),
        h_token((uint8_t*)"\x3B", 1),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        fprintf(stderr, "Failed to open file: %s\n", argv[1]);
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory for file content\n");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = gif_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (result) {
        printf("Parsing successful!\n");
    } else {
        printf("Parsing failed!\n");
    }

    free(buffer);
    return 0;
}