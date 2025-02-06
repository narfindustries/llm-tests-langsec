#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Helper functions
HParser* integer_bits(size_t n, bool signed_val) {
    return h_bits(n, signed_val);
}

HParser* color_entry() {
    return h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL);
}

HParser* color_table(int size) {
    return h_repeat_n(color_entry(), size);
}

HParser* gif_signature() {
    return h_sequence(h_ch('G'), h_ch('I'), h_ch('F'), NULL);
}

HParser* gif_version() {
    return h_choice(h_sequence(h_ch('8'), h_ch('7'), h_ch('a'), NULL),
                    h_sequence(h_ch('8'), h_ch('9'), h_ch('a'), NULL), NULL);
}

HParser* logical_screen_descriptor() {
    return h_sequence(
        h_uint16(),
        h_uint16(),
        integer_bits(1, false),
        integer_bits(3, false),
        integer_bits(1, false),
        integer_bits(3, false),
        h_uint8(),
        h_uint8(),
        NULL);
}

HParser* image_descriptor() {
    return h_sequence(
        h_ch(','),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        h_uint16(),
        integer_bits(1, false),
        integer_bits(1, false),
        integer_bits(1, false),
        integer_bits(3, false),
        NULL);
}

HParser* extension_block() {
    return h_sequence(
        h_ch('!'),
        h_uint8(),
        h_length_value(h_uint8(), h_greedy()),
        NULL);
}

HParser* gif_data() {
    return h_many(h_choice(image_descriptor(), extension_block(), NULL));
}

HParser* gif_parser() {
    return h_sequence(
        gif_signature(),
        gif_version(),
        logical_screen_descriptor(),
        h_optional(color_table(256)), // simplified assumed maximum color table
        gif_data(),
        h_ch(';'), // GIF trailer
        NULL);
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <filename>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("File opening failed");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long size = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, size, fp);
    fclose(fp);

    HParser* parser = gif_parser();
    HParseResult* result = h_parse(parser, buffer, size);
    if (result) {
        printf("GIF parsed successfully.\n");
    } else {
        printf("Failed to parse GIF.\n");
    }

    h_parse_result_free(result);
    h_parser_free(parser);
    free(buffer);

    return 0;
}