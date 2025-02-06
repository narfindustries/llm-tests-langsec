#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for basic types
HParser *png_signature_parser() {
    return h_sequence(h_token("\x89PNG\r\n\x1a\n", 8), NULL);
}

HParser *uint32_parser() {
    return h_uint32();
}

HParser *uint8_parser() {
    return h_uint8();
}

HParser *chunk_type_parser() {
    return h_sequence(h_ch('A'), h_ch('B'), h_ch('C'), h_ch('D'), NULL);
}

HParser *chunk_parser() {
    return h_sequence(uint32_parser(), chunk_type_parser(), h_length_value(uint32_parser(), h_uint8()), uint32_parser(), NULL);
}

HParser *ihdr_parser() {
    return h_sequence(uint32_parser(), uint32_parser(), uint8_parser(), uint8_parser(), uint8_parser(), uint8_parser(), uint8_parser(), NULL);
}

HParser *plte_parser() {
    return h_length_value(uint32_parser(), h_sequence(uint8_parser(), uint8_parser(), uint8_parser(), NULL));
}

HParser *idat_parser() {
    return h_length_value(uint32_parser(), h_uint8());
}

HParser *iend_parser() {
    return h_sequence(uint32_parser(), chunk_type_parser(), uint32_parser(), NULL);
}

HParser *png_parser() {
    return h_sequence(png_signature_parser(), h_many(chunk_parser()), NULL);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
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

    uint8_t *data = (uint8_t *)malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(png_parser(), data, file_size);
    if (result) {
        printf("Parsing succeeded!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}