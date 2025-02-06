#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for individual fields
HParser *legacy_version_parser() {
    return h_uint16();
}

HParser *random_parser() {
    return h_bits(32 * 8, false); // 32 bytes = 256 bits
}

HParser *legacy_session_id_parser() {
    return h_length_value(h_uint8(), h_bits(h_uint8() * 8, false));
}

HParser *cipher_suites_parser() {
    return h_length_value(h_uint16(), h_sequence(h_uint16(), NULL));
}

HParser *legacy_compression_methods_parser() {
    return h_length_value(h_uint8(), h_sequence(h_uint8(), NULL));
}

HParser *extension_parser() {
    return h_sequence(
        h_uint16(), // extension type
        h_length_value(h_uint16(), h_bits(h_uint16() * 8, false)), // extension data
        NULL
    );
}

HParser *extensions_parser() {
    return h_length_value(h_uint16(), h_sequence(extension_parser(), NULL));
}

// Define the main ClientHello parser
HParser *client_hello_parser() {
    return h_sequence(
        legacy_version_parser(),
        random_parser(),
        legacy_session_id_parser(),
        cipher_suites_parser(),
        legacy_compression_methods_parser(),
        extensions_parser(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
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

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = client_hello_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    free(data);
    return 0;
}