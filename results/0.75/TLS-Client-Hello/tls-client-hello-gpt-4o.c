#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

HParser *random_parser;
HParser *legacy_session_id_parser;
HParser *cipher_suite_parser;
HParser *cipher_suites_parser;
HParser *legacy_compression_methods_parser;
HParser *extension_parser;
HParser *extensions_parser;
HParser *client_hello_parser;

void init_parsers() {
    random_parser = h_repeat_n(h_uint8(), 32);

    legacy_session_id_parser = h_length_value(h_uint8(), h_uint8());

    cipher_suite_parser = h_uint16();
    HParser *cipher_suites_length = h_uint16(); // Length prefix for cipher_suites
    HParser *cipher_suites_body = h_many(cipher_suite_parser);
    cipher_suites_parser = h_sequence(cipher_suites_length, cipher_suites_body, NULL);

    legacy_compression_methods_parser = h_length_value(h_uint8(), h_uint8());

    HParser *extension_type = h_uint16();
    HParser *extension_length = h_uint16(); // Length prefix for extension_data
    HParser *extension_data = h_length_value(extension_length, h_uint8()); // Generic parser for extensions
    extension_parser = h_sequence(extension_type, extension_data, NULL);
    HParser *extensions_length = h_uint16(); // Length prefix for extensions
    HParser *extensions_body = h_many(extension_parser);
    extensions_parser = h_sequence(extensions_length, extensions_body, NULL);

    client_hello_parser = h_sequence(
        h_uint16(),                  // client_version
        random_parser,               // random
        legacy_session_id_parser,    // legacy_session_id
        cipher_suites_parser,        // cipher_suites
        legacy_compression_methods_parser, // legacy_compression_methods
        extensions_parser,           // extensions
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    const char *filename = argv[1];
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation failed");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    init_parsers();

    HParseResult *result = h_parse(client_hello_parser, buffer, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(buffer);
    h_parse_free(client_hello_parser);

    return EXIT_SUCCESS;
}