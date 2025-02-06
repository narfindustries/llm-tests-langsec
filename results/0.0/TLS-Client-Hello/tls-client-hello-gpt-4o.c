#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Function to create parsers for each field in the ClientHello message
HParser *create_client_version_parser() {
    return h_sequence(h_uint8(), h_uint8(), NULL);
}

HParser *create_random_parser() {
    return h_repeat_n(h_uint8(), 32);
}

HParser *create_legacy_session_id_parser() {
    return h_length_value(h_uint8(), h_uint8());
}

HParser *create_cipher_suites_parser() {
    return h_length_value(h_uint16(), h_uint16());
}

HParser *create_legacy_compression_methods_parser() {
    return h_length_value(h_uint8(), h_uint8());
}

HParser *create_extension_parser() {
    return h_sequence(h_uint16(), h_length_value(h_uint16(), h_uint8()), NULL);
}

HParser *create_extensions_parser() {
    return h_length_value(h_uint16(), h_many(create_extension_parser()));
}

// Function to create the ClientHello parser
HParser *create_client_hello_parser() {
    return h_sequence(
        create_client_version_parser(),
        create_random_parser(),
        create_legacy_session_id_parser(),
        create_cipher_suites_parser(),
        create_legacy_compression_methods_parser(),
        create_extensions_parser(),
        NULL
    );
}

void parse_file(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *client_hello_parser = create_client_hello_parser();
    HParseResult *result = h_parse(client_hello_parser, data, file_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    free(data);
    h_parser_free(client_hello_parser);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    parse_file(argv[1]);

    return EXIT_SUCCESS;
}