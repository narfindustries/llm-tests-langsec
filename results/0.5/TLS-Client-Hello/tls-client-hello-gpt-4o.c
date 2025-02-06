#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

HParser *create_tls_client_hello_parser() {
    HParser *client_version = h_uint16();
    HParser *random = h_fixed_bytes(32);
    HParser *session_id_length = h_uint8();
    HParser *session_id = h_length_value(session_id_length, h_uint8());
    HParser *cipher_suites_length = h_uint16();
    HParser *cipher_suites = h_length_value(cipher_suites_length, h_repeat_n(h_uint16(), 1));
    HParser *compression_methods_length = h_uint8();
    HParser *compression_methods = h_length_value(compression_methods_length, h_repeat_n(h_uint8(), 1));

    HParser *extension_type = h_uint16();
    HParser *extension_length = h_uint16();
    HParser *extension_data = h_length_value(extension_length, h_uint8());

    HParser *extensions_length = h_uint16();
    HParser *extensions = h_length_value(extensions_length,
        h_many(h_sequence(extension_type, extension_data, NULL)));

    HParser *client_hello = h_sequence(
        client_version,
        random,
        session_id_length,
        session_id,
        cipher_suites_length,
        cipher_suites,
        compression_methods_length,
        compression_methods,
        extensions,
        NULL
    );

    return client_hello;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    unsigned char *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *parser = create_tls_client_hello_parser();
    HParseResult *result = h_parse(parser, data, file_size);

    if (result) {
        printf("Parsed TLS ClientHello successfully.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse TLS ClientHello.\n");
    }

    h_parser_destroy(parser);
    free(data);
    return EXIT_SUCCESS;
}