#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Function Prototypes for parsers and utilities
HParser *uint8(void);
HParser *uint16(void);
HParser *uint24(void);
HParser *random_bytes(void);
HParser *session_id(void);
HParser *cipher_suites(void);
HParser *compression_methods(void);
HParser *extension(void);
HParser *extensions(void);
HParser *client_hello(void);

int main(int argc, char *argv[]) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <binary_tls_client_hello_file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (file == NULL) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    size_t file_size = (size_t)ftell(file);
    rewind(file);

    uint8_t *data = malloc(file_size);
    if (data == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParser *ch_parser = client_hello();
    HParseResult *result = h_parse(ch_parser, data, file_size);
    if (result) {
        h_pprint(stdout, result->ast, 0, 1);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Client Hello parsing failed.\n");
    }

    free(data);
    return 0;
}

HParser *uint8(void) {
    return h_uint8();
}

HParser *uint16(void) {
    return h_uint16();
}

HParser *uint24(void) {
    return h_bits(24, false);
}

HParser *random_bytes(void) {
    return h_bits(256, false); // 32 bytes
}

HParser *session_id(void) {
    return h_length_value(uint8(), h_bits(256, false)); // 32 bytes max
}

HParser *cipher_suites(void) {
    return h_length_value(uint16(), h_many1(uint16()));
}

HParser *compression_methods(void) {
    return h_length_value(uint8(), h_many1(uint8()));
}

HParser *extension(void) {
    return h_sequence(uint16(), h_length_value(uint16(), h_greedy1(h_uint8())), NULL);
}

HParser *extensions(void) {
    return h_length_value(uint16(), h_many1(extension()));
}

HParser *client_hello(void) {
    return h_sequence(
        uint16(), // Version: TLS 1.2 (0x0303)
        random_bytes(),
        session_id(),
        cipher_suites(),
        compression_methods(),
        extensions(),
        NULL
    );
}