#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint16_t legacy_version;
    uint8_t random[32];
    uint8_t legacy_session_id_len;
    uint8_t *legacy_session_id;
    uint16_t cipher_suites_len;
    uint16_t *cipher_suites;
    uint8_t legacy_compression_methods_len;
    uint8_t *legacy_compression_methods;
    uint16_t extensions_len;
    uint8_t *extensions;
} ClientHello;

HParser *parse_client_hello() {
    HParser *legacy_version = h_uint16();
    HParser *random = h_bits(32, false);
    HParser *legacy_session_id_len = h_uint8();
    HParser *legacy_session_id = h_repeat_n(h_uint8(), h_length_value(h_uint8(), h_uint8()));
    HParser *cipher_suites_len = h_uint16();
    HParser *cipher_suites = h_repeat_n(h_uint16(), h_length_value(h_uint16(), h_uint16()));
    HParser *legacy_compression_methods_len = h_uint8();
    HParser *legacy_compression_methods = h_repeat_n(h_uint8(), h_length_value(h_uint8(), h_uint8()));
    HParser *extensions_len = h_uint16();
    HParser *extensions = h_repeat_n(h_uint8(), h_length_value(h_uint16(), h_uint8()));

    return h_sequence(
        legacy_version,
        random,
        legacy_session_id_len,
        legacy_session_id,
        cipher_suites_len,
        cipher_suites,
        legacy_compression_methods_len,
        legacy_compression_methods,
        extensions_len,
        extensions,
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

    HParser *parser = parse_client_hello();
    HParseResult *result = h_parse(parser, data, file_size);

    if (!result) {
        fprintf(stderr, "Failed to parse ClientHello\n");
        free(data);
        return 1;
    }

    ClientHello *client_hello = (ClientHello *)result->ast;
    printf("Parsed ClientHello successfully\n");

    free(client_hello->legacy_session_id);
    free(client_hello->cipher_suites);
    free(client_hello->legacy_compression_methods);
    free(client_hello->extensions);
    free(data);
    h_parse_result_free(result);

    return 0;
}