#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Extension Types
#define EXT_SERVER_NAME 0
#define EXT_SUPPORTED_GROUPS 10
#define EXT_SIGNATURE_ALGORITHMS 13
#define EXT_KEY_SHARE 51
#define EXT_SUPPORTED_VERSIONS 43
#define EXT_PSK_KEY_EXCHANGE_MODES 45
#define EXT_EARLY_DATA 42
#define EXT_COOKIE 44
#define EXT_CERT_AUTHORITIES 47
#define EXT_SIG_ALGS_CERT 50
#define EXT_PADDING 21
#define EXT_RECORD_SIZE_LIMIT 28

static HParser* init_tls_parser(void) {
    // Basic field parsers
    HParser* legacy_version_p = h_uint16();
    HParser* random_p = h_repeat_n(h_uint8(), 32);
    HParser* legacy_session_id_p = h_length_value(h_uint8(), h_uint8());
    HParser* cipher_suites_p = h_length_value(h_uint16(), h_repeat_n(h_uint16(), 1));
    HParser* compression_methods_p = h_length_value(h_uint8(), h_uint8());

    // Extension parsers
    HParser* server_name_p = h_sequence(
        h_uint16(),
        h_length_value(h_uint8(),
                      h_length_value(h_uint16(), h_uint8())),
        NULL);

    HParser* supported_groups_p = h_length_value(
        h_uint16(),
        h_repeat_n(h_uint16(), 1));

    HParser* signature_algorithms_p = h_length_value(
        h_uint16(),
        h_repeat_n(h_uint16(), 1));

    HParser* key_share_p = h_length_value(
        h_uint16(),
        h_sequence(
            h_uint16(),
            h_length_value(h_uint16(), h_uint8()),
            NULL));

    HParser* supported_versions_p = h_length_value(
        h_uint8(),
        h_repeat_n(h_uint16(), 1));

    HParser* extension_p = h_choice(
        h_sequence(h_ch(EXT_SERVER_NAME), server_name_p, NULL),
        h_sequence(h_ch(EXT_SUPPORTED_GROUPS), supported_groups_p, NULL),
        h_sequence(h_ch(EXT_SIGNATURE_ALGORITHMS), signature_algorithms_p, NULL),
        h_sequence(h_ch(EXT_KEY_SHARE), key_share_p, NULL),
        h_sequence(h_ch(EXT_SUPPORTED_VERSIONS), supported_versions_p, NULL),
        NULL);

    HParser* extensions_p = h_length_value(h_uint16(), h_many(extension_p));

    return h_sequence(
        h_ch(0x01),           // handshake type
        h_uint16(),           // length (using uint16 instead of uint24)
        legacy_version_p,
        random_p,
        legacy_session_id_p,
        cipher_suites_p,
        compression_methods_p,
        extensions_p,
        NULL);
}

void print_parse_result(const HParseResult* result) {
    if (!result) {
        printf("Parse failed\n");
        return;
    }
    printf("Successfully parsed TLS ClientHello\n");
}

int main(int argc, char** argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* f = fopen(argv[1], "rb");
    if (!f) {
        perror("Failed to open file");
        return 1;
    }

    fseek(f, 0, SEEK_END);
    size_t size = ftell(f);
    fseek(f, 0, SEEK_SET);

    uint8_t* buffer = malloc(size);
    if (!buffer) {
        perror("Failed to allocate buffer");
        fclose(f);
        return 1;
    }

    if (fread(buffer, 1, size, f) != size) {
        perror("Failed to read file");
        free(buffer);
        fclose(f);
        return 1;
    }

    HParser* parser = init_tls_parser();
    HParseResult* result = h_parse(parser, buffer, size);
    print_parse_result(result);

    h_parse_result_free(result);
    free(buffer);
    fclose(f);
    return 0;
}