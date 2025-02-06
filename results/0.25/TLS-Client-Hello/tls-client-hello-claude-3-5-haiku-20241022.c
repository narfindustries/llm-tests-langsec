#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint8_t handshake_type;
    uint32_t length;
    uint16_t client_version;
    uint8_t random[32];
    HParsedToken* legacy_session_id;
    HParsedToken* cipher_suites;
    uint8_t compression_method;
    HParsedToken* extensions;
} TLSClientHello;

static HParser* parse_handshake_type() {
    return h_literal(h_ch(0x01));
}

static HParser* parse_length() {
    return h_bits(24, false);
}

static HParser* parse_client_version() {
    return h_literal(h_uint16(0x0303));
}

static HParser* parse_random() {
    return h_repeat_n(h_uint8(), 32);
}

static HParser* parse_legacy_session_id() {
    return h_length_value(h_uint8(), h_many(h_uint8()));
}

static HParser* parse_cipher_suites() {
    return h_length_value(h_uint16(), h_many(h_uint16()));
}

static HParser* parse_compression_methods() {
    return h_literal(h_ch(0x00));
}

static HParser* parse_extensions() {
    return h_length_value(h_uint16(), h_many(
        h_sequence(
            h_uint16(),  // extension type
            h_length_value(h_uint16(), h_many(h_uint8()))  // extension data
        )
    ));
}

static HParser* parse_tls_client_hello() {
    return h_sequence(
        parse_handshake_type(),
        parse_length(),
        parse_client_version(),
        parse_random(),
        parse_legacy_session_id(),
        parse_cipher_suites(),
        parse_compression_methods(),
        parse_extensions()
    );
}

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* file = fopen(argv[1], "rb");
    if (!file) {
        perror("Error opening file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("Error reading file");
        free(buffer);
        fclose(file);
        return 1;
    }
    fclose(file);

    HParser* parser = parse_tls_client_hello();
    HParseResult* result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        printf("Successfully parsed TLS Client Hello\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }

    h_destroy_parser(parser);
    free(buffer);
    return 0;
}