#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t handshake_type;
    uint32_t length;
    uint16_t client_version;
    uint8_t random[32];
    HParser* session_id;
    HParser* cipher_suites;
    uint8_t compression_method;
    HParser* extensions;
} TLSClientHello;

HParser* parse_handshake_type() {
    return h_ch(0x01);
}

HParser* parse_length() {
    return h_bits(24, false);
}

HParser* parse_client_version() {
    return h_ch16(0x0303);
}

HParser* parse_random() {
    return h_repeat_n(h_bits(8, false), 32);
}

HParser* parse_session_id() {
    return h_length_value(h_bits(8, false), h_bits(8, false));
}

HParser* parse_cipher_suites() {
    return h_length_value(h_bits(16, false), h_bits(16, false));
}

HParser* parse_compression_methods() {
    return h_ch(0x00);
}

HParser* parse_extensions() {
    return h_length_value(h_bits(16, false), h_bits(16, false));
}

HParser* parse_tls_client_hello() {
    return h_sequence(
        parse_handshake_type(),
        parse_length(),
        parse_client_version(),
        parse_random(),
        parse_session_id(),
        parse_cipher_suites(),
        parse_compression_methods(),
        parse_extensions(),
        NULL
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
    rewind(file);

    uint8_t* buffer = malloc(file_size);
    if (!buffer) {
        perror("Memory allocation error");
        fclose(file);
        return 1;
    }

    size_t read_size = fread(buffer, 1, file_size, file);
    fclose(file);

    if (read_size != file_size) {
        perror("File read error");
        free(buffer);
        return 1;
    }

    HParser* parser = parse_tls_client_hello();
    HParseResult* result = h_parse(parser, buffer, read_size);

    if (result && result->ast) {
        printf("TLS Client Hello parsed successfully\n");
    } else {
        printf("Parsing failed\n");
    }

    h_parse_result_free(result);
    h_destroy_parser(parser);
    free(buffer);

    return 0;
}