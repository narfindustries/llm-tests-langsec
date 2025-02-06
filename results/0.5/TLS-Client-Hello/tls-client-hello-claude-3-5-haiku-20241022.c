#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t handshake_type;
    uint32_t length;
    uint16_t legacy_version;
    uint8_t random[32];
    HParser* legacy_session_id;
    HParser* cipher_suites;
    uint8_t compression_method;
    HParser* extensions;
} TLSClientHello;

HParser* parse_random() {
    return h_repeat_n(h_uint8(), 32);
}

HParser* parse_legacy_session_id() {
    return h_length_value(h_uint8(), h_repeat_n(h_uint8(), 32));
}

HParser* parse_cipher_suites() {
    return h_length_value(h_uint16(), h_repeat_n(h_uint16(), 65535));
}

HParser* parse_extensions() {
    return h_length_value(h_uint16(), 
        h_repeat_n(
            h_sequence(
                h_uint16(),  // extension type
                h_length_value(h_uint16(), h_repeat_n(h_uint8(), 65535)) // extension data
            ),
            65535
        )
    );
}

HParser* parse_tls_client_hello() {
    return h_sequence(
        h_uint8(),           // handshake type
        h_bits(24, false),   // length (24-bit unsigned)
        h_uint16(),          // legacy version
        parse_random(),      // random
        parse_legacy_session_id(), // legacy session id
        parse_cipher_suites(), // cipher suites
        h_uint8(),           // compression method
        parse_extensions()   // extensions
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
        printf("Successfully parsed TLS Client Hello\n");
    } else {
        printf("Parsing failed\n");
    }

    h_parse_result_free(result);
    free(buffer);

    return 0;
}