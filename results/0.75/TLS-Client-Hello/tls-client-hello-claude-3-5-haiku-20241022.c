#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <hammer/hammer.h>
#include <hammer/parsers.h>

typedef struct {
    uint8_t handshake_type;
    uint32_t length;
    uint16_t legacy_version;
    uint8_t random[32];
    HParsedToken* legacy_session_id;
    HParsedToken* cipher_suites;
    HParsedToken* compression_methods;
    HParsedToken* extensions;
} TLSClientHello;

static HParser* parse_handshake_type() {
    return h_uint8();
}

static HParser* parse_length() {
    return h_bits(24, false);
}

static HParser* parse_legacy_version() {
    return h_uint16();
}

static HParser* parse_random() {
    return h_repeat_n(h_uint8(), 32);
}

static HParser* parse_legacy_session_id() {
    return h_length_value(h_uint8(), h_uint8());
}

static HParser* parse_cipher_suites() {
    return h_length_value(h_uint16(), h_uint16());
}

static HParser* parse_compression_methods() {
    return h_length_value(h_uint8(), h_uint8());
}

static HParser* parse_extension_type() {
    return h_uint16();
}

static HParser* parse_extension_length() {
    return h_uint16();
}

static HParser* parse_extensions() {
    HParser* extension = h_sequence(
        parse_extension_type(),
        parse_extension_length(),
        h_repeat_n(h_uint8(), 2)
    );
    return h_many(extension);
}

static HParser* parse_client_hello() {
    return h_sequence(
        parse_handshake_type(),
        parse_length(),
        parse_legacy_version(),
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

    HParser* parser = parse_client_hello();
    HParseResult* result = h_parse(parser, buffer, read_size);

    if (result && result->ast) {
        printf("TLS ClientHello parsed successfully\n");
    } else {
        printf("Parsing failed\n");
    }

    h_parse_result_free(result);
    h_arena_free(parser->arena);
    free(buffer);

    return 0;
}