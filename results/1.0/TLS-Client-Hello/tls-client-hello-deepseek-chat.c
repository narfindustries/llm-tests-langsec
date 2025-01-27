#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint8_t major;
    uint8_t minor;
} TLSVersion;

typedef struct {
    uint32_t gmt_unix_time;
    uint8_t random_bytes[28];
} Random;

typedef struct {
    uint8_t session_id_length;
    uint8_t *session_id;
} SessionID;

typedef struct {
    uint16_t cipher_suites_length;
    uint16_t *cipher_suites;
} CipherSuites;

typedef struct {
    uint8_t compression_methods_length;
    uint8_t *compression_methods;
} CompressionMethods;

typedef struct {
    uint16_t extensions_length;
    uint8_t *extensions;
} Extensions;

typedef struct {
    TLSVersion version;
    Random random;
    SessionID session_id;
    CipherSuites cipher_suites;
    CompressionMethods compression_methods;
    Extensions extensions;
} TLSClientHello;

HParser *tls_version_parser() {
    return h_sequence(
        h_uint8(), // major
        h_uint8(), // minor
        NULL
    );
}

HParser *random_parser() {
    return h_sequence(
        h_uint32(), // gmt_unix_time
        h_bytes(28), // random_bytes
        NULL
    );
}

HParser *session_id_parser() {
    return h_sequence(
        h_uint8(), // session_id_length
        h_length_value(h_uint8(), h_uint8()), // session_id
        NULL
    );
}

HParser *cipher_suites_parser() {
    return h_sequence(
        h_uint16(), // cipher_suites_length
        h_length_value(h_uint16(), h_uint16()), // cipher_suites
        NULL
    );
}

HParser *compression_methods_parser() {
    return h_sequence(
        h_uint8(), // compression_methods_length
        h_length_value(h_uint8(), h_uint8()), // compression_methods
        NULL
    );
}

HParser *extensions_parser() {
    return h_sequence(
        h_uint16(), // extensions_length
        h_length_value(h_uint16(), h_uint8()), // extensions
        NULL
    );
}

HParser *tls_client_hello_parser() {
    return h_sequence(
        tls_version_parser(),
        random_parser(),
        session_id_parser(),
        cipher_suites_parser(),
        compression_methods_parser(),
        extensions_parser(),
        NULL
    );
}

int main() {
    HParser *parser = tls_client_hello_parser();
    const uint8_t input[] = { /* Your TLS Client Hello data here */ };
    size_t input_size = sizeof(input);

    HParseResult *result = h_parse(parser, input, input_size);
    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    return 0;
}