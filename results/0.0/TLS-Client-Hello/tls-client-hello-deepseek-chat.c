#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct {
    uint8_t major;
    uint8_t minor;
} ProtocolVersion;

typedef struct {
    uint32_t gmt_unix_time;
    uint8_t random_bytes[28];
} Random;

typedef struct {
    uint8_t length;
    uint8_t *session_id;
} SessionID;

typedef struct {
    uint16_t length;
    uint8_t *cipher_suites;
} CipherSuites;

typedef struct {
    uint8_t length;
    uint8_t *compression_methods;
} CompressionMethods;

typedef struct {
    ProtocolVersion protocol_version;
    Random random;
    SessionID session_id;
    CipherSuites cipher_suites;
    CompressionMethods compression_methods;
} TLSClientHello;

HParser *protocol_version_parser() {
    return h_sequence(h_uint8(), h_uint8(), NULL);
}

HParser *random_parser() {
    return h_sequence(h_uint32(), h_bytes(28), NULL);
}

HParser *session_id_parser() {
    return h_sequence(h_uint8(), h_length_value(h_uint8(), h_uint8()), NULL);
}

HParser *cipher_suites_parser() {
    return h_sequence(h_uint16(), h_length_value(h_uint16(), h_uint8()), NULL);
}

HParser *compression_methods_parser() {
    return h_sequence(h_uint8(), h_length_value(h_uint8(), h_uint8()), NULL);
}

HParser *tls_client_hello_parser() {
    return h_sequence(
        protocol_version_parser(),
        random_parser(),
        session_id_parser(),
        cipher_suites_parser(),
        compression_methods_parser(),
        NULL
    );
}

int main() {
    HParser *parser = tls_client_hello_parser();
    uint8_t input[] = { /* Your TLS Client Hello data here */ };
    size_t input_len = sizeof(input);

    HParseResult *result = h_parse(parser, input, input_len);
    if (result) {
        TLSClientHello *client_hello = (TLSClientHello *)result->ast;
        printf("Parsed TLS Client Hello successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse TLS Client Hello.\n");
    }

    return 0;
}