#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

// Define TLS Client Hello message structure
typedef struct {
    uint8_t record_type;
    uint16_t protocol_version;
    uint16_t length;
    uint8_t handshake_type;
    uint16_t handshake_length;
    uint16_t client_version;
    uint8_t random[32];
    uint8_t session_id_length;
    uint8_t* session_id;
    uint16_t cipher_suites_length;
    uint16_t* cipher_suites;
    uint8_t compression_methods_length;
    uint8_t* compression_methods;
} TLSClientHello;

// Hammer parser for TLS Client Hello
static HParser* tls_client_hello_parser() {
    // Record type (22 = Handshake)
    HParsedToken* record_type = h_token_uint8(22);

    // Protocol version (TLS 1.2)
    HParsedToken* protocol_version = h_token_uint16(0x0303);

    // Length (variable)
    HParser* length = h_uint16();

    // Handshake type (1 = ClientHello)
    HParsedToken* handshake_type = h_token_uint8(1);

    // Handshake length
    HParser* handshake_length = h_uint16();

    // Client version (TLS 1.2)
    HParsedToken* client_version = h_token_uint16(0x0303);

    // Random bytes
    HParser* random = h_repeat_n(h_uint8(), 32);

    // Session ID
    HParser* session_id_length = h_uint8();
    HParser* session_id = h_repeat_n(h_uint8(), session_id_length);

    // Cipher suites
    HParser* cipher_suites_length = h_uint16();
    HParser* cipher_suites = h_repeat_n(h_uint16(), cipher_suites_length);

    // Compression methods
    HParser* compression_methods_length = h_uint8();
    HParser* compression_methods = h_repeat_n(h_uint8(), compression_methods_length);

    // Combine parsers
    return h_sequence(
        record_type,
        protocol_version,
        length,
        handshake_type,
        handshake_length,
        client_version,
        random,
        session_id_length,
        session_id,
        cipher_suites_length,
        cipher_suites,
        compression_methods_length,
        compression_methods,
        NULL
    );
}

int main() {
    // Initialize Hammer
    h_init();

    // Create parser
    HParser* parser = tls_client_hello_parser();

    // Example TLS Client Hello message (simplified)
    uint8_t tls_client_hello[] = {
        0x16,           // Record type (Handshake)
        0x03, 0x03,     // Protocol version (TLS 1.2)
        0x00, 0x30,     // Length
        0x01,           // Handshake type (ClientHello)
        0x00, 0x2C,     // Handshake length
        0x03, 0x03,     // Client version (TLS 1.2)
        // Random bytes (32 bytes)
        0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
        0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10,
        0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
        0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
        0x00,           // Session ID length
        0x00, 0x02,     // Cipher suites length
        0x13, 0x02,     // TLS_AES_256_GCM_SHA384
        0x01,           // Compression methods length
        0x00            // No compression
    };

    // Parse the message
    HParseResult* result = h_parse(parser, tls_client_hello, sizeof(tls_client_hello));

    if (result && result->ast) {
        printf("TLS Client Hello parsed successfully\n");
    } else {
        printf("Parsing failed\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_destroy(parser);

    return 0;
}