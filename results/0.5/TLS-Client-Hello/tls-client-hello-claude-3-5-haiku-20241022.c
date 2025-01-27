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
static HParsedToken* parse_tls_client_hello(void* p) {
    HParser* record_type = h_uint8();
    HParser* protocol_version = h_uint16();
    HParser* length = h_uint16();
    HParser* handshake_type = h_uint8();
    HParser* handshake_length = h_uint16();
    HParser* client_version = h_uint16();
    HParser* random = h_repeat_n(h_uint8(), 32);
    HParser* session_id_length = h_uint8();
    HParser* session_id = h_repeat_n(h_uint8(), h_get_token_value(session_id_length));
    HParser* cipher_suites_length = h_uint16();
    HParser* cipher_suites = h_repeat_n(h_uint16(), h_get_token_value(cipher_suites_length) / 2);
    HParser* compression_methods_length = h_uint8();
    HParser* compression_methods = h_repeat_n(h_uint8(), h_get_token_value(compression_methods_length));

    HParser* parser = h_sequence(
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

    return h_parse(parser, NULL, 0);
}

int main() {
    // Initialize Hammer
    h_init();

    // Example TLS Client Hello message
    uint8_t tls_client_hello[] = {
        0x16,       // Record Type: Handshake
        0x03, 0x03, // Protocol Version: TLS 1.2
        0x00, 0x4E, // Length
        0x01,       // Handshake Type: Client Hello
        0x00, 0x4A, // Handshake Length
        0x03, 0x03, // Client Version: TLS 1.2
        // Random (32 bytes)
        0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08,
        0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10,
        0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
        0x19, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
        0x00,       // Session ID Length
        0x00, 0x02, // Cipher Suites Length
        0x13, 0x01, // TLS_AES_128_GCM_SHA256
        0x01,       // Compression Methods Length
        0x00        // No compression
    };

    // Parse TLS Client Hello
    HParsedToken* result = parse_tls_client_hello(tls_client_hello);

    if (result) {
        printf("TLS Client Hello parsed successfully\n");
        // Additional processing or validation can be added here
    } else {
        printf("Failed to parse TLS Client Hello\n");
    }

    return 0;
}