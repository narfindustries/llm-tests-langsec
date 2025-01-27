#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t TLS_VERSION_MAJOR = 0x03;
static const uint8_t TLS_VERSION_MINOR = 0x03;

HParser* init_tls_client_hello_parser(void) {
    // Basic integer parsers
    HParser* uint8 = h_uint8();
    HParser* uint16 = h_uint16();
    HParser* uint24 = h_bits(24, false);
    HParser* uint32 = h_uint32();

    // Record Layer
    HParser* content_type = h_uint8();
    HParser* version = h_sequence(uint8, uint8, NULL);
    HParser* length = h_uint16();

    // Handshake Layer
    HParser* handshake_type = h_uint8();
    HParser* handshake_length = h_uint24();

    // Client Hello Fields
    HParser* client_version = h_sequence(uint8, uint8, NULL);
    HParser* random = h_repeat_n(uint8, 32);
    
    // Session ID
    HParser* session_id_length = h_uint8();
    HParser* session_id = h_length_value(session_id_length, uint8);

    // Cipher Suites
    HParser* cipher_suites_length = h_uint16();
    HParser* cipher_suites = h_length_value(cipher_suites_length, h_repeat_n(uint8, 2));

    // Compression Methods
    HParser* compression_methods_length = h_uint8();
    HParser* compression_methods = h_length_value(compression_methods_length, uint8);

    // Extensions
    HParser* extensions_length = h_uint16();
    HParser* extension_type = h_uint16();
    HParser* extension_length = h_uint16();
    HParser* extension_data = h_length_value(extension_length, uint8);
    HParser* extension = h_sequence(extension_type, extension_data, NULL);
    HParser* extensions = h_length_value(extensions_length, h_many1(extension));

    // Client Hello Message
    HParser* client_hello = h_sequence(
        client_version,
        random,
        session_id,
        cipher_suites,
        compression_methods,
        extensions,
        NULL
    );

    // Handshake Layer
    HParser* handshake = h_sequence(
        handshake_type,
        handshake_length,
        client_hello,
        NULL
    );

    // Record Layer
    return h_sequence(
        content_type,
        version,
        length,
        handshake,
        NULL
    );
}