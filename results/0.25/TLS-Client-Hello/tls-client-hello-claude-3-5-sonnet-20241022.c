#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t TLS_VERSIONS[] = {0x03, 0x03}; // TLS 1.2

HParser* init_tls_client_hello_parser() {
    // Basic integer parsers
    HParser* uint8 = h_uint8();
    HParser* uint16 = h_uint16();
    HParser* uint24 = h_bits(24, false);
    
    // Fixed values
    HParser* handshake_type = h_ch(0x01); // ClientHello
    HParser* tls_version = h_sequence(h_ch(0x03), h_ch(0x03), NULL);
    
    // Random (32 bytes)
    HParser* random = h_repeat_n(h_uint8(), 32);
    
    // Session ID
    HParser* session_id_length = h_uint8();
    HParser* session_id = h_length_value(session_id_length, h_uint8());
    
    // Cipher Suites
    HParser* cipher_suites_length = h_uint16();
    HParser* cipher_suites = h_length_value(cipher_suites_length, h_uint16());
    
    // Compression Methods
    HParser* compression_methods_length = h_uint8();
    HParser* compression_methods = h_length_value(compression_methods_length, h_uint8());
    
    // Extensions
    HParser* extensions_length = h_uint16();
    HParser* extension_type = h_uint16();
    HParser* extension_length = h_uint16();
    HParser* extension_data = h_length_value(extension_length, h_uint8());
    HParser* extension = h_sequence(extension_type, extension_data, NULL);
    HParser* extensions = h_length_value(extensions_length, h_many1(extension));
    
    // Client Hello Message
    HParser* client_hello = h_sequence(
        handshake_type,
        h_length_value(uint24,
            h_sequence(
                tls_version,
                random,
                session_id,
                cipher_suites,
                compression_methods,
                extensions,
                NULL
            )
        ),
        NULL
    );
    
    return client_hello;
}

int main() {
    HParser* parser = init_tls_client_hello_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    return 0;
}