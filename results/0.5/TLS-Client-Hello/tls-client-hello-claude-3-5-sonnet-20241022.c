#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t tls_versions[] = {0x03, 0x03}; // TLS 1.2

HParser* init_tls_client_hello_parser(void) {
    // Basic integer parsers
    HParser* uint8 = h_uint8();
    HParser* uint16 = h_uint16();
    
    // Content Type (0x16 for Handshake)
    HParser* content_type = h_ch(0x16);
    
    // Protocol Version (0x0303 for TLS 1.2)
    HParser* protocol_version = h_sequence(h_ch(0x03), h_ch(0x03), NULL);
    
    // Record Length (16 bits)
    HParser* record_length = uint16;
    
    // Handshake Type (1 for ClientHello)
    HParser* handshake_type = h_ch(0x01);
    
    // Handshake Length (24 bits, 3 bytes)
    HParser* handshake_length = h_sequence(uint8, uint16, NULL);
    
    // Client Version
    HParser* client_version = h_sequence(h_ch(0x03), h_ch(0x03), NULL);
    
    // Random (32 bytes)
    HParser* random = h_repeat_n(uint8, 32);
    
    // Session ID
    HParser* session_id_length = uint8;
    HParser* session_id = h_length_value(session_id_length, uint8);
    
    // Cipher Suites
    HParser* cipher_suites_length = uint16;
    HParser* cipher_suites = h_length_value(cipher_suites_length, uint8);
    
    // Compression Methods
    HParser* compression_methods_length = uint8;
    HParser* compression_methods = h_length_value(compression_methods_length, uint8);
    
    // Extensions Length
    HParser* extensions_length = uint16;
    
    // Extension
    HParser* extension_type = uint16;
    HParser* extension_length = uint16;
    HParser* extension_data = h_length_value(extension_length, uint8);
    HParser* extension = h_sequence(extension_type, extension_length, extension_data, NULL);
    
    // Extensions
    HParser* extensions = h_length_value(extensions_length, extension);
    
    // Combine all parts into ClientHello
    HParser* client_hello = h_sequence(
        content_type,
        protocol_version,
        record_length,
        handshake_type,
        handshake_length,
        client_version,
        random,
        session_id,
        cipher_suites,
        compression_methods,
        extensions,
        NULL
    );
    
    return client_hello;
}

int main(int argc, char** argv) {
    HParser* parser = init_tls_client_hello_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    return 0;
}