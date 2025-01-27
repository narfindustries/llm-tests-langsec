#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t VERSION_MAJOR = 0x03;
static const uint8_t VERSION_MINOR = 0x03;

HParser* init_tls_client_hello_parser(void) {
    // Basic integer parsers
    HParser* uint8 = h_uint8();
    HParser* uint16 = h_uint16();
    
    // Content Type (0x16 for Handshake)
    HParser* content_type = h_int_range(uint8, 0x16, 0x16);
    
    // Protocol Version (0x0301 - 0x0303)
    HParser* protocol_version = h_sequence(
        h_int_range(uint8, 0x03, 0x03),
        h_int_range(uint8, 0x01, 0x03),
        NULL
    );
    
    // Length (uint16)
    HParser* record_length = uint16;
    
    // Handshake Type (1 for ClientHello)
    HParser* handshake_type = h_int_range(uint8, 0x01, 0x01);
    
    // Handshake Length (uint24 - represented as 3 bytes)
    HParser* handshake_length = h_sequence(
        uint8,
        uint8,
        uint8,
        NULL
    );
    
    // Client Version
    HParser* client_version = h_sequence(
        h_ch(VERSION_MAJOR),
        h_ch(VERSION_MINOR),
        NULL
    );
    
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
    
    // Extensions
    HParser* extension_type = uint16;
    HParser* extension_length = uint16;
    HParser* extension_data = h_length_value(extension_length, uint8);
    
    HParser* extension = h_sequence(
        extension_type,
        extension_length,
        extension_data,
        NULL
    );
    
    HParser* extensions = h_length_value(extensions_length, extension);
    
    // Complete ClientHello
    return h_sequence(
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
}