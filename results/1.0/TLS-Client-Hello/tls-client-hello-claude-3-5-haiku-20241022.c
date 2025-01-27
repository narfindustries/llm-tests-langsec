#include <hammer/hammer.h>
#include <hammer/parsers.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

// TLS ClientHello Structure Specification
static HParser* tls_client_hello_parser() {
    // Record Type: Handshake (22)
    HParsedToken* record_type = h_ch(22);

    // TLS Version (3.3 = TLS 1.2)
    HParser* tls_version = h_sequence(
        h_ch(3),    // Major version 
        h_ch(3),    // Minor version
        NULL
    );

    // Random data (32 bytes)
    HParser* random_data = h_repeat_n(h_uint8(), 32);

    // Session ID (variable length)
    HParser* session_id = h_length_value(h_uint8(), h_uint8());

    // Cipher Suites (variable length)
    HParser* cipher_suites = h_length_value(
        h_uint16be(),  // Length field
        h_repeat(h_uint16be())  // Cipher suite values
    );

    // Compression Methods
    HParser* compression_methods = h_length_value(
        h_uint8(),     // Length field
        h_repeat(h_uint8())  // Compression methods
    );

    // Extensions (optional)
    HParser* extensions = h_optional(
        h_length_value(
            h_uint16be(),  // Total extensions length
            h_repeat(
                h_sequence(
                    h_uint16be(),  // Extension type
                    h_length_value(h_uint16be(), h_uint8()),  // Extension data
                    NULL
                )
            )
        )
    );

    // Complete ClientHello structure
    HParser* client_hello = h_sequence(
        record_type,      // Record type
        tls_version,      // TLS version
        random_data,      // Random bytes
        session_id,       // Session ID
        cipher_suites,    // Supported cipher suites
        compression_methods, // Compression methods
        extensions,       // Optional extensions
        NULL
    );

    return client_hello;
}

int main() {
    HParserBackend backend = PB_PACKRAT;
    h_init(&backend);

    HParser* parser = tls_client_hello_parser();
    return 0;
}