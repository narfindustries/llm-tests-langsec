#include <hammer/hammer.h>

HParser *parse_tls_version() {
    return h_sequence(h_uint8(), h_uint8(), NULL);
}

HParser *parse_tls_random() {
    return h_fixed_size(32, h_uint8());
}

HParser *parse_session_id() {
    return h_length(h_uint8(), h_uint8());
}

HParser *parse_cipher_suites() {
    return h_length(h_uint16(), h_uint16());
}

HParser *parse_compression_methods() {
    return h_length(h_uint8(), h_uint8());
}

HParser *parse_extensions() {
    return h_length(h_uint16(), h_uint8());
}

HParser *parse_tls_client_hello() {
    return h_sequence(
        h_literal_bs("\\x01", 1),      // Message Type: ClientHello (1)
        h_length(
            h_uint24(),
            h_sequence(
                parse_tls_version(),  // Protocol Version
                parse_tls_random(),   // Random
                parse_session_id(),   // Session ID
                parse_cipher_suites(),// Cipher Suites
                parse_compression_methods(), // Compression Methods
                parse_extensions()    // Extensions
            )
        ),
        NULL
    );
}

int main() {
    HParser *client_hello_parser = parse_tls_client_hello();
    // Further processing with the client_hello_parser as needed
    return 0;
}
