#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

static HParser* tls_protocol_version;
static HParser* random_bytes;
static HParser* cipher_suites;
static HParser* compression_methods;
static HParser* extensions;

static HParser* create_tls_client_hello_parser() {
    tls_protocol_version = h_choice(
        h_literal("\x03\x01"),  // TLS 1.0
        h_literal("\x03\x02"),  // TLS 1.1
        h_literal("\x03\x03"),  // TLS 1.2
        h_literal("\x03\x04"),  // TLS 1.3
        NULL
    );

    random_bytes = h_repeat_n(h_ch_range('\x00', '\xFF'), 32);

    cipher_suites = h_repeat_n(
        h_choice(
            h_literal("\x00\x2F"),   // TLS_RSA_WITH_AES_128_CBC_SHA
            h_literal("\xC0\x2B"),   // TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256
            h_literal("\xC0\x30"),   // TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384
            NULL
        ), 
        1, 10
    );

    compression_methods = h_repeat_n(
        h_choice(
            h_ch('\x00'),  // No compression
            h_ch('\x01')   // Deflate compression
        ),
        1, 5
    );

    extensions = h_repeat_n(
        h_choice(
            h_literal("\x00\x00"),   // Server Name Indication
            h_literal("\x00\x0A"),   // Elliptic Curves
            h_literal("\x00\x0B")    // EC Point Formats
        ),
        0, 10
    );

    return h_sequence(
        h_ch('\x16'),           // Handshake record type
        h_ch('\x03\x01'),       // Protocol version
        h_length_value(         // Length of handshake message
            h_sequence(
                h_ch('\x01'),   // ClientHello type
                h_length_value( // ClientHello length
                    h_sequence(
                        tls_protocol_version,
                        random_bytes,
                        h_length_value(h_choice(h_end_p(), h_ch('\x00'), NULL)),  // Session ID
                        h_length_value(cipher_suites),
                        h_length_value(compression_methods),
                        h_length_value(extensions)
                    )
                )
            )
        ),
        NULL
    );
}

int main(int argc, char** argv) {
    HParser* parser = create_tls_client_hello_parser();
    return 0;
}