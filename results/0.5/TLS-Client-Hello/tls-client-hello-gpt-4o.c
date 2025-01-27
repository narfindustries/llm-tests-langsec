#include <hammer/hammer.h>

// Define the TLS Client Hello message structure
HParser *create_tls_client_hello_parser() {
    // Define parsers for various fields within the Client Hello message
    HParser *protocol_version = h_sequence(h_uint8(), h_uint8(), NULL);
    HParser *random = h_repeat_n(h_uint8(), 32);
    HParser *session_id_length = h_uint8();
    HParser *session_id = h_repeat(h_uint8(), session_id_length);
    HParser *cipher_suites_length = h_uint16();
    HParser *cipher_suites = h_repeat(h_uint16(), h_length_value(cipher_suites_length, 2));
    HParser *compression_methods_length = h_uint8();
    HParser *compression_methods = h_repeat(h_uint8(), compression_methods_length);

    // Define a parser for extensions
    HParser *extensions_length = h_uint16();
    HParser *extension_type = h_uint16();
    HParser *extension_data_length = h_uint16();
    HParser *extension_data = h_repeat(h_uint8(), extension_data_length);
    HParser *extension = h_sequence(extension_type, extension_data_length, extension_data, NULL);
    HParser *extensions = h_repeat(extension, h_length_value(extensions_length, 4));

    // Combine all parts to form the Client Hello parser
    HParser *client_hello = h_sequence(
        protocol_version,
        random,
        session_id_length,
        session_id,
        cipher_suites_length,
        cipher_suites,
        compression_methods_length,
        compression_methods,
        extensions_length,
        extensions,
        NULL
    );

    return client_hello;
}

int main() {
    // Create the parser
    HParser *parser = create_tls_client_hello_parser();

    // Clean up
    h_parser_free(parser);

    return 0;
}