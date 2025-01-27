#include <hammer/hammer.h>

HParser *create_tls_client_hello_parser() {
    // Define basic parsers
    HParser *uint8 = h_uint8();
    HParser *uint16 = h_uint16();
    HParser *uint24 = h_repeat_n(uint8, 3);
    HParser *uint32 = h_uint32();

    // Define parsers for specific TLS structures
    HParser *protocol_version = h_sequence(uint8, uint8, NULL);
    HParser *random = h_repeat_n(uint8, 32);
    HParser *session_id_length = uint8;
    HParser *session_id = h_length_value(session_id_length, uint8);
    HParser *cipher_suites_length = uint16;
    HParser *cipher_suites = h_length_value(cipher_suites_length, uint16);
    HParser *compression_methods_length = uint8;
    HParser *compression_methods = h_length_value(compression_methods_length, uint8);

    // Extensions
    HParser *extension_type = uint16;
    HParser *extension_data_length = uint16;
    HParser *extension_data = h_length_value(extension_data_length, uint8);
    HParser *extension = h_sequence(extension_type, extension_data, NULL);
    HParser *extensions_length = uint16;
    HParser *extensions = h_length_value(extensions_length, h_many(extension));

    // ClientHello message
    HParser *client_hello = h_sequence(
        protocol_version,
        random,
        session_id,
        cipher_suites,
        compression_methods,
        extensions,
        NULL
    );

    return client_hello;
}

int main() {
    HParser *parser = create_tls_client_hello_parser();
    // Use the parser with some input data
    // Example: HParseResult *result = h_parse(parser, input_data, input_length);

    // Clean up
    h_parser_free(parser);
    return 0;
}