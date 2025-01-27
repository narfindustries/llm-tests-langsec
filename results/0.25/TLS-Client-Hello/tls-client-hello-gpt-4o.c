#include <hammer/hammer.h>

HParser *create_tls_client_hello_parser() {
    // Define basic parsers for primitive types
    HParser *u8 = h_uint8();
    HParser *u16 = h_uint16();
    HParser *u24 = h_uint24();
    HParser *u32 = h_uint32();

    // Define parsers for specific TLS structures
    HParser *protocol_version = h_sequence(u8, u8, NULL);
    HParser *random = h_repeat_n(u8, 32);

    // Define parsers for session ID
    HParser *session_id_length = u8;
    HParser *session_id = h_length_value(session_id_length, u8);

    // Define parsers for cipher suites
    HParser *cipher_suites_length = u16;
    HParser *cipher_suites = h_length_value(cipher_suites_length, u16);

    // Define parsers for compression methods
    HParser *compression_methods_length = u8;
    HParser *compression_methods = h_length_value(compression_methods_length, u8);

    // Define parsers for extensions
    HParser *extension_type = u16;
    HParser *extension_length = u16;
    HParser *extension_data = h_length_value(extension_length, u8);
    HParser *extension = h_sequence(extension_type, extension_data, NULL);

    HParser *extensions_length = u16;
    HParser *extensions = h_length_value(extensions_length, extension);

    // Define the complete ClientHello parser
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

int main(int argc, char **argv) {
    HParser *parser = create_tls_client_hello_parser();
    // Use the parser with input data here
    // Remember to free the parser when done
    h_parser_free(parser);
    return 0;
}