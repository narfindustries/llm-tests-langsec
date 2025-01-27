#include <hammer/hammer.h>

static HParser *create_tls_client_hello_parser(void) {
    // Define parsers for TLS Client Hello message structure
    HParser *uint8 = h_uint8();
    HParser *uint16 = h_uint16();
    HParser *uint24 = h_repeat_n(uint8, 3);
    HParser *uint32 = h_uint32();

    // Define SSLVersion parser (2 bytes)
    HParser *ssl_version = uint16;

    // Define Random parser (32 bytes)
    HParser *random = h_repeat_n(uint8, 32);

    // Define Session ID parser
    HParser *session_id_length = uint8;
    HParser *session_id = h_repeat(uint8, session_id_length);

    // Define Cipher Suites parser
    HParser *cipher_suite_length = uint16;
    HParser *cipher_suites = h_repeat(uint16, h_length_value(cipher_suite_length, uint16));

    // Define Compression Methods parser
    HParser *compression_methods_length = uint8;
    HParser *compression_methods = h_repeat(uint8, compression_methods_length);

    // Define Extensions parser
    HParser *extensions_length = uint16;
    HParser *extensions = h_repeat(uint8, h_length_value(extensions_length, uint8));

    // Construct the full TLS Client Hello parser
    HParser *tls_client_hello = h_sequence(
        ssl_version,
        random,
        session_id,
        cipher_suites,
        compression_methods,
        extensions,
        NULL
    );

    return tls_client_hello;
}

int main(int argc, char *argv[]) {
    HParser *parser = create_tls_client_hello_parser();

    // Example usage: parsing data
    // Note: This is a placeholder. Replace with actual TLS Client Hello data.
    const uint8_t data[] = {
        0x03, 0x03, // TLS Version 1.2
        // Random (32 bytes)
        0x53, 0x2e, 0x86, 0x91, 0xa1, 0x56, 0x34, 0x5c, 0x2f, 0x23, 0x47, 0x10, 0x72, 0x7f, 0x37, 0xfc,
        0x3e, 0x3a, 0x99, 0x84, 0xdc, 0x5e, 0x4f, 0x3a, 0x5e, 0x1b, 0x80, 0x8b, 0x93, 0x48, 0x4c, 0x9c,
        0x00, // Session ID Length
        0x00, 0x02, // Cipher Suites Length
        0x13, 0x01, // Cipher Suite TLS_AES_128_GCM_SHA256
        0x01, // Compression Methods Length
        0x00, // Compression Method: null
        0x00, 0x00, // Extensions Length
        // Extensions (if any)
    };

    HParseResult *result = h_parse(parser, data, sizeof(data));
    if (result) {
        printf("Parsing successful.\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed.\n");
    }

    // Clean up
    h_parser_free(parser);

    return 0;
}