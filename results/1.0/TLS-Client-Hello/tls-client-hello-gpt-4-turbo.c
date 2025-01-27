#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdint.h>

// Forward declarations
static const HParser *tls_extension;
static const HParser *session_id;
static const HParser *cipher_suites;
static const HParser *compression_methods;

// Parser for a TLS Extension
static const HParser *tls_extension = h_sequence(
    h_int16(), // Extension Type
    h_len_value(h_uint16(), h_bytes(h_uint16())), // Extension Data
    NULL
);

// Parser for Session ID
static const HParser *session_id = h_len_value(h_uint8(), h_bytes(h_uint8()));

// Parser for Cipher Suites
static const HParser *cipher_suites = h_len_value(h_uint16(), h_sequence(h_uint16(), NULL, NULL));

// Parser for Compression Methods
static const HParser *compression_methods = h_len_value(h_uint8(), h_sequence(h_uint8(), NULL, NULL));

// TLS Client Hello message structure
static const HParser *tls_client_hello = h_sequence(
    h_ignore(h_uint16()),  // Protocol Version (major.minor)
    h_bytes(32),           // Random (32 bytes fixed length)
    session_id,            // Session ID
    cipher_suites,         // Cipher Suites
    compression_methods,   // Compression Methods
    h_optional(h_len_value(h_uint16(), h_many(tls_extension))), // Extensions
    NULL
);

int main(int argc, char *argv[]) {
    // Setting up the parser environment
    HParser *parser = tls_client_hello;
    size_t input_size;
    uint8_t *input = read_file(stdin, &input_size);
    HParseResult *result = h_parse(parser, input, input_size);

    if (result) {
        // Successfully parsed
        printf("TLS Client Hello parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        // Failed to parse
        fprintf(stderr, "Failed to parse TLS Client Hello.\n");
    }

    free(input);
    return 0;
}