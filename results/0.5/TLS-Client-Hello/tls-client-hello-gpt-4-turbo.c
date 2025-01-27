#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define basic field parsers
static HParser *uint8 = h_uint8();
static HParser *uint16 = h_uint16();
static HParser *uint24 = h_bits(24, false);
static HParser *uint32 = h_uint32();

// Define a parser for TLS ProtocolVersion
static HParser *protocol_version = h_sequence(uint8, uint8, NULL);

// Define a parser for Random structure in TLS ClientHello
static HParser *random = h_sequence(uint32, h_bytes(28), NULL);

// Define a parser for SessionID
static HParser *session_id = h_length_value(uint8, h_uint8());

// Define a parser for Cipher Suites
static HParser *cipher_suites = h_length_value(uint16, h_uint16());

// Define a parser for Compression Methods
static HParser *compression_methods = h_length_value(uint8, h_uint8());

// Define a parser for Extension
static HParser *extension = h_sequence(uint16, h_length_value(uint16, h_any()));

// Define a parser for Extensions
static HParser *extensions = h_length_value(uint16, extension);

// Define a parser for TLS ClientHello
static HParser *client_hello = h_sequence(
    protocol_version,  // Version
    random,            // Random
    session_id,        // Session ID
    cipher_suites,     // Cipher Suites
    compression_methods, // Compression Methods
    extensions,        // Extensions
    NULL
);

// Main function to parse input
int main(int argc, char *argv[]) {
    size_t len;
    uint8_t *input = read_file(stdin, &len);
    if (!input) {
        fprintf(stderr, "Failed to read input\n");
        return 1;
    }

    HParseResult *result = h_parse(client_hello, input, len);
    if (result == NULL) {
        fprintf(stderr, "Parse failed\n");
        free(input);
        return 1;
    }

    json_t *json = h_value_to_json(result->ast);
    char *json_str = json_dumps(json, JSON_INDENT(2));
    printf("%s\n", json_str);

    // Cleanup
    free(json_str);
    json_decref(json);
    h_parse_result_free(result);
    free(input);

    return 0;
}