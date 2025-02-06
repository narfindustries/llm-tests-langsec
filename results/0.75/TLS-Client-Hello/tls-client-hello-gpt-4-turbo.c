#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parser for legacy version
static HParser *parse_legacy_version() {
    return h_uint16();
}

// Define parser for random data (32 bytes)
static HParser *parse_random_bytes() {
    return h_bits(32*8, false);
}

// Define parser for legacy session ID
static HParser *parse_legacy_session_id() {
    return h_length_value(h_uint8(), h_bytes(h_uint8()));
}

// Define parser for cipher suites
static HParser *parse_cipher_suites() {
    return h_length_value(h_uint16(), h_many1(h_uint16()));
}

// Define parser for legacy compression method
static HParser *parse_legacy_compression_methods() {
    return h_length_value(h_uint8(), h_many1(h_uint8()));
}

// Define parser for extensions
static HParser *parse_extension() {
    HParser *type = h_uint16();
    HParser *data = h_length_value(h_uint16(), h_any());
    return h_sequence(type, data, NULL);
}

static HParser *parse_extensions() {
    return h_length_value(h_uint16(), h_many1(parse_extension()));
}

// Define parser for the TLS Client Hello message
static HParser *tls_client_hello() {
    return h_sequence(
        parse_legacy_version(),
        parse_random_bytes(),
        parse_legacy_session_id(),
        parse_cipher_suites(),
        parse_legacy_compression_methods(),
        parse_extensions(),
        NULL
    );
}

// Function to parse the ClientHello from a file
void parse_client_hello(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        return;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);
    uint8_t *data = malloc(length);
    if (data) {
        fread(data, 1, length, file);

        HParser *parser = tls_client_hello();
        HParseResult *result = h_parse(parser, data, length);
        if (result) {
            printf("ClientHello parsed successfully.\n");
            h_pprint(stdout, result->ast, 0, 1);
        } else {
            printf("Failed to parse ClientHello.\n");
        }

        free(data);
    } else {
        printf("Failed to allocate memory for file data.\n");
    }

    fclose(file);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <TLS_ClientHello_file>\n", argv[0]);
        return 1;
    }

    parse_client_hello(argv[1]);
    return 0;
}