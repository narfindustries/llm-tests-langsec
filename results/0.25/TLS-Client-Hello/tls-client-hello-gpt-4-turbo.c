#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for basic types
HParser *uint8;
HParser *uint16;
HParser *uint32;

// Function to create a parser for a byte sequence with a preceding length byte
HParser *bytes_with_length_u8() {
    return h_length_value(uint8, h_many(uint8));
}

// Function to create a parser for a byte sequence with a preceding two-byte length
HParser *bytes_with_length_u16() {
    return h_length_value(uint16, h_many(uint8));
}

// Parser for the Random field (32 bytes)
HParser *random_bytes;

// Parser for the Session ID (0-32 bytes)
HParser *session_id;

// Parser for the Cipher Suites list
HParser *cipher_suites;

// Parser for the Compression Methods (1 byte, but can be more in other contexts)
HParser *compression_methods;

// Parser for a single extension
HParser *extension() {
    return h_sequence(uint16, bytes_with_length_u16(), NULL);
}

// Parser for the Extensions field
HParser *extensions;

// Parser for the ClientHello message
HParser *client_hello;

void init_parsers() {
    uint8 = h_uint8();
    uint16 = h_bits(16, false);
    uint32 = h_bits(32, false);
    random_bytes = h_repeat_n(uint8, 32);
    session_id = bytes_with_length_u8();
    cipher_suites = bytes_with_length_u16();
    compression_methods = bytes_with_length_u8();
    extensions = bytes_with_length_u16();
    client_hello = h_sequence(
        uint16,          // legacy_version (TLS 1.2 fixed value)
        random_bytes,    // random
        session_id,      // session_id
        cipher_suites,   // cipher_suites
        compression_methods, // compression_methods
        extensions,      // extensions
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file containing ClientHello>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    size_t size = ftell(fp);
    rewind(fp);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    if (fread(data, 1, size, fp) != size) {
        perror("Failed to read file");
        free(data);
        fclose(fp);
        return 1;
    }
    fclose(fp);

    init_parsers();

    HParseResult *result = h_parse(client_hello, data, size);
    if (result) {
        printf("ClientHello parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
    } else {
        printf("Failed to parse ClientHello.\n");
    }

    h_parse_result_free(result);
    free(data);

    return 0;
}