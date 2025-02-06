#include <stdio.h>
#include <stdlib.h>
#include <hammer/hammer.h>

// Define structures for TLS ClientHello message fields

// Helper parsers
HParser *parse_uint16() { return h_uint16(); }
HParser *parse_uint8() { return h_uint8(); }

HParser *parse_uint8_fixed(uint8_t value) {
    return h_int_range(h_uint8(), value, value);
}

HParser *parse_bytes(size_t length) {
    return h_repeat_n(h_uint8(), length);
}

HParser *parse_variable_bytes(size_t max_length) {
    return h_length_value(h_uint8(), h_repeat_n(h_uint8(), max_length));
}

HParser *parse_variable_list(HParser *elem_parser, size_t max_length) {
    return h_length_value(h_uint16(), h_repeat_n(elem_parser, max_length));
}

// TLS ClientHello specific parsers

HParser *parse_legacy_version() {
    return parse_uint16();
}

HParser *parse_random() {
    return parse_bytes(32);
}

HParser *parse_legacy_session_id() {
    return parse_variable_bytes(32);
}

HParser *parse_cipher_suites() {
    return parse_variable_list(parse_uint16(), 0xFFFF);
}

HParser *parse_legacy_compression_methods() {
    return h_length_value(parse_uint8(), h_choice(parse_uint8_fixed(0x00), NULL));
}

HParser *parse_supported_versions() {
    return h_sequence(parse_uint16(), h_length_value(parse_uint8(), parse_uint16()), NULL);
}

HParser *parse_extensions() {
    HParser *extension_parser = h_choice(
        h_sequence(parse_uint16(), parse_supported_versions(), NULL),  // Extension ID and supported versions data
        h_sequence(parse_uint16(), parse_variable_bytes(255), NULL)    // Generic extension parser
    );

    return h_length_value(parse_uint16(), h_repeat(extension_parser, 0, 0xFFFF));
}

// TLS ClientHello parser
HParser *parse_tls_client_hello() {
    return h_sequence(
        parse_legacy_version(),
        parse_random(),
        parse_legacy_session_id(),
        parse_cipher_suites(),
        parse_legacy_compression_methods(),
        parse_extensions(),
        NULL
    );
}

// Main function
int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <tls_client_hello.bin>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("fopen");
        return EXIT_FAILURE;
    }

    // Determine file size
    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Read file into buffer
    uint8_t *buffer = (uint8_t *)malloc(file_size);
    if (!buffer) {
        perror("malloc");
        fclose(file);
        return EXIT_FAILURE;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        perror("fread");
        free(buffer);
        fclose(file);
        return EXIT_FAILURE;
    }
    fclose(file);

    // Parse ClientHello
    HParser *tls_client_hello_parser = parse_tls_client_hello();
    HParseResult *result = h_parse(tls_client_hello_parser, buffer, file_size);

    if (result) {
        printf("Successfully parsed TLS ClientHello.\n");
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse TLS ClientHello.\n");
    }

    h_parser_free(tls_client_hello_parser);
    free(buffer);
    return EXIT_SUCCESS;
}