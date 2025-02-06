#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for individual fields
HParser *legacy_version_parser() {
    return h_uint16();
}

HParser *random_parser() {
    return h_length_value(h_uint32(), h_repeat_n(h_uint8(), 28));
}

HParser *legacy_session_id_parser() {
    return h_length_value(h_uint8(), h_repeat_n(h_uint8(), 32));
}

HParser *cipher_suites_parser() {
    return h_length_value(h_uint16(), h_repeat_n(h_uint16(), 0));
}

HParser *legacy_compression_methods_parser() {
    return h_length_value(h_uint8(), h_repeat_n(h_uint8(), 1));
}

HParser *extension_parser() {
    return h_sequence(h_uint16(), h_length_value(h_uint16(), h_repeat_n(h_uint8(), 0)), NULL);
}

HParser *extensions_parser() {
    return h_length_value(h_uint16(), h_repeat_n(extension_parser(), 0));
}

// Define the main Client Hello parser
HParser *client_hello_parser() {
    return h_sequence(
        legacy_version_parser(),
        random_parser(),
        legacy_session_id_parser(),
        cipher_suites_parser(),
        legacy_compression_methods_parser(),
        extensions_parser(),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(file_size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, file_size, file);
    fclose(file);

    HParseResult *result = h_parse(client_hello_parser(), data, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse Client Hello\n");
        free(data);
        return 1;
    }

    printf("Successfully parsed Client Hello\n");
    h_parse_result_free(result);
    free(data);
    return 0;
}