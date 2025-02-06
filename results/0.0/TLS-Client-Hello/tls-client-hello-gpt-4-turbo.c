#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for various components of the TLS Client Hello message
HParser *parse_version() {
    return h_uint16();
}

HParser *parse_random() {
    return h_bits(32*8, false);
}

HParser *parse_session_id() {
    return h_length_value(h_uint8(), h_bits(32*8, false));
}

HParser *parse_cipher_suites() {
    return h_length_value(h_uint16(), h_uint16());
}

HParser *parse_compression_methods() {
    return h_length_value(h_uint8(), h_uint8());
}

HParser *parse_extension() {
    return h_sequence(h_uint16(), h_length_value(h_uint16(), h_bits(0, false)), NULL);
}

HParser *parse_extensions() {
    return h_length_value(h_uint16(), h_many(parse_extension()));
}

HParser *parse_client_hello() {
    return h_sequence(
        parse_version(),
        parse_random(),
        parse_session_id(),
        parse_cipher_suites(),
        parse_compression_methods(),
        parse_extensions(),
        NULL
    );
}

void print_hex(const uint8_t *s, size_t len) {
    for (size_t i = 0; i < len; ++i) {
        printf("%02x", s[i]);
    }
    printf("\n");
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file path>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (!fp) {
        perror("Failed to open file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *data = malloc(fsize);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(fp);
        return 1;
    }

    fread(data, 1, fsize, fp);
    fclose(fp);

    HParser *client_hello_parser = parse_client_hello();
    HParseResult *result = h_parse(client_hello_parser, data, fsize);
    if (result) {
        printf("Client Hello parsed successfully.\n");
        // Additional processing can be done here
    } else {
        fprintf(stderr, "Failed to parse Client Hello.\n");
    }

    h_free_parser(client_hello_parser);
    free(data);
    return 0;
}