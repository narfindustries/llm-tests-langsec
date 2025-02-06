#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define parsers for basic types
static HParser *uint8;
static HParser *uint16;
static HParser *uint32;

// Define parsers for complex types
static HParser *protocol_version;
static HParser *random_bytes;
static HParser *session_id;
static HParser *cipher_suite;
static HParser *cipher_suites;
static HParser *compression_methods;
static HParser *extension_type;
static HParser *extension_data;
static HParser *extension;
static HParser *extensions;
static HParser *client_hello;

void init_parsers() {
    uint8 = h_uint8();
    uint16 = h_uint16();
    uint32 = h_uint32();

    protocol_version = uint16;
    random_bytes = h_bits(32*8, false);
    session_id = h_length_value(uint8, h_bits(32*8, false));
    cipher_suite = uint16;
    cipher_suites = h_length_value(uint16, h_many1(cipher_suite));
    compression_methods = h_length_value(uint8, h_many1(uint8));
    extension_type = uint16;
    extension_data = h_length_value(uint16, h_bytes(0));
    extension = h_sequence(extension_type, extension_data, NULL);
    extensions = h_length_value(uint16, h_many1(extension));
    client_hello = h_sequence(protocol_version, random_bytes, session_id, cipher_suites, compression_methods, extensions, NULL);
}

void print_hex(const uint8_t *data, size_t length) {
    for (size_t i = 0; i < length; i++) {
        printf("%02X ", data[i]);
    }
    printf("\n");
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file path>\n", argv[0]);
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

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        fprintf(stderr, "Failed to allocate memory\n");
        fclose(file);
        return 1;
    }

    if (fread(buffer, 1, file_size, file) != file_size) {
        fprintf(stderr, "Failed to read file\n");
        free(buffer);
        fclose(file);
        return 1;
    }

    init_parsers();

    HParseResult *result = h_parse(client_hello, buffer, file_size);
    if (result) {
        printf("ClientHello parsed successfully\n");
    } else {
        fprintf(stderr, "Failed to parse ClientHello\n");
    }

    free(buffer);
    fclose(file);
    return 0;
}