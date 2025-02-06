#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

typedef struct {
    uint16_t major;
    uint16_t minor;
} TlsVersion;

typedef struct {
    uint8_t random_bytes[32];
} TlsRandom;

typedef struct {
    uint8_t session_id_bytes[32];
    size_t session_id_len;
} TlsSessionId;

typedef struct {
    uint16_t cipher_suite;
} TlsCipherSuite;

typedef struct {
    uint8_t compression_method;
} TlsCompressionMethod;

typedef struct {
    uint16_t extension_type;
    size_t extension_data_len;
    uint8_t *extension_data;
} TlsExtension;

typedef struct {
    size_t num_extensions;
    TlsExtension *extensions;
} TlsExtensions;

typedef struct {
    TlsVersion version;
    TlsRandom random;
    TlsSessionId legacy_session_id;
    size_t num_cipher_suites;
    uint16_t *cipher_suites;
    size_t num_compression_methods;
    uint8_t *compression_methods;
    TlsExtensions extensions;
} TlsClientHello;

static HammerParser tls_version = seq(h_uint16, h_uint16, &((TlsVersion*)0)->major, &((TlsVersion*)0)->minor);
static HammerParser tls_random = bytes(32, &((TlsRandom*)0)->random_bytes);
static HammerParser tls_session_id = count(h_uint8, &((TlsSessionId*)0)->session_id_len, &((TlsSessionId*)0)->session_id_bytes, 0, 32);
static HammerParser tls_cipher_suite = h_uint16;
static HammerParser tls_compression_method = h_uint8;
static HammerParser tls_extension = seq(h_uint16, count(h_uint8, &((TlsExtension*)0)->extension_data_len, &((TlsExtension*)0)->extension_data), &((TlsExtension*)0)->extension_type);
static HammerParser tls_extensions = count(tls_extension, &((TlsExtensions*)0)->num_extensions, &((TlsExtensions*)0)->extensions);

static HammerParser tls_client_hello = seq(
    tls_version, tls_random, tls_session_id, 
    count(tls_cipher_suite, &((TlsClientHello*)0)->num_cipher_suites, &((TlsClientHello*)0)->cipher_suites),
    count(tls_compression_method, &((TlsClientHello*)0)->num_compression_methods, &((TlsClientHello*)0)->compression_methods),
    tls_extensions, &((TlsClientHello*)0)->version, &((TlsClientHello*)0)->random, &((TlsClientHello*)0)->legacy_session_id, &((TlsClientHello*)0)->extensions
);


int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fileSize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    if (fread(buffer, 1, fileSize, fp) != fileSize) {
        perror("Error reading file");
        free(buffer);
        fclose(fp);
        return 1;
    }

    fclose(fp);

    TlsClientHello client_hello;
    HammerResult result = hammer_parse(tls_client_hello, buffer, fileSize, &client_hello);

    if (result.success) {
        printf("Client Hello parsed successfully:\n");
        printf("Version: %u.%u\n", client_hello.version.major, client_hello.version.minor);
        for (size_t i = 0; i < client_hello.extensions.num_extensions; ++i) {
            printf("Extension Type: %u\n", client_hello.extensions.extensions[i].extension_type);
        }
    } else {
        fprintf(stderr, "Error parsing Client Hello: %s at offset %zu\n", result.error_message, result.error_offset);
    }

    free(buffer);
    free(client_hello.cipher_suites);
    free(client_hello.compression_methods);
    for (size_t i = 0; i < client_hello.extensions.num_extensions; ++i) {
        free(client_hello.extensions.extensions[i].extension_data);
    }
    free(client_hello.extensions.extensions);

    return 0;
}
