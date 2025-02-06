#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stddef.h>

typedef struct {
    uint16_t client_version;
    uint8_t random[64];
} ClientHelloHeader;

typedef struct {
    uint8_t legacy_session_id_length;
    uint8_t legacy_session_id[32];
} LegacySessionID;

typedef struct {
    uint16_t cipher_suite_length;
    uint16_t cipher_suites[100];
} CipherSuites;

typedef struct {
    uint8_t compression_method_length;
    uint8_t compression_methods[1];
} CompressionMethods;

typedef struct {
    uint16_t extension_type;
    uint16_t extension_length;
    uint8_t extension_data[1024];
} Extension;

typedef struct {
    uint16_t extensions_length;
    Extension extensions[100];
} Extensions;

typedef struct {
    ClientHelloHeader header;
    LegacySessionID legacy_session_id;
    CipherSuites cipher_suites;
    CompressionMethods compression_methods;
    Extensions extensions;
} ClientHello;

static HParser client_hello_header_parser() {
    return h_seq(
        h_map(h_uint16, offsetof(ClientHelloHeader, client_version)),
        h_map(h_bytes(64), offsetof(ClientHelloHeader, random)),
        h_ret(ClientHelloHeader)
    );
}

static HParser legacy_session_id_parser() {
    return h_seq(
        h_map(h_uint8, offsetof(LegacySessionID, legacy_session_id_length)),
        h_map(h_bytes_n(offsetof(LegacySessionID, legacy_session_id), sizeof(((LegacySessionID*)0)->legacy_session_id)), offsetof(LegacySessionID, legacy_session_id)),
        h_ret(LegacySessionID)
    );
}

static HParser cipher_suites_parser() {
    return h_seq(
        h_map(h_uint16, offsetof(CipherSuites, cipher_suite_length)),
        h_map(h_array_n(offsetof(CipherSuites, cipher_suites), sizeof(((CipherSuites*)0)->cipher_suites)/sizeof(((CipherSuites*)0)->cipher_suites[0]), h_uint16), offsetof(CipherSuites, cipher_suites)),
        h_ret(CipherSuites)
    );
}

static HParser compression_methods_parser() {
    return h_seq(
        h_map(h_uint8, offsetof(CompressionMethods, compression_method_length)),
        h_map(h_bytes_n(offsetof(CompressionMethods, compression_methods), sizeof(((CompressionMethods*)0)->compression_methods)), offsetof(CompressionMethods, compression_methods)),
        h_ret(CompressionMethods)
    );
}

static HParser extension_parser() {
    return h_seq(
        h_map(h_uint16, offsetof(Extension, extension_type)),
        h_map(h_uint16, offsetof(Extension, extension_length)),
        h_map(h_bytes_n(offsetof(Extension, extension_data), sizeof(((Extension*)0)->extension_data)), offsetof(Extension, extension_data)),
        h_ret(Extension)
    );
}

static HParser extensions_parser() {
    return h_seq(
        h_map(h_uint16, offsetof(Extensions, extensions_length)),
        h_map(h_array_n(offsetof(Extensions, extensions), sizeof(((Extensions*)0)->extensions)/sizeof(((Extensions*)0)->extensions[0]), extension_parser), offsetof(Extensions, extensions)),
        h_ret(Extensions)
    );
}

static HParser client_hello_parser() {
    return h_seq(
        h_map(client_hello_header_parser, offsetof(ClientHello, header)),
        h_map(legacy_session_id_parser, offsetof(ClientHello, legacy_session_id)),
        h_map(cipher_suites_parser, offsetof(ClientHello, cipher_suites)),
        h_map(compression_methods_parser, offsetof(ClientHello, compression_methods)),
        h_map(extensions_parser, offsetof(ClientHello, extensions)),
        h_ret(ClientHello)
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
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

    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    HParseResult result = h_parse(&client_hello_parser(), buffer, fileSize);

    if (result.status == H_SUCCESS) {
        ClientHello *clientHello = (ClientHello *)result.value;
        printf("ClientHello parsed successfully!\n");
        free(clientHello);
    } else {
        fprintf(stderr, "Parsing failed: %s\n", result.error);
    }

    free(buffer);
    return 0;
}
