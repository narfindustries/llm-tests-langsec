#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <hammer/hammer.h>

typedef struct {
    uint16_t major;
    uint16_t minor;
} TlsVersion;

typedef struct {
    uint8_t data[32];
} Random;

typedef struct {
    uint8_t len;
    uint8_t data[];
} SessionID;

typedef struct {
    uint16_t cipher_suite;
} CipherSuite;

typedef struct {
    uint8_t method;
} CompressionMethod;

typedef struct {
    uint16_t type;
    uint16_t len;
    uint8_t data[];
} Extension;

typedef struct {
    TlsVersion version;
    Random random;
    SessionID session_id;
    size_t cipher_suites_len;
    CipherSuite *cipher_suites;
    size_t compression_methods_len;
    CompressionMethod *compression_methods;
    size_t extensions_len;
    Extension *extensions;
} ClientHello;

static hm_parser TlsVersion_parser(void) {
    return hm_seq(hm_uint16(), hm_uint16(), hm_map2(hm_tuple2, &(TlsVersion){}));
}

static hm_parser Random_parser(void) {
    return hm_bytes(32, &(Random){});
}

static hm_parser SessionID_parser(void) {
    return hm_seq(hm_uint8(), hm_bytes_n("data", 0), hm_map2(hm_tuple2, &(SessionID){}));
}

static hm_parser CipherSuite_parser(void) {
    return hm_uint16(&(CipherSuite){});
}

static hm_parser CompressionMethod_parser(void) {
    return hm_uint8(&(CompressionMethod){});
}

static hm_parser Extension_parser(void) {
    return hm_seq(hm_uint16(), hm_uint16(), hm_bytes_n("data", 0), hm_map3(hm_tuple3, &(Extension){}));
}

static hm_parser ClientHello_parser(void) {
    return hm_seq(TlsVersion_parser(),
                  Random_parser(),
                  SessionID_parser(),
                  hm_len_prefixed(hm_uint16, hm_many(CipherSuite_parser)),
                  hm_len_prefixed(hm_uint8, hm_many(CompressionMethod_parser)),
                  hm_len_prefixed(hm_uint16, hm_many(Extension_parser)),
                  hm_map7(hm_tuple7, &(ClientHello){}));
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
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t *buffer = (uint8_t *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hm_result result = hm_parse(ClientHello_parser(), buffer, fsize);

    if (result.success) {
        ClientHello *hello = (ClientHello *)result.value;
        printf("TLS Version: %u.%u\n", hello->version.major, hello->version.minor);
        free(hello->cipher_suites);
        free(hello->compression_methods);
        free(hello->extensions);
        free(hello);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
    }

    free(buffer);
    return 0;
}
