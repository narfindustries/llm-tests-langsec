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
    uint8_t random[32];
} TlsRandom;

typedef struct {
    uint8_t data[32];
} TlsSessionId;

typedef struct {
    uint16_t code;
    uint16_t length;
    uint8_t data[];
} TlsExtension;

typedef struct {
    uint16_t client_version;
    uint8_t random[32];
    uint8_t session_id_len;
    uint8_t session_id[32];
    uint16_t cipher_suites_len;
    uint16_t cipher_suites[0];
    uint8_t compression_methods_len;
    uint8_t compression_methods[0];
    uint16_t extensions_len;
    TlsExtension extensions[0];
} TlsClientHello;

HammerParser tls_version = h_map2(h_uint16, h_uint16, (HParser*)&TlsVersion);
HammerParser tls_random = h_map(h_bytes(32), (HParser*)&TlsRandom);
HammerParser tls_session_id_len = h_uint8;
HammerParser tls_session_id = h_count(tls_session_id_len, h_uint8);
HammerParser tls_cipher_suites_len = h_uint16;
HammerParser tls_cipher_suites = h_count(tls_cipher_suites_len, h_uint16);
HammerParser tls_compression_methods_len = h_uint8;
HammerParser tls_compression_methods = h_count(tls_compression_methods_len, h_uint8);

HammerParser tls_extension = h_sequence(h_uint16, h_uint16, h_many(h_uint8),
                                      \(uint16 type, uint16 length, Vector<uint8> data) {
                                          TlsExtension* ext = (TlsExtension*)malloc(sizeof(TlsExtension) + length);
                                          ext->code = type;
                                          ext->length = length;
                                          memcpy(ext->data, data.data, length);
                                          return ext;
                                      });

HammerParser tls_extensions_len = h_uint16;
HammerParser tls_extensions = h_count(tls_extensions_len, tls_extension);

HammerParser tls_client_hello = h_sequence(h_uint16, tls_random, tls_session_id_len, tls_session_id,
                                        tls_cipher_suites_len, tls_cipher_suites, tls_compression_methods_len,
                                        tls_compression_methods, tls_extensions_len, tls_extensions,
                                        \(uint16 version, TlsRandom random, uint8 session_id_len, Vector<uint8> session_id,
                                            uint16 cipher_suites_len, Vector<uint16> cipher_suites, uint8 compression_methods_len,
                                            Vector<uint8> compression_methods, uint16 extensions_len, Vector<TlsExtension*> extensions) {
                                            size_t hello_size = sizeof(TlsClientHello) + (cipher_suites_len * sizeof(uint16)) + compression_methods_len + (extensions_len * sizeof(TlsExtension));
                                            TlsClientHello* hello = (TlsClientHello*)malloc(hello_size);
                                            hello->client_version = version;
                                            memcpy(hello->random, random.random, 32);
                                            hello->session_id_len = session_id_len;
                                            memcpy(hello->session_id, session_id.data, session_id_len);
                                            hello->cipher_suites_len = cipher_suites_len;
                                            memcpy(hello->cipher_suites, cipher_suites.data, cipher_suites_len * sizeof(uint16));
                                            hello->compression_methods_len = compression_methods_len;
                                            memcpy(hello->compression_methods, compression_methods.data, compression_methods_len);
                                            hello->extensions_len = extensions_len;
                                            for (size_t i = 0; i < extensions.size; i++) {
                                                memcpy(&hello->extensions[i], extensions.data[i], sizeof(TlsExtension) + extensions.data[i]->length);
                                            }
                                            return hello;
                                        });

int main(int argc, char* argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fileSize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    uint8_t* buffer = (uint8_t*)malloc(fileSize);
    fread(buffer, 1, fileSize, fp);
    fclose(fp);

    Result<TlsClientHello*> result = h_parse(tls_client_hello, buffer, fileSize);

    if (result.is_ok()) {
        TlsClientHello* hello = result.value;
        printf("Client Hello parsed successfully!\n");
        free(hello);
    } else {
        fprintf(stderr, "Error parsing Client Hello: %s\n", result.error.message);
    }

    free(buffer);
    return 0;
}
