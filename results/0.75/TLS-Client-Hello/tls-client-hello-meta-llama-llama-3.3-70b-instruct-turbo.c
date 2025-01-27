#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

// Define TLS Client Hello structure
typedef struct {
    uint8_t record_content_type;
    uint16_t version;
    uint16_t length;
    uint8_t handshake_type;
    uint24_t length_handshake;
    uint16_t version_handshake;
    uint8_t random_bytes[32];
    uint8_t session_id_length;
    uint8_t session_id[32];
    uint16_t cipher_suites_length;
    uint16_t cipher_suites[100];
    uint8_t compression_methods_length;
    uint8_t compression_methods[10];
    uint16_t extensions_length;
    uint16_t extension_type;
    uint16_t extension_length;
} TLSClientHello;

// Define a function to parse TLS Client Hello
int parse_TLSClientHello(const uint8_t* data, size_t size) {
    TLSClientHello* client_hello = (TLSClientHello*)data;

    // Check if the record content type is 0x14 (handshake)
    if (client_hello->record_content_type != 0x14) {
        printf("Invalid record content type\n");
        return -1;
    }

    // Check if the handshake type is 0x01 (client hello)
    if (client_hello->handshake_type != 0x01) {
        printf("Invalid handshake type\n");
        return -1;
    }

    // Check if the version is 0x0303 (TLS 1.2)
    if (client_hello->version_handshake != 0x0303) {
        printf("Invalid version\n");
        return -1;
    }

    // Print the session ID
    printf("Session ID: ");
    for (int i = 0; i < client_hello->session_id_length; i++) {
        printf("%02x", client_hello->session_id[i]);
    }
    printf("\n");

    // Print the cipher suites
    printf("Cipher suites: ");
    for (int i = 0; i < client_hello->cipher_suites_length / 2; i++) {
        printf("%04x ", client_hello->cipher_suites[i]);
    }
    printf("\n");

    return 0;
}

int main() {
    // Example TLS Client Hello data
    uint8_t data[] = {
        0x14, 0x03, 0x03, 0x00, 0x01, 0x00, 0x00, 0x3b,
        0x03, 0x03, 0x52, 0x6c, 0x6c, 0x6c, 0x6c, 0x6c,
        0x6c, 0x6c, 0x6c, 0x6c, 0x6c, 0x6c, 0x6c, 0x6c,
        0x6c, 0x6c, 0x6c, 0x6c, 0x6c, 0x6c, 0x6c, 0x6c,
        0x00, 0x00, 0x00, 0x05, 0x00, 0x05, 0x00, 0x00,
        0x05, 0x00, 0x05, 0x01, 0x00, 0x00, 0x06, 0x00,
        0x04, 0x00, 0x00
    };

    parse_TLSClientHello(data, sizeof(data));

    return 0;
}