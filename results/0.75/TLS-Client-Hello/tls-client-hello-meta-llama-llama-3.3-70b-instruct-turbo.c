#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#define MAX_EXTENSION_LENGTH 65535
#define MAX_EXTENSIONS 255

typedef struct {
    uint8_t major;
    uint8_t minor;
} ProtocolVersion;

typedef struct {
    uint8_t data[32];
} Random;

typedef struct {
    size_t length;
    uint8_t* data;
} Opaque;

typedef struct {
    ProtocolVersion version;
} ExtensionTypeSupportedVersions;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypeKeyShare;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypePreSharedKey;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypeEarlyData;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypeCookie;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypePskKeyExchangeModes;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypeTicketEarlyDataInfo;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypeTls12SignatureAlgorithms;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypeTls13SignatureAlgorithms;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypeCertificateAuthorities;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypeOidFilters;

typedef struct {
    uint16_t length;
    uint8_t* data;
} ExtensionTypePostHandshakeAuth;

typedef struct {
    uint16_t type;
    uint16_t length;
    union {
        ExtensionTypeSupportedVersions supported_versions;
        ExtensionTypeKeyShare key_share;
        ExtensionTypePreSharedKey pre_shared_key;
        ExtensionTypeEarlyData early_data;
        ExtensionTypeCookie cookie;
        ExtensionTypePskKeyExchangeModes psk_key_exchange_modes;
        ExtensionTypeTicketEarlyDataInfo ticket_early_data_info;
        ExtensionTypeTls12SignatureAlgorithms tls12_signature_algorithms;
        ExtensionTypeTls13SignatureAlgorithms tls13_signature_algorithms;
        ExtensionTypeCertificateAuthorities certificate_authorities;
        ExtensionTypeOidFilters oid_filters;
        ExtensionTypePostHandshakeAuth post_handshake_auth;
        Opaque opaque;
    } data;
} Extension;

typedef struct {
    ProtocolVersion legacy_version;
    Random random;
    Opaque legacy_session_id;
    Opaque legacy_compressed_certificate;
    ProtocolVersion* supported_versions;
    size_t supported_versions_length;
    Extension* extensions;
    size_t extensions_length;
} ClientHello;

int main(int argc, char** argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE* input_file = fopen(argv[1], "rb");
    if (!input_file) {
        printf("Failed to open input file\n");
        return 1;
    }

    fseek(input_file, 0, SEEK_END);
    long file_size = ftell(input_file);
    rewind(input_file);

    uint8_t* input_data = malloc(file_size);
    if (!input_data) {
        printf("Failed to allocate memory for input data\n");
        return 1;
    }

    size_t read_size = fread(input_data, 1, file_size, input_file);
    if (read_size != file_size) {
        printf("Failed to read input file\n");
        return 1;
    }

    fclose(input_file);

    HParser* uint8_parser = h_uint8();
    HParser* uint16_parser = h_uint16();

    HParser* bytes_32_parser = h_bytes(32);
    HParser* opaque_parser = h_bytes_till_zero();

    HParser* extension_type_supported_versions_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_key_share_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_pre_shared_key_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_early_data_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_cookie_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_psk_key_exchange_modes_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_ticket_early_data_info_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_tls12_signature_algorithms_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_tls13_signature_algorithms_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_certificate_authorities_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_oid_filters_parser = h_sequence(uint16_parser, opaque_parser);
    HParser* extension_type_post_handshake_auth_parser = h_sequence(uint16_parser, opaque_parser);

    HParser* extension_parser = h_sequence(uint16_parser, uint16_parser, opaque_parser);

    HParser* protocol_version_parser = h_sequence(uint8_parser, uint8_parser);
    HParser* client_hello_parser = h_sequence(
        protocol_version_parser,
        bytes_32_parser,
        opaque_parser,
        opaque_parser,
        h_list(protocol_version_parser),
        h_list(extension_parser)
    );

    HParseResult* result = h_parse(client_hello_parser, input_data, file_size);
    if (!result) {
        printf("Failed to parse input data\n");
        return 1;
    }

    ClientHello* client_hello = (ClientHello*)result;

    printf("Legacy Version: %d.%d\n", client_hello->legacy_version.major, client_hello->legacy_version.minor);
    printf("Random: ");
    for (int i = 0; i < 32; i++) {
        printf("%02x", client_hello->random.data[i]);
    }
    printf("\n");
    printf("Legacy Session ID: ");
    for (int i = 0; i < client_hello->legacy_session_id.length; i++) {
        printf("%02x", client_hello->legacy_session_id.data[i]);
    }
    printf("\n");
    printf("Legacy Compressed Certificate: ");
    for (int i = 0; i < client_hello->legacy_compressed_certificate.length; i++) {
        printf("%02x", client_hello->legacy_compressed_certificate.data[i]);
    }
    printf("\n");
    printf("Supported Versions: ");
    for (int i = 0; i < client_hello->supported_versions_length; i++) {
        printf("%d.%d ", client_hello->supported_versions[i].major, client_hello->supported_versions[i].minor);
    }
    printf("\n");
    printf("Extensions: \n");
    for (int i = 0; i < client_hello->extensions_length; i++) {
        printf("Type: %d, Length: %d, Data: ", client_hello->extensions[i].type, client_hello->extensions[i].length);
        for (int j = 0; j < client_hello->extensions[i].length; j++) {
            printf("%02x", client_hello->extensions[i].data.opaque.data[j]);
        }
        printf("\n");
    }

    free(input_data);

    return 0;
}