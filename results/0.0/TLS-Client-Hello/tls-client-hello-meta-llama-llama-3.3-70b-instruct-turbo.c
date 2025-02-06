#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define TLS_VERSION_MAJOR 3
#define TLS_VERSION_MINOR 3

typedef struct {
    uint8_t major;
    uint8_t minor;
} protocol_version_t;

typedef struct {
    uint32_t gmt_unix_time;
    uint8_t random_bytes[28];
} random_t;

typedef struct {
    protocol_version_t legacy_version;
    random_t random;
    uint8_t legacy_session_id[32];
    uint8_t legacy_compression_methods[255];
    protocol_version_t supported_versions[255];
    uint16_t num_supported_versions;
    uint8_t extensions[65535];
    uint16_t extension_length;
} client_hello_t;

typedef struct {
    uint16_t type;
    uint16_t length;
    uint8_t data[65535];
} extension_t;

typedef struct {
    uint16_t group;
    uint8_t key_exchange[255];
} supported_group_t;

typedef struct {
    uint16_t group;
    uint8_t key_exchange[255];
} key_share_t;

typedef struct {
    uint16_t identity;
    uint8_t obfuscated_ticket_age[32];
} pre_shared_key_t;

typedef struct {
    uint8_t cookie[255];
} cookie_t;

typedef struct {
    protocol_version_t version;
} supported_version_t;

typedef struct {
    uint8_t mode;
} psk_key_exchange_mode_t;

typedef struct {
    uint16_t max_early_data_size;
} ticket_early_data_info_t;

typedef struct {
    uint16_t identity;
} pre_shared_key_identity_t;

typedef struct {
    uint8_t binding[32];
} post_handshake_auth_t;

#define HAMMER_OK 0
#define HAMMER_ERROR -1

typedef struct {
    uint8_t *data;
    size_t length;
    size_t pos;
} hammer_t;

void hammer_init(hammer_t *h, uint8_t *data, size_t length) {
    h->data = data;
    h->length = length;
    h->pos = 0;
}

int hammer_uint8(hammer_t *h, uint8_t *value) {
    if (h->pos >= h->length) {
        return HAMMER_ERROR;
    }
    *value = h->data[h->pos++];
    return HAMMER_OK;
}

int hammer_uint16(hammer_t *h, uint16_t *value) {
    if (h->pos + 2 > h->length) {
        return HAMMER_ERROR;
    }
    *value = (h->data[h->pos++] << 8) | h->data[h->pos++];
    return HAMMER_OK;
}

int hammer_uint32(hammer_t *h, uint32_t *value) {
    if (h->pos + 4 > h->length) {
        return HAMMER_ERROR;
    }
    *value = (h->data[h->pos++] << 24) | (h->data[h->pos++] << 16) | (h->data[h->pos++] << 8) | h->data[h->pos++];
    return HAMMER_OK;
}

int hammer_bytes(hammer_t *h, uint8_t *data, size_t length) {
    if (h->pos + length > h->length) {
        return HAMMER_ERROR;
    }
    memcpy(data, h->data + h->pos, length);
    h->pos += length;
    return HAMMER_OK;
}

int hammer_peek_uint16(hammer_t *h) {
    if (h->pos + 2 > h->length) {
        return HAMMER_ERROR;
    }
    return (h->data[h->pos] << 8) | h->data[h->pos + 1];
}

int parse_protocol_version(hammer_t *h, protocol_version_t *version) {
    if (hammer_uint8(h, &version->major) != HAMMER_OK ||
        hammer_uint8(h, &version->minor) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_random(hammer_t *h, random_t *random) {
    if (hammer_uint32(h, &random->gmt_unix_time) != HAMMER_OK ||
        hammer_bytes(h, random->random_bytes, 28) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_extension(hammer_t *h, extension_t *extension) {
    if (hammer_uint16(h, &extension->type) != HAMMER_OK ||
        hammer_uint16(h, &extension->length) != HAMMER_OK ||
        hammer_bytes(h, extension->data, extension->length) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_supported_group(hammer_t *h, supported_group_t *group) {
    if (hammer_uint16(h, &group->group) != HAMMER_OK ||
        hammer_bytes(h, group->key_exchange, 255) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_key_share(hammer_t *h, key_share_t *share) {
    if (hammer_uint16(h, &share->group) != HAMMER_OK ||
        hammer_bytes(h, share->key_exchange, 255) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_pre_shared_key(hammer_t *h, pre_shared_key_t *key) {
    if (hammer_uint16(h, &key->identity) != HAMMER_OK ||
        hammer_bytes(h, key->obfuscated_ticket_age, 32) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_cookie(hammer_t *h, cookie_t *cookie) {
    if (hammer_bytes(h, cookie->cookie, 255) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_supported_version(hammer_t *h, supported_version_t *version) {
    if (parse_protocol_version(h, &version->version) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_psk_key_exchange_mode(hammer_t *h, psk_key_exchange_mode_t *mode) {
    if (hammer_uint8(h, &mode->mode) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_ticket_early_data_info(hammer_t *h, ticket_early_data_info_t *info) {
    if (hammer_uint16(h, &info->max_early_data_size) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_pre_shared_key_identity(hammer_t *h, pre_shared_key_identity_t *identity) {
    if (hammer_uint16(h, &identity->identity) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_post_handshake_auth(hammer_t *h, post_handshake_auth_t *auth) {
    if (hammer_bytes(h, auth->binding, 32) != HAMMER_OK) {
        return HAMMER_ERROR;
    }
    return HAMMER_OK;
}

int parse_client_hello(hammer_t *h, client_hello_t *hello) {
    if (parse_protocol_version(h, &hello->legacy_version) != HAMMER_OK ||
        parse_random(h, &hello->random) != HAMMER_OK ||
        hammer_bytes(h, hello->legacy_session_id, 32) != HAMMER_OK ||
        hammer_bytes(h, hello->legacy_compression_methods, 255) != HAMMER_OK) {
        return HAMMER_ERROR;
    }

    hello->num_supported_versions = 0;
    while (hammer_peek_uint16(h) == 772) {
        supported_version_t version;
        if (parse_supported_version(h, &version) != HAMMER_OK) {
            return HAMMER_ERROR;
        }
        hello->supported_versions[hello->num_supported_versions++] = version.version;
    }

    hello->extension_length = 0;
    while (hammer_peek_uint16(h) != 0) {
        extension_t extension;
        if (parse_extension(h, &extension) != HAMMER_OK) {
            return HAMMER_ERROR;
        }
        memcpy(hello->extensions + hello->extension_length, &extension, sizeof(extension_t));
        hello->extension_length += sizeof(extension_t);
    }

    return HAMMER_OK;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        printf("Usage: %s <input_file>\n", argv[0]);
        return 1;
    }

    FILE *input_file = fopen(argv[1], "rb");
    if (!input_file) {
        printf("Error opening input file\n");
        return 1;
    }

    fseek(input_file, 0, SEEK_END);
    long file_size = ftell(input_file);
    rewind(input_file);

    uint8_t *input_data = malloc(file_size);
    if (!input_data) {
        printf("Error allocating memory\n");
        return 1;
    }

    fread(input_data, 1, file_size, input_file);
    fclose(input_file);

    hammer_t h;
    hammer_init(&h, input_data, file_size);

    client_hello_t hello;
    if (parse_client_hello(&h, &hello) != HAMMER_OK) {
        printf("Error parsing Client Hello\n");
        return 1;
    }

    printf("Legacy Version: %u.%u\n", hello.legacy_version.major, hello.legacy_version.minor);
    printf("Random: %u\n", hello.random.gmt_unix_time);
    printf("Legacy Session ID: ");
    for (int i = 0; i < 32; i++) {
        printf("%02x", hello.legacy_session_id[i]);
    }
    printf("\n");
    printf("Legacy Compression Methods: ");
    for (int i = 0; i < 255; i++) {
        printf("%02x", hello.legacy_compression_methods[i]);
    }
    printf("\n");
    printf("Supported Versions: ");
    for (int i = 0; i < hello.num_supported_versions; i++) {
        printf("%u.%u ", hello.supported_versions[i].major, hello.supported_versions[i].minor);
    }
    printf("\n");
    printf("Extensions: ");
    for (int i = 0; i < hello.extension_length; i += sizeof(extension_t)) {
        extension_t *extension = (extension_t *)(hello.extensions + i);
        printf("Type: %u, Length: %u, Data: ", extension->type, extension->length);
        for (int j = 0; j < extension->length; j++) {
            printf("%02x", extension->data[j]);
        }
        printf("\n");
    }

    free(input_data);
    return 0;
}