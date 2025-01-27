#include <hammer/hammer.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define HAMMERssid_t hammer_int8_t
#define HAMMER_TlsVersion hammer_int16_t

typedef struct {
    HAMMERssid_t record_content_type;
    HAMMER_TlsVersion version;
    uint16_t length;
    HAMMERssid_t handshake_type;
    uint24_t length_handshake;
    HAMMER_TlsVersion version_handshake;
    uint32_t random_gmt_unix_time;
    uint8_t random_bytes[28];
    HAMMERssid_t session_id_length;
    uint8_t session_id[];
    HAMMERssid_t cipher_suites_length;
    HAMMERssid_t compression_methods_length;
} TlsClientHello;

int main() {
    hammer_t *hammer = hammer_init("TLS-Client-Hello");
    if (!hammer) {
        return 1;
    }

    hammer_type_t *record_content_type = hammer_int8_t(hammer, "record_content_type");
    hammer_type_t *version = hammer_int16_t(hammer, "version");
    hammer_type_t *length = hammer_int16_t(hammer, "length");
    hammer_type_t *handshake_type = hammer_int8_t(hammer, "handshake_type");
    hammer_type_t *length_handshake = hammer_int24_t(hammer, "length_handshake");
    hammer_type_t *version_handshake = hammer_int16_t(hammer, "version_handshake");
    hammer_type_t *random_gmt_unix_time = hammer_int32_t(hammer, "random_gmt_unix_time");
    hammer_type_t *random_bytes = hammer_bytes_t(hammer, "random_bytes", 28);
    hammer_type_t *session_id_length = hammer_int8_t(hammer, "session_id_length");
    hammer_type_t *session_id = hammer_bytes_t(hammer, "session_id", -1);
    hammer_type_t *cipher_suites_length = hammer_int16_t(hammer, "cipher_suites_length");
    hammer_type_t *compression_methods_length = hammer_int8_t(hammer, "compression_methods_length");

    TlsClientHello *msg = (TlsClientHello *)malloc(sizeof(TlsClientHello));
    if (!msg) {
        return 1;
    }

    msg->record_content_type = 22;
    msg->version = 771;
    msg->length = 51;
    msg->handshake_type = 1;
    msg->length_handshake = 46;
    msg->version_handshake = 771;
    msg->random_gmt_unix_time = 1643723900;
    memset(msg->random_bytes, 0x12, 28);
    msg->session_id_length = 0;
    msg->session_id = NULL;
    msg->cipher_suites_length = 2;
    msg->compression_methods_length = 1;

    hammer_free(hammer);
    free(msg);
    return 0;
}