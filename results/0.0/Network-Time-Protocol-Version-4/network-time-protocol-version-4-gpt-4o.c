#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <hammer/hammer.h>

typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t ref_id;
    uint64_t ref_timestamp;
    uint64_t orig_timestamp;
    uint64_t recv_timestamp;
    uint64_t trans_timestamp;
    uint32_t key_id;
    uint64_t message_digest;
    int has_authenticator;
} ntp_packet_t;

HParser *ntp_packet_parser() {
    HParser *li_vn_mode = h_bits(8, false);
    HParser *stratum = h_bits(8, false);
    HParser *poll = h_bits(8, false);
    HParser *precision = h_bits(8, false);
    HParser *root_delay = h_bits(32, false);
    HParser *root_dispersion = h_bits(32, false);
    HParser *ref_id = h_bits(32, false);
    HParser *ref_timestamp = h_bits(64, false);
    HParser *orig_timestamp = h_bits(64, false);
    HParser *recv_timestamp = h_bits(64, false);
    HParser *trans_timestamp = h_bits(64, false);
    HParser *key_id = h_bits(32, false);
    HParser *message_digest = h_bits(64, false);

    HParser *authenticator = h_sequence(key_id, message_digest, NULL);
    HParser *authenticator_opt = h_optional(authenticator);

    return h_sequence(
        li_vn_mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        ref_id,
        ref_timestamp,
        orig_timestamp,
        recv_timestamp,
        trans_timestamp,
        authenticator_opt,
        NULL
    );
}

void print_ntp_packet(ntp_packet_t *packet) {
    printf("LI_VN_MODE: %02x\n", packet->li_vn_mode);
    printf("Stratum: %u\n", packet->stratum);
    printf("Poll: %u\n", packet->poll);
    printf("Precision: %u\n", packet->precision);
    printf("Root Delay: %u\n", packet->root_delay);
    printf("Root Dispersion: %u\n", packet->root_dispersion);
    printf("Reference ID: %u\n", packet->ref_id);
    printf("Reference Timestamp: %llu\n", packet->ref_timestamp);
    printf("Originate Timestamp: %llu\n", packet->orig_timestamp);
    printf("Receive Timestamp: %llu\n", packet->recv_timestamp);
    printf("Transmit Timestamp: %llu\n", packet->trans_timestamp);
    if (packet->has_authenticator) {
        printf("Key ID: %u\n", packet->key_id);
        printf("Message Digest: %llu\n", packet->message_digest);
    } else {
        printf("No Authenticator\n");
    }
}

uint32_t read_uint32(const uint8_t *data) {
    return (data[0] << 24) | (data[1] << 16) | (data[2] << 8) | data[3];
}

uint64_t read_uint64(const uint8_t *data) {
    return ((uint64_t)read_uint32(data) << 32) | read_uint32(data + 4);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return EXIT_FAILURE;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return EXIT_FAILURE;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = ntp_packet_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result) {
        ntp_packet_t packet;
        const uint8_t *data = h_parse_result_bytes(result);

        packet.li_vn_mode = data[0];
        packet.stratum = data[1];
        packet.poll = data[2];
        packet.precision = data[3];
        packet.root_delay = read_uint32(data + 4);
        packet.root_dispersion = read_uint32(data + 8);
        packet.ref_id = read_uint32(data + 12);
        packet.ref_timestamp = read_uint64(data + 16);
        packet.orig_timestamp = read_uint64(data + 24);
        packet.recv_timestamp = read_uint64(data + 32);
        packet.trans_timestamp = read_uint64(data + 40);

        if (h_parse_result_length(result) > 48) {
            packet.key_id = read_uint32(data + 48);
            packet.message_digest = read_uint64(data + 52);
            packet.has_authenticator = 1;
        } else {
            packet.has_authenticator = 0;
        }

        print_ntp_packet(&packet);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse NTP packet\n");
    }

    free(buffer);
    h_parser_free(parser);
    return EXIT_SUCCESS;
}