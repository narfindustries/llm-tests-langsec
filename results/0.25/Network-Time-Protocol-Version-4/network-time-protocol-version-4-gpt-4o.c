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
    uint64_t transmit_timestamp;
    uint32_t key_id;
    uint8_t message_digest[16];
} ntp_packet_t;

HParser *ntp_parser() {
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
    HParser *transmit_timestamp = h_bits(64, false);
    HParser *key_id = h_bits(32, false);
    HParser *message_digest = h_repeat_n(h_bits(8, false), 16);

    HParser *optional_fields = h_sequence(key_id, message_digest, NULL);

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
        transmit_timestamp,
        h_optional(optional_fields),
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
    printf("Transmit Timestamp: %llu\n", packet->transmit_timestamp);
    printf("Key ID: %u\n", packet->key_id);
    printf("Message Digest: ");
    for (int i = 0; i < 16; i++) {
        printf("%02x", packet->message_digest[i]);
    }
    printf("\n");
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

    HParser *parser = ntp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);

    if (result && result->ast) {
        ntp_packet_t packet;
        memcpy(&packet, result->ast->bytes, sizeof(ntp_packet_t));
        print_ntp_packet(&packet);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse NTP packet\n");
    }

    free(buffer);
    return EXIT_SUCCESS;
}