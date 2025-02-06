#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

typedef struct {
    uint8_t li;
    uint8_t vn;
    uint8_t mode;
    uint8_t stratum;
    uint8_t poll;
    int8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t ref_id;
    uint64_t ref_timestamp;
    uint64_t orig_timestamp;
    uint64_t recv_timestamp;
    uint64_t transmit_timestamp;
    uint32_t key_id;
    uint8_t message_digest[16];
} NTPv4Packet;

HParser* parse_ntpv4_packet() {
    return h_sequence(
        h_bits(2, false),
        h_bits(3, false),
        h_bits(3, false),
        h_uint8(),
        h_uint8(),
        h_int8(),
        h_uint32(),
        h_uint32(),
        h_uint32(),
        h_uint64(),
        h_uint64(),
        h_uint64(),
        h_uint64(),
        h_optional(h_uint32()),
        h_optional(h_repeat_n(h_uint8(), 16)),
        NULL
    );
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *buffer = malloc(file_size);
    if (!buffer) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(buffer, 1, file_size, file);
    fclose(file);

    HParser *parser = parse_ntpv4_packet();
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse NTPv4 packet\n");
        free(buffer);
        return 1;
    }

    NTPv4Packet *packet = (NTPv4Packet*)result->ast;

    printf("LI: %u\n", packet->li);
    printf("VN: %u\n", packet->vn);
    printf("Mode: %u\n", packet->mode);
    printf("Stratum: %u\n", packet->stratum);
    printf("Poll: %u\n", packet->poll);
    printf("Precision: %d\n", packet->precision);
    printf("Root Delay: %u\n", packet->root_delay);
    printf("Root Dispersion: %u\n", packet->root_dispersion);
    printf("Reference ID: %u\n", packet->ref_id);
    printf("Reference Timestamp: %lu\n", packet->ref_timestamp);
    printf("Origin Timestamp: %lu\n", packet->orig_timestamp);
    printf("Receive Timestamp: %lu\n", packet->recv_timestamp);
    printf("Transmit Timestamp: %lu\n", packet->transmit_timestamp);
    if (packet->key_id) {
        printf("Key Identifier: %u\n", packet->key_id);
    }
    if (packet->message_digest[0]) {
        printf("Message Digest: ");
        for (int i = 0; i < 16; i++) {
            printf("%02x", packet->message_digest[i]);
        }
        printf("\n");
    }

    free(buffer);
    h_parse_result_free(result);
    return 0;
}