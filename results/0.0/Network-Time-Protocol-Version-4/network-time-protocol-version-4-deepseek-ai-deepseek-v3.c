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
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    uint64_t reference_timestamp;
    uint64_t origin_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
    uint32_t key_identifier;
    uint64_t mac[2]; // Representing 128 bits as two 64-bit integers
} ntp_packet;

HParser *ntp_parser() {
    return h_sequence(
        h_bits(2, false), // LI
        h_bits(3, false), // VN
        h_bits(3, false), // Mode
        h_uint8(), // Stratum
        h_uint8(), // Poll
        h_uint8(), // Precision
        h_uint32(), // Root Delay
        h_uint32(), // Root Dispersion
        h_uint32(), // Reference ID
        h_uint64(), // Reference Timestamp
        h_uint64(), // Origin Timestamp
        h_uint64(), // Receive Timestamp
        h_uint64(), // Transmit Timestamp
        h_optional(h_uint32()), // Key Identifier
        h_optional(h_sequence(h_uint64(), h_uint64(), NULL)), // MAC (128 bits as two 64-bit integers)
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

    HParser *parser = ntp_parser();
    HParseResult *result = h_parse(parser, buffer, file_size);
    if (!result) {
        fprintf(stderr, "Failed to parse NTP packet\n");
        free(buffer);
        return 1;
    }

    ntp_packet *packet = (ntp_packet *)result->ast;
    printf("LI: %u\n", packet->li);
    printf("VN: %u\n", packet->vn);
    printf("Mode: %u\n", packet->mode);
    printf("Stratum: %u\n", packet->stratum);
    printf("Poll: %u\n", packet->poll);
    printf("Precision: %u\n", packet->precision);
    printf("Root Delay: %u\n", packet->root_delay);
    printf("Root Dispersion: %u\n", packet->root_dispersion);
    printf("Reference ID: %u\n", packet->reference_id);
    printf("Reference Timestamp: %lu\n", packet->reference_timestamp);
    printf("Origin Timestamp: %lu\n", packet->origin_timestamp);
    printf("Receive Timestamp: %lu\n", packet->receive_timestamp);
    printf("Transmit Timestamp: %lu\n", packet->transmit_timestamp);
    if (packet->key_identifier) {
        printf("Key Identifier: %u\n", packet->key_identifier);
    }
    if (packet->mac[0] || packet->mac[1]) {
        printf("MAC: %lu%lu\n", packet->mac[0], packet->mac[1]);
    }

    h_parse_result_free(result);
    free(buffer);
    return 0;
}