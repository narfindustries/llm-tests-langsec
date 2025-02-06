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
    uint32_t ref_id;
    uint64_t ref_timestamp;
    uint64_t orig_timestamp;
    uint64_t recv_timestamp;
    uint64_t transmit_timestamp;
} ntp_packet_t;

HParser *ntp_parser() {
    HParser *li = h_bits(2, false);
    HParser *vn = h_bits(3, false);
    HParser *mode = h_bits(3, false);
    HParser *stratum = h_uint8();
    HParser *poll = h_uint8();
    HParser *precision = h_uint8();
    HParser *root_delay = h_uint32();
    HParser *root_dispersion = h_uint32();
    HParser *ref_id = h_uint32();
    HParser *ref_timestamp = h_uint64();
    HParser *orig_timestamp = h_uint64();
    HParser *recv_timestamp = h_uint64();
    HParser *transmit_timestamp = h_uint64();

    return h_sequence(li, vn, mode, stratum, poll, precision, root_delay, root_dispersion, ref_id, ref_timestamp, orig_timestamp, recv_timestamp, transmit_timestamp, NULL);
}

void print_ntp_packet(ntp_packet_t *packet) {
    printf("LI: %u\n", packet->li);
    printf("VN: %u\n", packet->vn);
    printf("Mode: %u\n", packet->mode);
    printf("Stratum: %u\n", packet->stratum);
    printf("Poll: %u\n", packet->poll);
    printf("Precision: %u\n", packet->precision);
    printf("Root Delay: %u\n", packet->root_delay);
    printf("Root Dispersion: %u\n", packet->root_dispersion);
    printf("Reference ID: %u\n", packet->ref_id);
    printf("Reference Timestamp: %lu\n", packet->ref_timestamp);
    printf("Originate Timestamp: %lu\n", packet->orig_timestamp);
    printf("Receive Timestamp: %lu\n", packet->recv_timestamp);
    printf("Transmit Timestamp: %lu\n", packet->transmit_timestamp);
}

void extract_ntp_packet(ntp_packet_t *packet, const HParsedToken *tokens) {
    packet->li = *(uint8_t *)h_sequence_get(tokens, 0);
    packet->vn = *(uint8_t *)h_sequence_get(tokens, 1);
    packet->mode = *(uint8_t *)h_sequence_get(tokens, 2);
    packet->stratum = *(uint8_t *)h_sequence_get(tokens, 3);
    packet->poll = *(uint8_t *)h_sequence_get(tokens, 4);
    packet->precision = *(uint8_t *)h_sequence_get(tokens, 5);
    packet->root_delay = *(uint32_t *)h_sequence_get(tokens, 6);
    packet->root_dispersion = *(uint32_t *)h_sequence_get(tokens, 7);
    packet->ref_id = *(uint32_t *)h_sequence_get(tokens, 8);
    packet->ref_timestamp = *(uint64_t *)h_sequence_get(tokens, 9);
    packet->orig_timestamp = *(uint64_t *)h_sequence_get(tokens, 10);
    packet->recv_timestamp = *(uint64_t *)h_sequence_get(tokens, 11);
    packet->transmit_timestamp = *(uint64_t *)h_sequence_get(tokens, 12);
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <input_file>\n", argv[0]);
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
    free(buffer);

    if (!result) {
        fprintf(stderr, "Failed to parse NTP packet\n");
        return 1;
    }

    ntp_packet_t packet;
    extract_ntp_packet(&packet, result->ast);

    print_ntp_packet(&packet);

    h_parse_result_free(result);
    h_parser_free(parser);

    return 0;
}