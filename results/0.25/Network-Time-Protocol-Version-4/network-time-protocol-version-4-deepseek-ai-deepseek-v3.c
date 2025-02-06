#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

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
    uint8_t digest[20];
} ntp_packet_t;

HParser *ntp_parser() {
    HParser *li = h_bits(2, false);
    HParser *vn = h_bits(3, false);
    HParser *mode = h_bits(3, false);
    HParser *stratum = h_uint8();
    HParser *poll = h_uint8();
    HParser *precision = h_int8();
    HParser *root_delay = h_uint32();
    HParser *root_dispersion = h_uint32();
    HParser *ref_id = h_uint32();
    HParser *ref_timestamp = h_uint64();
    HParser *orig_timestamp = h_uint64();
    HParser *recv_timestamp = h_uint64();
    HParser *transmit_timestamp = h_uint64();
    HParser *key_id = h_optional(h_uint32());
    HParser *digest = h_optional(h_repeat_n(h_uint8(), 20));

    return h_sequence(
        li, vn, mode, stratum, poll, precision, root_delay, root_dispersion,
        ref_id, ref_timestamp, orig_timestamp, recv_timestamp, transmit_timestamp,
        key_id, digest, NULL
    );
}

void parse_ntp_packet(const uint8_t *data, size_t size) {
    HParseResult *result = h_parse(ntp_parser(), data, size);
    if (result) {
        ntp_packet_t packet;
        packet.li = h_act_uint8(result->ast->seq->elements[0]);
        packet.vn = h_act_uint8(result->ast->seq->elements[1]);
        packet.mode = h_act_uint8(result->ast->seq->elements[2]);
        packet.stratum = h_act_uint8(result->ast->seq->elements[3]);
        packet.poll = h_act_uint8(result->ast->seq->elements[4]);
        packet.precision = h_act_int8(result->ast->seq->elements[5]);
        packet.root_delay = h_act_uint32(result->ast->seq->elements[6]);
        packet.root_dispersion = h_act_uint32(result->ast->seq->elements[7]);
        packet.ref_id = h_act_uint32(result->ast->seq->elements[8]);
        packet.ref_timestamp = h_act_uint64(result->ast->seq->elements[9]);
        packet.orig_timestamp = h_act_uint64(result->ast->seq->elements[10]);
        packet.recv_timestamp = h_act_uint64(result->ast->seq->elements[11]);
        packet.transmit_timestamp = h_act_uint64(result->ast->seq->elements[12]);
        if (result->ast->seq->elements[13]) {
            packet.key_id = h_act_uint32(result->ast->seq->elements[13]);
        } else {
            packet.key_id = 0;
        }
        if (result->ast->seq->elements[14]) {
            for (int i = 0; i < 20; i++) {
                packet.digest[i] = h_act_uint8(result->ast->seq->elements[14]->seq->elements[i]);
            }
        } else {
            memset(packet.digest, 0, 20);
        }

        printf("LI: %u\n", packet.li);
        printf("VN: %u\n", packet.vn);
        printf("Mode: %u\n", packet.mode);
        printf("Stratum: %u\n", packet.stratum);
        printf("Poll: %u\n", packet.poll);
        printf("Precision: %d\n", packet.precision);
        printf("Root Delay: %u\n", packet.root_delay);
        printf("Root Dispersion: %u\n", packet.root_dispersion);
        printf("Reference ID: %u\n", packet.ref_id);
        printf("Reference Timestamp: %lu\n", packet.ref_timestamp);
        printf("Origin Timestamp: %lu\n", packet.orig_timestamp);
        printf("Receive Timestamp: %lu\n", packet.recv_timestamp);
        printf("Transmit Timestamp: %lu\n", packet.transmit_timestamp);
        printf("Key ID: %u\n", packet.key_id);
        printf("Digest: ");
        for (int i = 0; i < 20; i++) {
            printf("%02x", packet.digest[i]);
        }
        printf("\n");

        h_parse_result_free(result);
    } else {
        printf("Failed to parse NTP packet.\n");
    }
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <binary file>\n", argv[0]);
        return 1;
    }

    FILE *file = fopen(argv[1], "rb");
    if (!file) {
        perror("Failed to open file");
        return 1;
    }

    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    fread(data, 1, size, file);
    fclose(file);

    parse_ntp_packet(data, size);

    free(data);
    return 0;
}