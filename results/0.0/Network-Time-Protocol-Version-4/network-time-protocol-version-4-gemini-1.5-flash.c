#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <arpa/inet.h>

typedef struct {
    uint8_t li : 3;
    uint8_t vn : 3;
    uint8_t mode : 3;
} ntp_header_t;

typedef struct {
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t ref_id;
    uint64_t ref_timestamp;
    uint64_t orig_timestamp;
    uint64_t recv_timestamp;
    uint64_t xmit_timestamp;
} ntp_body_t;

typedef struct {
    ntp_header_t header;
    ntp_body_t body;
} ntp_packet_t;

hammer_parser ntp_parser() {
    hammer_parser header_parser = h_seq(
        h_uint8_bits_field(&ntp_header_t::li, 3),
        h_uint8_bits_field(&ntp_header_t::vn, 3),
        h_uint8_bits_field(&ntp_header_t::mode, 3)
    );

    hammer_parser body_parser = h_seq(
        h_uint8_field(&ntp_body_t::stratum),
        h_uint8_field(&ntp_body_t::poll),
        h_uint8_field(&ntp_body_t::precision),
        h_uint32_field(&ntp_body_t::root_delay),
        h_uint32_field(&ntp_body_t::root_dispersion),
        h_uint32_field(&ntp_body_t::ref_id),
        h_uint64_field(&ntp_body_t::ref_timestamp),
        h_uint64_field(&ntp_body_t::orig_timestamp),
        h_uint64_field(&ntp_body_t::recv_timestamp),
        h_uint64_field(&ntp_body_t::xmit_timestamp)
    );

    return h_map2(header_parser, body_parser, [](ntp_header_t h, ntp_body_t b) {
        ntp_packet_t p;
        p.header = h;
        p.body = b;
        return p;
    });
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary_file>\n", argv[0]);
        return 1;
    }

    FILE *fp = fopen(argv[1], "rb");
    if (fp == NULL) {
        perror("Error opening file");
        return 1;
    }

    fseek(fp, 0, SEEK_END);
    long fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    char *buffer = (char *)malloc(fsize);
    if (buffer == NULL) {
        perror("Memory allocation failed");
        fclose(fp);
        return 1;
    }

    fread(buffer, 1, fsize, fp);
    fclose(fp);

    hammer_result result = h_parse(ntp_parser(), buffer, fsize);

    if (result.success) {
        ntp_packet_t packet = *(ntp_packet_t *)result.value;
        printf("NTP Packet:\n");
        printf("  LI: %u\n", packet.header.li);
        printf("  VN: %u\n", packet.header.vn);
        printf("  Mode: %u\n", packet.header.mode);
        printf("  Stratum: %u\n", packet.body.stratum);
        printf("  Poll: %u\n", packet.body.poll);
        printf("  Precision: %d\n", (int8_t)packet.body.precision);
        printf("  Root Delay: %u\n", packet.body.root_delay);
        printf("  Root Dispersion: %u\n", packet.body.root_dispersion);
        printf("  Reference ID: 0x%08X\n", packet.body.ref_id);
        printf("  Reference Timestamp: %llu\n", packet.body.ref_timestamp);
        printf("  Originate Timestamp: %llu\n", packet.body.orig_timestamp);
        printf("  Receive Timestamp: %llu\n", packet.body.recv_timestamp);
        printf("  Transmit Timestamp: %llu\n", packet.body.xmit_timestamp);
    } else {
        fprintf(stderr, "Parsing failed at offset %zu: %s\n", result.offset, result.error);
        return 1;
    }

    free(buffer);
    return 0;
}
