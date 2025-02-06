#include <hammer/hammer.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define NTP_HEADER_SIZE 48

// Define a struct to hold the NTP header fields
typedef struct {
    uint8_t li_vn_mode;
    uint8_t stratum;
    uint8_t poll;
    uint8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_id;
    uint64_t reference_timestamp;
    uint64_t originate_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} ntp_header_t;

// Hammer parsers for individual fields
HParser *parse_li_vn_mode() {
    return h_bits(2, h_uint8());
}

HParser *parse_stratum() {
    return h_uint8();
}

HParser *parse_poll() {
    return h_uint8();
}

HParser *parse_precision() {
    return h_uint8();
}

HParser *parse_root_delay() {
    return h_uint32();
}

HParser *parse_root_dispersion() {
    return h_uint32();
}

HParser *parse_reference_id() {
    return h_uint32();
}

HParser *parse_timestamp() {
    return h_uint64();
}

// Main parser for the NTP header
HParser *parse_ntp_header() {
    return h_sequence(
        parse_li_vn_mode(), parse_stratum(), parse_poll(), parse_precision(),
        parse_root_delay(), parse_root_dispersion(), parse_reference_id(),
        parse_timestamp(), parse_timestamp(), parse_timestamp(), parse_timestamp(),
        NULL
    );
}

// Function to parse an NTP packet from a binary file
ntp_header_t parse_ntp_packet(const char *filename) {
    FILE *file = fopen(filename, "rb");
    if (!file) {
        perror("Failed to open file");
        exit(EXIT_FAILURE);
    }

    uint8_t buffer[NTP_HEADER_SIZE];
    fread(buffer, 1, NTP_HEADER_SIZE, file);
    fclose(file);

    HParseResult *result = h_parse(parse_ntp_header(), buffer, NTP_HEADER_SIZE);
    if (!result) {
        fprintf(stderr, "Failed to parse NTP packet\n");
        exit(EXIT_FAILURE);
    }

    uint8_t *fields = (uint8_t *)result->ast;
    ntp_header_t header = {
        .li_vn_mode = fields[0],
        .stratum = fields[1],
        .poll = fields[2],
        .precision = fields[3],
        .root_delay = *(uint32_t *)(fields + 4),
        .root_dispersion = *(uint32_t *)(fields + 8),
        .reference_id = *(uint32_t *)(fields + 12),
        .reference_timestamp = *(uint64_t *)(fields + 16),
        .originate_timestamp = *(uint64_t *)(fields + 24),
        .receive_timestamp = *(uint64_t *)(fields + 32),
        .transmit_timestamp = *(uint64_t *)(fields + 40)
    };

    h_parse_result_free(result);
    return header;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file>\n", argv[0]);
        return EXIT_FAILURE;
    }

    ntp_header_t header = parse_ntp_packet(argv[1]);

    printf("LI/VN/Mode: 0x%02X\n", header.li_vn_mode);
    printf("Stratum: %u\n", header.stratum);
    printf("Poll: %u\n", header.poll);
    printf("Precision: %d\n", header.precision);
    printf("Root Delay: %u\n", header.root_delay);
    printf("Root Dispersion: %u\n", header.root_dispersion);
    printf("Reference ID: 0x%08X\n", header.reference_id);
    printf("Reference Timestamp: %llu\n", header.reference_timestamp);
    printf("Originate Timestamp: %llu\n", header.originate_timestamp);
    printf("Receive Timestamp: %llu\n", header.receive_timestamp);
    printf("Transmit Timestamp: %llu\n", header.transmit_timestamp);

    return EXIT_SUCCESS;
}