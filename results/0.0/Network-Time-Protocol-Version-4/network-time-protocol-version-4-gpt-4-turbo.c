#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define NTP constants
#define LI_MASK 0xC0
#define VN_MASK 0x38
#define MODE_MASK 0x07
#define STRATUM_MASK 0xFF
#define POLL_MASK 0xFF
#define PRECISION_MASK 0xFF

// Define structure for NTP header
typedef struct {
    uint8_t li_vn_mode; // Leap Indicator, Version, Mode
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
} ntp_packet;

// Hammer parsers for NTP fields
static HParser *ntp_li_vn_mode;
static HParser *ntp_stratum;
static HParser *ntp_poll;
static HParser *ntp_precision;
static HParser *ntp_root_delay;
static HParser *ntp_root_dispersion;
static HParser *ntp_ref_id;
static HParser *ntp_timestamp;
static HParser *ntp_packet_parser;

void init_parsers() {
    ntp_li_vn_mode = h_bits(8, false);
    ntp_stratum = h_bits(8, false);
    ntp_poll = h_bits(8, false);
    ntp_precision = h_bits(8, false);
    ntp_root_delay = h_bits(32, false);
    ntp_root_dispersion = h_bits(32, false);
    ntp_ref_id = h_bits(32, false);
    ntp_timestamp = h_bits(64, false);

    ntp_packet_parser = h_sequence(ntp_li_vn_mode, ntp_stratum, ntp_poll, ntp_precision,
                                    ntp_root_delay, ntp_root_dispersion, ntp_ref_id,
                                    ntp_timestamp, ntp_timestamp, ntp_timestamp, ntp_timestamp,
                                    NULL);
}

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "Usage: %s <binary file path>\n", argv[0]);
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

    init_parsers();

    HParseResult *result = h_parse(ntp_packet_parser, buffer, file_size);
    if (result) {
        ntp_packet *parsed_packet = (ntp_packet *)result->ast;
        printf("Leap Indicator, Version, Mode: %02x\n", parsed_packet->li_vn_mode);
        printf("Stratum: %d\n", parsed_packet->stratum);
        printf("Poll: %d\n", parsed_packet->poll);
        printf("Precision: %d\n", parsed_packet->precision);
        printf("Root Delay: %u\n", parsed_packet->root_delay);
        printf("Root Dispersion: %u\n", parsed_packet->root_dispersion);
        printf("Reference ID: %u\n", parsed_packet->ref_id);
        printf("Reference Timestamp: %llu\n", parsed_packet->ref_timestamp);
        printf("Originate Timestamp: %llu\n", parsed_packet->orig_timestamp);
        printf("Receive Timestamp: %llu\n", parsed_packet->recv_timestamp);
        printf("Transmit Timestamp: %llu\n", parsed_packet->trans_timestamp);
    } else {
        fprintf(stderr, "Failed to parse NTP packet\n");
    }

    free(buffer);
    return 0;
}