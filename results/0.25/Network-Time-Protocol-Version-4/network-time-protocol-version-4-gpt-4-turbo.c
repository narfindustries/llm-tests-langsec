#include <hammer/hammer.h>
#include <stdio.h>
#include <stdlib.h>

// Define constants for NTP
#define LI_MASK 0xC0
#define VN_MASK 0x38
#define MODE_MASK 0x07
#define STRATUM_MASK 0xFF
#define POLL_MASK 0xFF
#define PRECISION_MASK 0xFF

// NTP Header structure
typedef struct {
    uint8_t li_vn_mode;   // 8 bits combining LI, VN, and Mode
    uint8_t stratum;      // 8 bits stratum
    uint8_t poll;         // 8 bits poll interval
    uint8_t precision;    // 8 bits precision
    uint32_t root_delay;          // 32 bits root delay
    uint32_t root_dispersion;     // 32 bits root dispersion
    uint32_t ref_id;              // 32 bits reference identifier
    uint64_t ref_timestamp;       // 64 bits reference timestamp
    uint64_t orig_timestamp;      // 64 bits originate timestamp
    uint64_t recv_timestamp;      // 64 bits receive timestamp
    uint64_t trans_timestamp;     // 64 bits transmit timestamp
} ntp_packet;

// Hammer parsers for NTP fields
HParser *ntp_li_vn_mode, *ntp_stratum, *ntp_poll, *ntp_precision;
HParser *ntp_root_delay, *ntp_root_dispersion, *ntp_ref_id;
HParser *ntp_ref_timestamp, *ntp_orig_timestamp, *ntp_recv_timestamp, *ntp_trans_timestamp;
HParser *ntp_packet_parser;

void init_parsers() {
    ntp_li_vn_mode = h_bits(8, false);
    ntp_stratum = h_bits(8, false);
    ntp_poll = h_bits(8, false);
    ntp_precision = h_bits(8, false);
    ntp_root_delay = h_bits(32, false);
    ntp_root_dispersion = h_bits(32, false);
    ntp_ref_id = h_bits(32, false);
    ntp_ref_timestamp = h_bits(64, false);
    ntp_orig_timestamp = h_bits(64, false);
    ntp_recv_timestamp = h_bits(64, false);
    ntp_trans_timestamp = h_bits(64, false);

    ntp_packet_parser = h_sequence(ntp_li_vn_mode, ntp_stratum, ntp_poll, ntp_precision,
                                    ntp_root_delay, ntp_root_dispersion, ntp_ref_id,
                                    ntp_ref_timestamp, ntp_orig_timestamp, ntp_recv_timestamp,
                                    ntp_trans_timestamp, NULL);
}

int main(int argc, char **argv) {
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
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    uint8_t *data = malloc(size);
    if (!data) {
        perror("Failed to allocate memory");
        fclose(file);
        return 1;
    }

    if (fread(data, 1, size, file) != size) {
        fprintf(stderr, "Failed to read the file\n");
        free(data);
        fclose(file);
        return 1;
    }

    fclose(file);

    init_parsers();

    HParseResult *result = h_parse(ntp_packet_parser, data, size);
    if (result) {
        ntp_packet *parsed_packet = (ntp_packet*)result->ast;
        printf("Parsed NTP packet:\n");
        printf("LI_VN_Mode: %02X\n", parsed_packet->li_vn_mode);
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

    free(data);
    return 0;
}