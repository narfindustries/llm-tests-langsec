#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define the NTPv4 packet structure
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
} ntp_packet_t;

// Define the Hammer parser for NTPv4
HParser *ntp_parser() {
    return h_sequence(
        h_bits(2, NULL),  // Leap Indicator (LI)
        h_bits(3, NULL),  // Version Number (VN)
        h_bits(3, NULL),  // Mode
        h_bits(8, NULL),  // Stratum
        h_bits(8, NULL),  // Poll
        h_bits(8, NULL),  // Precision
        h_bits(32, NULL), // Root Delay
        h_bits(32, NULL), // Root Dispersion
        h_bits(32, NULL), // Reference ID
        h_bits(64, NULL), // Reference Timestamp
        h_bits(64, NULL), // Originate Timestamp
        h_bits(64, NULL), // Receive Timestamp
        h_bits(64, NULL), // Transmit Timestamp
        NULL
    );
}

// Main function to parse NTPv4 packet
int main() {
    uint8_t data[] = {
        // Example NTPv4 packet data
        0x23, 0x01, 0x06, 0xEC, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    HParser *parser = ntp_parser();
    HParseResult *result = h_parse(parser, data, sizeof(data));

    if (result) {
        printf("Parsing successful!\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed!\n");
    }

    h_parser_free(parser);
    return 0;
}