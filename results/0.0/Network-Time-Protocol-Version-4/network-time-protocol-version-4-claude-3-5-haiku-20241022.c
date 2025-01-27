#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

// NTP Packet Structure Specification
typedef struct {
    uint8_t leap_indicator : 2;
    uint8_t version : 3;
    uint8_t mode : 3;
    uint8_t stratum;
    uint8_t poll_interval;
    int8_t precision;
    uint32_t root_delay;
    uint32_t root_dispersion;
    uint32_t reference_identifier;
    uint64_t reference_timestamp;
    uint64_t originate_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} NTPPacket;

// Hammer Parser Definitions
static HParser *ntp_leap_indicator;
static HParser *ntp_version;
static HParser *ntp_mode;
static HParser *ntp_stratum;
static HParser *ntp_poll_interval;
static HParser *ntp_precision;
static HParser *ntp_root_delay;
static HParser *ntp_root_dispersion;
static HParser *ntp_reference_identifier;
static HParser *ntp_reference_timestamp;
static HParser *ntp_originate_timestamp;
static HParser *ntp_receive_timestamp;
static HParser *ntp_transmit_timestamp;
static HParser *ntp_packet_parser;

// Parser Construction Function
static void create_ntp_parsers() {
    ntp_leap_indicator = h_bits(2, false);
    ntp_version = h_bits(3, false);
    ntp_mode = h_bits(3, false);
    ntp_stratum = h_bits(8, false);
    ntp_poll_interval = h_bits(8, false);
    ntp_precision = h_signed_bits(8, false);
    ntp_root_delay = h_bits(32, false);
    ntp_root_dispersion = h_bits(32, false);
    ntp_reference_identifier = h_bits(32, false);
    ntp_reference_timestamp = h_bits(64, false);
    ntp_originate_timestamp = h_bits(64, false);
    ntp_receive_timestamp = h_bits(64, false);
    ntp_transmit_timestamp = h_bits(64, false);

    ntp_packet_parser = h_sequence(
        ntp_leap_indicator,
        ntp_version,
        ntp_mode,
        ntp_stratum,
        ntp_poll_interval,
        ntp_precision,
        ntp_root_delay,
        ntp_root_dispersion,
        ntp_reference_identifier,
        ntp_reference_timestamp,
        ntp_originate_timestamp,
        ntp_receive_timestamp,
        ntp_transmit_timestamp,
        NULL
    );
}

// Main Parsing Function
static bool parse_ntp_packet(const uint8_t *data, size_t len) {
    create_ntp_parsers();
    
    HParseResult *result = h_parse(ntp_packet_parser, data, len);
    
    if (result == NULL) {
        return false;
    }

    // Optional: Extract and validate parsed data
    NTPPacket packet;
    // Populate packet structure from parse result if needed
    
    h_parse_result_free(result);
    return true;
}

int main() {
    // Example NTP packet data
    uint8_t ntp_data[] = {
        0x24, 0x01, 0x03, 0xE9, 
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0xC0, 0xA8, 0x01, 0x01,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00,
        0x00, 0x00, 0x00, 0x00
    };

    bool result = parse_ntp_packet(ntp_data, sizeof(ntp_data));
    printf("NTP Packet Parsing %s\n", result ? "Successful" : "Failed");

    return 0;
}