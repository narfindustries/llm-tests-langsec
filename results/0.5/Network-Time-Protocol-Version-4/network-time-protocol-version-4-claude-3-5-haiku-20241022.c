#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// NTP Packet Structure Specification
typedef struct {
    uint8_t leap_indicator;
    uint8_t version;
    uint8_t mode;
    uint8_t stratum;
    uint8_t poll_interval;
    int8_t precision;
    int32_t root_delay;
    int32_t root_dispersion;
    uint32_t reference_timestamp;
    uint32_t originate_timestamp;
    uint32_t receive_timestamp;
    uint32_t transmit_timestamp;
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
static HParser *ntp_reference_timestamp;
static HParser *ntp_originate_timestamp;
static HParser *ntp_receive_timestamp;
static HParser *ntp_transmit_timestamp;
static HParser *ntp_packet_parser;

// Parser Initialization Function
void init_ntp_parsers() {
    ntp_leap_indicator = h_bits(2, false);
    ntp_version = h_bits(3, false);
    ntp_mode = h_bits(3, false);
    ntp_stratum = h_bits(8, false);
    ntp_poll_interval = h_bits(8, false);
    ntp_precision = h_signed_bits(8, false);
    ntp_root_delay = h_signed_bits(32, false);
    ntp_root_dispersion = h_bits(32, false);
    ntp_reference_timestamp = h_bits(32, false);
    ntp_originate_timestamp = h_bits(32, false);
    ntp_receive_timestamp = h_bits(32, false);
    ntp_transmit_timestamp = h_bits(32, false);

    ntp_packet_parser = h_sequence(
        ntp_leap_indicator,
        ntp_version,
        ntp_mode,
        ntp_stratum,
        ntp_poll_interval,
        ntp_precision,
        ntp_root_delay,
        ntp_root_dispersion,
        ntp_reference_timestamp,
        ntp_originate_timestamp,
        ntp_receive_timestamp,
        ntp_transmit_timestamp,
        NULL
    );
}

// NTP Packet Parsing Function
int parse_ntp_packet(uint8_t *data, size_t len) {
    HParseResult *result = h_parse(ntp_packet_parser, data, len);
    
    if (result == NULL) {
        fprintf(stderr, "NTP Packet Parsing Failed\n");
        return -1;
    }

    NTPPacket packet;
    // Extract parsed values into packet structure
    packet.leap_indicator = h_act_first(result->ast)->uint;
    packet.version = h_act_first(result->ast)->uint;
    packet.mode = h_act_first(result->ast)->uint;
    packet.stratum = h_act_first(result->ast)->uint;
    packet.poll_interval = h_act_first(result->ast)->uint;
    packet.precision = h_act_first(result->ast)->sint;
    packet.root_delay = h_act_first(result->ast)->sint;
    packet.root_dispersion = h_act_first(result->ast)->uint;
    packet.reference_timestamp = h_act_first(result->ast)->uint;
    packet.originate_timestamp = h_act_first(result->ast)->uint;
    packet.receive_timestamp = h_act_first(result->ast)->uint;
    packet.transmit_timestamp = h_act_first(result->ast)->uint;

    h_parse_result_free(result);
    return 0;
}

int main() {
    h_init();
    init_ntp_parsers();

    // Example NTP packet data
    uint8_t ntp_data[] = {
        0x24, 0x01, 0x03, 0xEC, 0x0A, 0xF0, 
        0x00, 0x01, 0x00, 0x00, 0x00, 0x00,
        0xC0, 0xA8, 0x01, 0x01, 0x00, 0x00, 
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    };

    int result = parse_ntp_packet(ntp_data, sizeof(ntp_data));
    
    return result;
}