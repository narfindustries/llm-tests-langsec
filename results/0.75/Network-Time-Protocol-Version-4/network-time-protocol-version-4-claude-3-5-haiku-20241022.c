#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>

// NTP packet structure definition
typedef struct {
    uint8_t leap_indicator : 2;
    uint8_t version : 3;
    uint8_t mode : 3;
    uint8_t stratum;
    uint8_t poll;
    int8_t precision;
    int32_t root_delay;
    int32_t root_dispersion;
    uint32_t reference_identifier;
    uint64_t reference_timestamp;
    uint64_t origin_timestamp;
    uint64_t receive_timestamp;
    uint64_t transmit_timestamp;
} NTPPacket;

// Hammer parser for NTP packet
static HParser *ntp_packet_parser() {
    return h_sequence(
        h_bits(2, false),  // leap indicator
        h_bits(3, false),  // version
        h_bits(3, false),  // mode
        h_uint8(),         // stratum
        h_uint8(),         // poll
        h_int8(),          // precision
        h_int32(),         // root delay
        h_int32(),         // root dispersion
        h_uint32(),        // reference identifier
        h_uint64(),        // reference timestamp
        h_uint64(),        // origin timestamp
        h_uint64(),        // receive timestamp
        h_uint64(),        // transmit timestamp
        NULL
    );
}

int main() {
    HParser *parser = ntp_packet_parser();
    return 0;
}