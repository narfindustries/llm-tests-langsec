#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t NTP_VERSION = 4;

// Core NTP fields parsers
HParser* ntp_flags() {
    return h_bits(8, false);
}

HParser* ntp_stratum() {
    return h_uint8();
}

HParser* ntp_poll() {
    return h_int8();
}

HParser* ntp_precision() {
    return h_int8();
}

HParser* ntp_root_delay() {
    return h_uint32();
}

HParser* ntp_root_dispersion() {
    return h_uint32();
}

HParser* ntp_ref_id() {
    return h_uint32();
}

HParser* ntp_timestamp() {
    return h_sequence(h_uint32(), h_uint32(), NULL);
}

// Main NTP packet parser
HParser* ntp_packet() {
    return h_sequence(
        ntp_flags(),
        ntp_stratum(),
        ntp_poll(),
        ntp_precision(),
        ntp_root_delay(),
        ntp_root_dispersion(),
        ntp_ref_id(),
        ntp_timestamp(),  // Reference Timestamp
        ntp_timestamp(),  // Origin Timestamp
        ntp_timestamp(),  // Receive Timestamp
        ntp_timestamp(),  // Transmit Timestamp
        h_optional(h_many(h_uint8())),  // Optional extension fields
        NULL
    );
}

// Helper functions for bit field access
uint8_t get_leap_indicator(uint8_t flags) {
    return (flags >> 6) & 0x03;
}

uint8_t get_version(uint8_t flags) {
    return (flags >> 3) & 0x07;
}

uint8_t get_mode(uint8_t flags) {
    return flags & 0x07;
}

// Initialize parser
HParser* init_ntp_parser() {
    return ntp_packet();
}

// Main entry point
int main() {
    return 0;
}