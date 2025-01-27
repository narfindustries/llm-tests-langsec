#include <hammer/hammer.h>

static HParser *create_ntp_parser() {
    // Define basic types
    HParser *uint8 = h_uint8();
    HParser *uint16 = h_uint16();
    HParser *uint32 = h_uint32();

    // NTP Packet Structure
    HParser *li_vn_mode = h_bits(8); // Leap Indicator (2 bits), Version Number (3 bits), Mode (3 bits)
    HParser *stratum = uint8; // Stratum (8 bits)
    HParser *poll = uint8; // Poll Interval (8 bits)
    HParser *precision = uint8; // Precision (8 bits)
    HParser *root_delay = uint32; // Root Delay (32 bits)
    HParser *root_dispersion = uint32; // Root Dispersion (32 bits)
    HParser *reference_id = uint32; // Reference ID (32 bits)
    HParser *reference_timestamp = h_repeat_n(uint32, 2); // Reference Timestamp (64 bits)
    HParser *originate_timestamp = h_repeat_n(uint32, 2); // Originate Timestamp (64 bits)
    HParser *receive_timestamp = h_repeat_n(uint32, 2); // Receive Timestamp (64 bits)
    HParser *transmit_timestamp = h_repeat_n(uint32, 2); // Transmit Timestamp (64 bits)

    // Combine all components into the final NTP packet parser
    HParser *ntp_packet = h_sequence(
        li_vn_mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        originate_timestamp,
        receive_timestamp,
        transmit_timestamp,
        NULL
    );

    return ntp_packet;
}

int main() {
    // Create the NTP parser
    HParser *ntp_parser = create_ntp_parser();

    // Clean up
    h_parser_free(ntp_parser);

    return 0;
}