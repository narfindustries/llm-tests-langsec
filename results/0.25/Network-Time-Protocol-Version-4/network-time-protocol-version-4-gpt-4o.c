#include <hammer/hammer.h>

HParser *create_ntp_parser() {
    // Define basic types
    HParser *uint8 = h_uint8();
    HParser *uint16 = h_uint16();
    HParser *uint32 = h_uint32();

    // Define NTP header fields
    HParser *leap_indicator = h_bits(2, false);
    HParser *version_number = h_bits(3, false);
    HParser *mode = h_bits(3, false);
    HParser *stratum = uint8;
    HParser *poll = uint8;
    HParser *precision = uint8;
    HParser *root_delay = uint32;
    HParser *root_dispersion = uint32;
    HParser *reference_id = uint32;
    HParser *reference_timestamp = h_repeat_n(uint32, 2);
    HParser *originate_timestamp = h_repeat_n(uint32, 2);
    HParser *receive_timestamp = h_repeat_n(uint32, 2);
    HParser *transmit_timestamp = h_repeat_n(uint32, 2);

    // Combine fields into NTP header
    HParser *ntp_header = h_sequence(
        leap_indicator,
        version_number,
        mode,
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

    return ntp_header;
}

int main() {
    HParser *ntp_parser = create_ntp_parser();

    // Example usage of the parser
    // This part would typically involve reading NTP data and parsing it
    // For demonstration purposes, this is left as a placeholder

    // Cleanup
    h_parser_free(ntp_parser);

    return 0;
}