#include <hammer/hammer.h>

HParser *create_ntp_parser() {
    // Define basic types
    HParser *byte = h_uint8();
    HParser *word = h_uint16();
    HParser *dword = h_uint32();
    HParser *qword = h_uint64();

    // Define bitfields
    HParser *leap_indicator = h_bits(2);
    HParser *version_number = h_bits(3);
    HParser *mode = h_bits(3);

    // Define the fixed header fields
    HParser *ntp_header = h_sequence(
        h_choice(
            h_token(h_bits_value(0, 2), leap_indicator),
            h_token(h_bits_value(1, 2), leap_indicator),
            h_token(h_bits_value(2, 2), leap_indicator),
            h_token(h_bits_value(3, 2), leap_indicator),
            NULL
        ),
        version_number,
        mode,
        byte,    // stratum
        byte,    // poll
        byte,    // precision
        dword,   // root delay
        dword,   // root dispersion
        dword,   // reference identifier
        qword,   // reference timestamp
        qword,   // originate timestamp
        qword,   // receive timestamp
        qword,   // transmit timestamp
        NULL
    );

    return ntp_header;
}

int main(void) {
    HParser *parser = create_ntp_parser();

    // Clean up the parser after usage
    h_parser_free(parser);

    return 0;
}