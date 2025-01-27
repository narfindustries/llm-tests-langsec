#include <hammer/hammer.h>

HParser *icmp_type_parser() {
    return h_choice(h_int8(8), h_int8(0), NULL);
}

HParser *icmp_code_parser() {
    return h_choice(
        h_int8(0),  // Echo Reply
        h_int8(3),  // Destination Unreachable
        h_int8(11), // Time Exceeded
        h_int8(8),  // Echo Request
        NULL
    );
}

HParser *icmp_checksum_parser() {
    return h_int16();
}

HParser *icmp_identifier_parser() {
    return h_int16();
}

HParser *icmp_sequence_number_parser() {
    return h_int16();
}

HParser *icmp_header_parser() {
    return h_sequence(
        icmp_type_parser(),
        icmp_code_parser(),
        icmp_checksum_parser(),
        icmp_identifier_parser(),
        icmp_sequence_number_parser(),
        NULL
    );
}

HParser *create_icmp_parser() {
    return icmp_header_parser();
}

int main() {
    HParser *parser = create_icmp_parser();
    // Here you can add code to use the parser, like parsing input data.
    h_parser_free(parser);
    return 0;
}