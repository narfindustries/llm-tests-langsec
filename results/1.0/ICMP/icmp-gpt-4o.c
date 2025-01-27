#include <hammer/hammer.h>

HParser *create_icmp_parser() {
    // Define ICMP Type parser
    HParser *type_parser = h_uint8();

    // Define ICMP Code parser
    HParser *code_parser = h_uint8();

    // Define Checksum parser
    HParser *checksum_parser = h_uint16();

    // Combine the above parsers into a sequence for the ICMP header
    HParser *icmp_header = h_sequence(type_parser, code_parser, checksum_parser, NULL);

    // Define the variable data payload field (through rest of the input)
    HParser *payload_parser = h_bind(h_left_data(0, h_eof()), h_blob);

    // Combine header and data payload into the complete ICMP parser
    HParser *icmp_packet_parser = h_sequence(icmp_header, payload_parser, NULL);

    return icmp_packet_parser;
}

int main(int argc, char **argv) {
    HParser *parser = create_icmp_parser();
    HParseResult *result;
    
    // Assume 'data' contains the ICMP packet and 'data_len' its length
    const uint8_t data[] = {/* ICMP packet bytes */};
    size_t data_len = sizeof(data);
    
    // Parse the packet
    result = h_parse(parser, data, data_len);
    
    if (result) {
        // Successfully parsed the ICMP packet
        h_parse_result_free(result);
    } else {
        // Handle parse error
    }
    
    h_parser_free(parser);
    return 0;
}
