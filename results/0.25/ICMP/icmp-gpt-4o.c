#include <hammer/hammer.h>

HParser *create_icmp_parser() {
    // Define ICMP header fields
    HParser *type = h_uint8();
    HParser *code = h_uint8();
    HParser *checksum = h_uint16();
    HParser *rest_of_header = h_uint32();

    // Define the ICMP header structure
    HParser *icmp_header = h_sequence(type, code, checksum, rest_of_header, NULL);

    // Define the ICMP payload (variable length)
    HParser *payload = h_rest();

    // Combine header and payload into the full ICMP packet parser
    HParser *icmp_packet = h_sequence(icmp_header, payload, NULL);

    return icmp_packet;
}

int main(int argc, char **argv) {
    // Create the ICMP parser
    HParser *icmp_parser = create_icmp_parser();

    // Example usage: parse an ICMP packet from a buffer
    const uint8_t icmp_data[] = {
        0x08, 0x00, 0xf7, 0xff, // ICMP header: type, code, checksum
        0x00, 0x01, 0x02, 0x03, // Rest of header
        0x61, 0x62, 0x63, 0x64  // Payload: "abcd"
    };
    HParseResult *result = h_parse(icmp_parser, icmp_data, sizeof(icmp_data));

    if (result) {
        printf("ICMP packet parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ICMP packet.\n");
    }

    // Free the parser
    h_parser_free(icmp_parser);

    return 0;
}