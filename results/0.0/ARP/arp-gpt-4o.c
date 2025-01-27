#include <hammer/hammer.h>

HParser *create_arp_parser() {
    // Define the ARP header fields
    HParser *htype = h_uint16();
    HParser *ptype = h_uint16();
    HParser *hlen = h_uint8();
    HParser *plen = h_uint8();
    HParser *oper = h_uint16();
    HParser *sha = h_repeat_n(h_uint8(), 6); // Sender hardware address
    HParser *spa = h_repeat_n(h_uint8(), 4); // Sender protocol address
    HParser *tha = h_repeat_n(h_uint8(), 6); // Target hardware address
    HParser *tpa = h_repeat_n(h_uint8(), 4); // Target protocol address

    // Combine the fields into an ARP packet parser
    HParser *arp_packet = h_sequence(
        htype,
        ptype,
        hlen,
        plen,
        oper,
        sha,
        spa,
        tha,
        tpa,
        NULL
    );

    return arp_packet;
}

int main(int argc, char **argv) {
    // Create the ARP parser
    HParser *arp_parser = create_arp_parser();

    // Example ARP packet data (replace with actual data)
    uint8_t arp_data[] = {
        0x00, 0x01, // htype: Ethernet
        0x08, 0x00, // ptype: IPv4
        0x06,       // hlen: 6
        0x04,       // plen: 4
        0x00, 0x01, // oper: request
        0x00, 0x0c, 0x29, 0xab, 0xcd, 0xef, // sha: sender hardware address
        0xc0, 0xa8, 0x01, 0x01,             // spa: sender protocol address
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // tha: target hardware address
        0xc0, 0xa8, 0x01, 0x02              // tpa: target protocol address
    };

    // Parse the ARP packet
    HParseResult *result = h_parse(arp_parser, arp_data, sizeof(arp_data));

    if (result) {
        printf("ARP packet parsed successfully.\n");
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ARP packet.\n");
    }

    // Free the parser
    h_parser_free(arp_parser);

    return 0;
}