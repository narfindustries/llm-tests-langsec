#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define the ARP packet structure
static HParser *arp_packet;

static void init_arp_parser() {
    HParser *htype = h_uint16();  // Hardware type
    HParser *ptype = h_uint16();  // Protocol type
    HParser *hlen = h_uint8();    // Hardware address length
    HParser *plen = h_uint8();    // Protocol address length
    HParser *oper = h_uint16();   // Operation (request/reply)
    HParser *sha = h_bits(8*6, false);  // Sender hardware address (48 bits)
    HParser *spa = h_bits(8*4, false);  // Sender protocol address (32 bits)
    HParser *tha = h_bits(8*6, false);  // Target hardware address (48 bits)
    HParser *tpa = h_bits(8*4, false);  // Target protocol address (32 bits)

    arp_packet = h_sequence(htype, ptype, hlen, plen, oper, sha, spa, tha, tpa, NULL);
}

int main(int argc, char **argv) {
    HParser *parser;
    HParseResult *result;
    HBitReader *br;
    uint8_t input[] = {
        0x00, 0x01,  // Hardware type (Ethernet)
        0x08, 0x00,  // Protocol type (IPv4)
        0x06,        // Hardware size
        0x04,        // Protocol size
        0x00, 0x01,  // Opcode (request)
        0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC,  // Sender MAC
        0xC0, 0xA8, 0x01, 0x01,              // Sender IP
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Target MAC (unknown)
        0xC0, 0xA8, 0x01, 0x02               // Target IP
    };

    init_arp_parser();
    parser = arp_packet;
    br = h_bit_reader_new(input, sizeof(input));
    result = h_parse(parser, br);

    if (result) {
        printf("ARP packet parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ARP packet.\n");
    }

    h_bit_reader_free(br);
    h_parser_unref(parser);
    return 0;
}