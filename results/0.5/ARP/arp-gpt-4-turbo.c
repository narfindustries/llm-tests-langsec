#include <hammer/hammer.h>
#include <hammer/glue.h>

// ARP packet structure: Hardware type (2 bytes), Protocol type (2 bytes), Hardware size (1 byte),
// Protocol size (1 byte), Opcode (2 bytes), Sender MAC (6 bytes), Sender IP (4 bytes),
// Target MAC (6 bytes), Target IP (4 bytes)

// Define the basic parsers for ARP fields
static HParser *arp_hwtype;
static HParser *arp_prtype;
static HParser *arp_hwsize;
static HParser *arp_prsize;
static HParser *arp_opcode;
static HParser *arp_sender_mac;
static HParser *arp_sender_ip;
static HParser *arp_target_mac;
static HParser *arp_target_ip;

// ARP packet parser
static HParser *arp_packet;

void init_parsers() {
    arp_hwtype = h_uint16();
    arp_prtype = h_uint16();
    arp_hwsize = h_uint8();
    arp_prsize = h_uint8();
    arp_opcode = h_uint16();
    arp_sender_mac = h_repeat_n(h_uint8(), 6);
    arp_sender_ip = h_repeat_n(h_uint8(), 4);
    arp_target_mac = h_repeat_n(h_uint8(), 6);
    arp_target_ip = h_repeat_n(h_uint8(), 4);

    arp_packet = h_sequence(arp_hwtype, arp_prtype, arp_hwsize, arp_prsize, arp_opcode,
                            arp_sender_mac, arp_sender_ip, arp_target_mac, arp_target_ip, NULL);
}

int main(int argc, char **argv) {
    init_parsers();
    HParser *parser = arp_packet;

    // Example data for an ARP request
    uint8_t example_arp_request[] = {
        0x00, 0x01, // Hardware type: Ethernet (1)
        0x08, 0x00, // Protocol type: IPv4 (0x0800)
        0x06,       // Hardware size: 6
        0x04,       // Protocol size: 4
        0x00, 0x01, // Opcode: request (1)
        0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, // Sender MAC
        0xc0, 0xa8, 0x01, 0x01,             // Sender IP
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Target MAC
        0xc0, 0xa8, 0x01, 0x02              // Target IP
    };

    size_t len = sizeof(example_arp_request);
    HParseResult *result = h_parse(parser, example_arp_request, len);
    if (result) {
        printf("ARP packet parsed successfully.\n");
        h_pprint(stdout, result->ast, 0, 0);
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ARP packet.\n");
    }

    return 0;
}