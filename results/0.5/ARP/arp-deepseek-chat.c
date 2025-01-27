#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

// Define the ARP header structure
typedef struct {
    uint16_t hw_type;
    uint16_t proto_type;
    uint8_t hw_addr_len;
    uint8_t proto_addr_len;
    uint16_t opcode;
    uint8_t sender_hw_addr[6];
    uint8_t sender_proto_addr[4];
    uint8_t target_hw_addr[6];
    uint8_t target_proto_addr[4];
} arp_header_t;

// Define the Hammer parser for the ARP header
HParser *arp_parser() {
    return h_sequence(
        h_bits_be(16, NULL),  // hw_type
        h_bits_be(16, NULL),  // proto_type
        h_bits(8, NULL),      // hw_addr_len
        h_bits(8, NULL),      // proto_addr_len
        h_bits_be(16, NULL),  // opcode
        h_bits(48, NULL),     // sender_hw_addr
        h_bits(32, NULL),     // sender_proto_addr
        h_bits(48, NULL),     // target_hw_addr
        h_bits(32, NULL),     // target_proto_addr
        NULL
    );
}

// Function to parse ARP packet
int parse_arp_packet(const uint8_t *data, size_t len) {
    HParser *parser = arp_parser();
    HParseResult *result = h_parse(parser, data, len);

    if (!result) {
        fprintf(stderr, "Failed to parse ARP packet\n");
        return -1;
    }

    arp_header_t *arp_header = (arp_header_t *)result->ast;
    printf("ARP Header:\n");
    printf("  HW Type: %04x\n", arp_header->hw_type);
    printf("  Proto Type: %04x\n", arp_header->proto_type);
    printf("  HW Addr Len: %d\n", arp_header->hw_addr_len);
    printf("  Proto Addr Len: %d\n", arp_header->proto_addr_len);
    printf("  Opcode: %04x\n", arp_header->opcode);
    printf("  Sender HW Addr: %02x:%02x:%02x:%02x:%02x:%02x\n",
           arp_header->sender_hw_addr[0], arp_header->sender_hw_addr[1],
           arp_header->sender_hw_addr[2], arp_header->sender_hw_addr[3],
           arp_header->sender_hw_addr[4], arp_header->sender_hw_addr[5]);
    printf("  Sender Proto Addr: %d.%d.%d.%d\n",
           arp_header->sender_proto_addr[0], arp_header->sender_proto_addr[1],
           arp_header->sender_proto_addr[2], arp_header->sender_proto_addr[3]);
    printf("  Target HW Addr: %02x:%02x:%02x:%02x:%02x:%02x\n",
           arp_header->target_hw_addr[0], arp_header->target_hw_addr[1],
           arp_header->target_hw_addr[2], arp_header->target_hw_addr[3],
           arp_header->target_hw_addr[4], arp_header->target_hw_addr[5]);
    printf("  Target Proto Addr: %d.%d.%d.%d\n",
           arp_header->target_proto_addr[0], arp_header->target_proto_addr[1],
           arp_header->target_proto_addr[2], arp_header->target_proto_addr[3]);

    h_parse_result_free(result);
    return 0;
}

int main() {
    // Example ARP packet data
    uint8_t arp_packet[] = {
        0x00, 0x01, 0x08, 0x00, 0x06, 0x04, 0x00, 0x01,
        0x00, 0x02, 0x03, 0x04, 0x05, 0x06, 0xC0, 0xA8,
        0x01, 0x01, 0x00, 0x07, 0x08, 0x09, 0x0A, 0x0B,
        0x0C, 0xC0, 0xA8, 0x01, 0x02
    };

    if (parse_arp_packet(arp_packet, sizeof(arp_packet)) != 0) {
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}