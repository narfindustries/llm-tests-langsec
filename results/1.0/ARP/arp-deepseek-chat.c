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
        h_bits(16, &arp_header_t, hw_type),          // Hardware type
        h_bits(16, &arp_header_t, proto_type),       // Protocol type
        h_bits(8, &arp_header_t, hw_addr_len),       // Hardware address length
        h_bits(8, &arp_header_t, proto_addr_len),    // Protocol address length
        h_bits(16, &arp_header_t, opcode),           // Operation code
        h_bits(48, &arp_header_t, sender_hw_addr),   // Sender hardware address
        h_bits(32, &arp_header_t, sender_proto_addr),// Sender protocol address
        h_bits(48, &arp_header_t, target_hw_addr),   // Target hardware address
        h_bits(32, &arp_header_t, target_proto_addr) // Target protocol address
    );
}

int main() {
    // Example ARP packet data
    uint8_t arp_packet[] = {
        0x00, 0x01, // Hardware type (Ethernet)
        0x08, 0x00, // Protocol type (IPv4)
        0x06,       // Hardware address length (6 bytes)
        0x04,       // Protocol address length (4 bytes)
        0x00, 0x01, // Operation code (Request)
        0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, // Sender hardware address
        0xC0, 0xA8, 0x01, 0x01,             // Sender protocol address
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Target hardware address
        0xC0, 0xA8, 0x01, 0x02              // Target protocol address
    };

    // Parse the ARP packet
    HParseResult *result = h_parse(arp_parser(), arp_packet, sizeof(arp_packet));
    if (result) {
        arp_header_t *arp_header = (arp_header_t *)result->ast;
        printf("ARP Header Parsed Successfully:\n");
        printf("Hardware Type: 0x%04X\n", arp_header->hw_type);
        printf("Protocol Type: 0x%04X\n", arp_header->proto_type);
        printf("Hardware Address Length: %d\n", arp_header->hw_addr_len);
        printf("Protocol Address Length: %d\n", arp_header->proto_addr_len);
        printf("Operation Code: 0x%04X\n", arp_header->opcode);
        printf("Sender Hardware Address: %02X:%02X:%02X:%02X:%02X:%02X\n",
               arp_header->sender_hw_addr[0], arp_header->sender_hw_addr[1],
               arp_header->sender_hw_addr[2], arp_header->sender_hw_addr[3],
               arp_header->sender_hw_addr[4], arp_header->sender_hw_addr[5]);
        printf("Sender Protocol Address: %d.%d.%d.%d\n",
               arp_header->sender_proto_addr[0], arp_header->sender_proto_addr[1],
               arp_header->sender_proto_addr[2], arp_header->sender_proto_addr[3]);
        printf("Target Hardware Address: %02X:%02X:%02X:%02X:%02X:%02X\n",
               arp_header->target_hw_addr[0], arp_header->target_hw_addr[1],
               arp_header->target_hw_addr[2], arp_header->target_hw_addr[3],
               arp_header->target_hw_addr[4], arp_header->target_hw_addr[5]);
        printf("Target Protocol Address: %d.%d.%d.%d\n",
               arp_header->target_proto_addr[0], arp_header->target_proto_addr[1],
               arp_header->target_proto_addr[2], arp_header->target_proto_addr[3]);
        h_parse_result_free(result);
    } else {
        fprintf(stderr, "Failed to parse ARP packet.\n");
    }

    return 0;
}