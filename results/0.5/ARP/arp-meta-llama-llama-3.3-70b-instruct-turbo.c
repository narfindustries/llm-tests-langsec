#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// Define the ARP packet structure
typedef struct {
    uint16_t hw_type;
    uint16_t proto_type;
    uint8_t hw_addr_len;
    uint8_t proto_addr_len;
    uint16_t op_code;
    uint8_t sender_hw_addr[6];
    uint8_t sender_proto_addr[4];
    uint8_t target_hw_addr[6];
    uint8_t target_proto_addr[4];
} __attribute__((packed)) arp_packet_t;

// Define the Ethernet frame structure
typedef struct {
    uint8_t dest_mac[6];
    uint8_t src_mac[6];
    uint16_t eth_type;
    arp_packet_t arp;
} __attribute__((packed)) eth_frame_t;

int main() {
    // Create an example ARP packet
    arp_packet_t arp = {
        .hw_type = 0x0001, // Ethernet
        .proto_type = 0x0800, // IPv4
        .hw_addr_len = 6, // MAC address length
        .proto_addr_len = 4, // IPv4 address length
        .op_code = 0x0001, // Request
        .sender_hw_addr = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
        .sender_proto_addr = {0x00, 0x00, 0x00, 0x00},
        .target_hw_addr = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
        .target_proto_addr = {0x00, 0x00, 0x00, 0x00}
    };

    // Create an example Ethernet frame
    eth_frame_t frame = {
        .dest_mac = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
        .src_mac = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
        .eth_type = 0x0806, // ARP
        .arp = arp
    };

    // Print the Ethernet frame
    printf("Ethernet Frame:\n");
    printf("Destination MAC: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", frame.dest_mac[i]);
    }
    printf("\n");
    printf("Source MAC: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", frame.src_mac[i]);
    }
    printf("\n");
    printf("Ethernet Type: 0x%04x\n", frame.eth_type);

    // Print the ARP packet
    printf("ARP Packet:\n");
    printf("Hardware Type: 0x%04x\n", frame.arp.hw_type);
    printf("Protocol Type: 0x%04x\n", frame.arp.proto_type);
    printf("Hardware Address Length: %d\n", frame.arp.hw_addr_len);
    printf("Protocol Address Length: %d\n", frame.arp.proto_addr_len);
    printf("Op Code: 0x%04x\n", frame.arp.op_code);
    printf("Sender Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", frame.arp.sender_hw_addr[i]);
    }
    printf("\n");
    printf("Sender Protocol Address: ");
    for (int i = 0; i < 4; i++) {
        printf("%02x:", frame.arp.sender_proto_addr[i]);
    }
    printf("\n");
    printf("Target Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", frame.arp.target_hw_addr[i]);
    }
    printf("\n");
    printf("Target Protocol Address: ");
    for (int i = 0; i < 4; i++) {
        printf("%02x:", frame.arp.target_proto_addr[i]);
    }
    printf("\n");

    return 0;
}