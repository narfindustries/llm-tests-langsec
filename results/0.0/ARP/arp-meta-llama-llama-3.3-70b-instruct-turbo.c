#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define the structure for an ARP packet
typedef struct {
    uint16_t hw_type;
    uint16_t proto_type;
    uint8_t hw_addr_len;
    uint8_t proto_addr_len;
    uint16_t op;
    uint8_t sender_hw_addr[6];
    uint8_t sender_proto_addr[4];
    uint8_t target_hw_addr[6];
    uint8_t target_proto_addr[4];
} arp_packet_t;

// Define the structure for an Ethernet frame
typedef struct {
    uint8_t dest_mac[6];
    uint8_t src_mac[6];
    uint16_t ethertype;
    arp_packet_t arp;
} ethernet_frame_t;

// Function to generate an ARP packet
void generate_arp_packet(arp_packet_t* arp, uint8_t* sender_hw_addr, uint8_t* sender_proto_addr, uint8_t* target_hw_addr, uint8_t* target_proto_addr) {
    arp->hw_type = 0x0001; // Ethernet
    arp->proto_type = 0x0800; // IPv4
    arp->hw_addr_len = 6; // MAC address length
    arp->proto_addr_len = 4; // IPv4 address length
    arp->op = 0x0001; // Request
    memcpy(arp->sender_hw_addr, sender_hw_addr, 6);
    memcpy(arp->sender_proto_addr, sender_proto_addr, 4);
    memcpy(arp->target_hw_addr, target_hw_addr, 6);
    memcpy(arp->target_proto_addr, target_proto_addr, 4);
}

// Function to generate an Ethernet frame
void generate_ethernet_frame(ethernet_frame_t* frame, uint8_t* dest_mac, uint8_t* src_mac, arp_packet_t* arp) {
    memcpy(frame->dest_mac, dest_mac, 6);
    memcpy(frame->src_mac, src_mac, 6);
    frame->ethertype = 0x0806; // ARP
    frame->arp = *arp;
}

int main() {
    // Define the MAC and IP addresses
    uint8_t sender_hw_addr[] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    uint8_t sender_proto_addr[] = {0x00, 0x00, 0x00, 0x00};
    uint8_t target_hw_addr[] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    uint8_t target_proto_addr[] = {0x00, 0x00, 0x00, 0x00};
    uint8_t dest_mac[] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    uint8_t src_mac[] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

    // Generate the ARP packet
    arp_packet_t arp;
    generate_arp_packet(&arp, sender_hw_addr, sender_proto_addr, target_hw_addr, target_proto_addr);

    // Generate the Ethernet frame
    ethernet_frame_t frame;
    generate_ethernet_frame(&frame, dest_mac, src_mac, &arp);

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
    printf("Ethertype: %04x\n", frame.ethertype);
    printf("ARP Packet:\n");
    printf("Hardware Type: %04x\n", frame.arp.hw_type);
    printf("Protocol Type: %04x\n", frame.arp.proto_type);
    printf("Hardware Address Length: %02x\n", frame.arp.hw_addr_len);
    printf("Protocol Address Length: %02x\n", frame.arp.proto_addr_len);
    printf("Operation: %04x\n", frame.arp.op);
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