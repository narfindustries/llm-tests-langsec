#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Define the structure for ARP packet
typedef struct {
    uint16_t hardware_type;
    uint16_t protocol_type;
    uint8_t hardware_address_length;
    uint8_t protocol_address_length;
    uint16_t operation;
    uint8_t sender_hardware_address[6];
    uint8_t sender_protocol_address[4];
    uint8_t target_hardware_address[6];
    uint8_t target_protocol_address[4];
} arp_packet_t;

// Define the structure for Ethernet frame
typedef struct {
    uint8_t destination_mac[6];
    uint8_t source_mac[6];
    uint16_t ethertype;
    arp_packet_t arp;
} ethernet_frame_t;

int main() {
    // Create an ARP packet
    arp_packet_t arp;
    arp.hardware_type = 0x0001;
    arp.protocol_type = 0x0800;
    arp.hardware_address_length = 6;
    arp.protocol_address_length = 4;
    arp.operation = 0x0001;
    uint8_t sender_mac[6] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    uint8_t sender_ip[4] = {0x0a, 0x00, 0x00, 0x01};
    uint8_t target_mac[6] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    uint8_t target_ip[4] = {0x0a, 0x00, 0x00, 0x02};
    memcpy(arp.sender_hardware_address, sender_mac, 6);
    memcpy(arp.sender_protocol_address, sender_ip, 4);
    memcpy(arp.target_hardware_address, target_mac, 6);
    memcpy(arp.target_protocol_address, target_ip, 4);

    // Create an Ethernet frame
    ethernet_frame_t frame;
    uint8_t dest_mac[6] = {0xff, 0xff, 0xff, 0xff, 0xff, 0xff};
    uint8_t src_mac[6] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    memcpy(frame.destination_mac, dest_mac, 6);
    memcpy(frame.source_mac, src_mac, 6);
    frame.ethertype = 0x0806;
    frame.arp = arp;

    // Print the Ethernet frame
    printf("Ethernet Frame:\n");
    printf("Destination MAC: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", frame.destination_mac[i]);
    }
    printf("\n");
    printf("Source MAC: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", frame.source_mac[i]);
    }
    printf("\n");
    printf("EtherType: %04x\n", frame.ethertype);
    printf("ARP Packet:\n");
    printf("Hardware Type: %04x\n", frame.arp.hardware_type);
    printf("Protocol Type: %04x\n", frame.arp.protocol_type);
    printf("Hardware Address Length: %02x\n", frame.arp.hardware_address_length);
    printf("Protocol Address Length: %02x\n", frame.arp.protocol_address_length);
    printf("Operation: %04x\n", frame.arp.operation);
    printf("Sender Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", frame.arp.sender_hardware_address[i]);
    }
    printf("\n");
    printf("Sender Protocol Address: ");
    for (int i = 0; i < 4; i++) {
        printf("%02x:", frame.arp.sender_protocol_address[i]);
    }
    printf("\n");
    printf("Target Hardware Address: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", frame.arp.target_hardware_address[i]);
    }
    printf("\n");
    printf("Target Protocol Address: ");
    for (int i = 0; i < 4; i++) {
        printf("%02x:", frame.arp.target_protocol_address[i]);
    }
    printf("\n");

    return 0;
}