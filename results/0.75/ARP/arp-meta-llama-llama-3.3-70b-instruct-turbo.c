#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

// Define structs
typedef struct {
    uint16_t hw_type;
    uint16_t proto_type;
    uint8_t hw_addr_len;
    uint8_t proto_addr_len;
    uint16_t op;
    uint8_t src_hw_addr[6];
    uint8_t src_ip_addr[4];
    uint8_t dst_hw_addr[6];
    uint8_t dst_ip_addr[4];
} arp_packet_t;

// Function to parse ARP packet
arp_packet_t parse_arp(uint8_t* data, int length) {
    arp_packet_t packet;
    // Check if length is sufficient to parse ARP packet
    if (length < 28) {
        printf("Error: Invalid ARP packet length\n");
        return packet;
    }

    // Parse ARP packet fields
    packet.hw_type = (uint16_t)data[0] << 8 | (uint16_t)data[1];
    packet.proto_type = (uint16_t)data[2] << 8 | (uint16_t)data[3];
    packet.hw_addr_len = data[4];
    packet.proto_addr_len = data[5];
    packet.op = (uint16_t)data[6] << 8 | (uint16_t)data[7];

    // Copy source and destination hardware addresses
    for (int i = 0; i < 6; i++) {
        packet.src_hw_addr[i] = data[8 + i];
        packet.dst_hw_addr[i] = data[18 + i];
    }

    // Copy source and destination IP addresses
    for (int i = 0; i < 4; i++) {
        packet.src_ip_addr[i] = data[14 + i];
        packet.dst_ip_addr[i] = data[24 + i];
    }

    return packet;
}

// Function to generate ARP packet
uint8_t* generate_arp(arp_packet_t packet, int* length) {
    // Calculate length of ARP packet
    *length = 28;

    // Allocate memory for ARP packet
    uint8_t* data = (uint8_t*)malloc(*length);

    // Generate ARP packet fields
    data[0] = (uint8_t)(packet.hw_type >> 8);
    data[1] = (uint8_t)(packet.hw_type & 0xFF);
    data[2] = (uint8_t)(packet.proto_type >> 8);
    data[3] = (uint8_t)(packet.proto_type & 0xFF);
    data[4] = packet.hw_addr_len;
    data[5] = packet.proto_addr_len;
    data[6] = (uint8_t)(packet.op >> 8);
    data[7] = (uint8_t)(packet.op & 0xFF);

    // Copy source and destination hardware addresses
    for (int i = 0; i < 6; i++) {
        data[8 + i] = packet.src_hw_addr[i];
        data[18 + i] = packet.dst_hw_addr[i];
    }

    // Copy source and destination IP addresses
    for (int i = 0; i < 4; i++) {
        data[14 + i] = packet.src_ip_addr[i];
        data[24 + i] = packet.dst_ip_addr[i];
    }

    return data;
}

int main() {
    arp_packet_t packet;
    packet.hw_type = 1;
    packet.proto_type = 0x0800;
    packet.hw_addr_len = 6;
    packet.proto_addr_len = 4;
    packet.op = 1;

    for (int i = 0; i < 6; i++) {
        packet.src_hw_addr[i] = (uint8_t)(i + 1);
        packet.dst_hw_addr[i] = (uint8_t)(i + 10);
    }

    for (int i = 0; i < 4; i++) {
        packet.src_ip_addr[i] = (uint8_t)(i + 100);
        packet.dst_ip_addr[i] = (uint8_t)(i + 200);
    }

    int length;
    uint8_t* data = generate_arp(packet, &length);

    // Use the generated ARP packet
    arp_packet_t parsed_packet = parse_arp(data, length);

    printf("Parsed ARP packet:\n");
    printf("  HW Type: %u\n", parsed_packet.hw_type);
    printf("  Proto Type: %u\n", parsed_packet.proto_type);
    printf("  HW Addr Len: %u\n", parsed_packet.hw_addr_len);
    printf("  Proto Addr Len: %u\n", parsed_packet.proto_addr_len);
    printf("  Operation: %u\n", parsed_packet.op);

    for (int i = 0; i < 6; i++) {
        printf("  Src HW Addr[%d]: %u\n", i, parsed_packet.src_hw_addr[i]);
        printf("  Dst HW Addr[%d]: %u\n", i, parsed_packet.dst_hw_addr[i]);
    }

    for (int i = 0; i < 4; i++) {
        printf("  Src IP Addr[%d]: %u\n", i, parsed_packet.src_ip_addr[i]);
        printf("  Dst IP Addr[%d]: %u\n", i, parsed_packet.dst_ip_addr[i]);
    }

    free(data);
    return 0;
}