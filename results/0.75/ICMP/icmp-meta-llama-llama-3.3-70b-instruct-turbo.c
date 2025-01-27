#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

// Define the structure for ICMP packet
typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint32_t unreachable_ip;
} icmp_packet_t;

// Define the structure for IP packet
typedef struct {
    uint8_t version_and_header_length;
    uint8_t differentiated_services;
    uint16_t total_length;
    uint16_t identification;
    uint16_t flags_and_fragment_offset;
    uint8_t time_to_live;
    uint8_t protocol;
    uint16_t header_checksum;
    uint32_t source_ip;
    uint32_t destination_ip;
} ip_packet_t;

// Define the structure for Ethernet frame
typedef struct {
    uint8_t destination_mac[6];
    uint8_t source_mac[6];
    uint16_t ethertype;
    uint8_t payload[];
} ethernet_frame_t;

// Function to generate ICMP packet
icmp_packet_t* generate_icmp_packet(uint8_t type, uint8_t code, uint32_t unreachable_ip) {
    icmp_packet_t* icmp_packet = malloc(sizeof(icmp_packet_t));
    icmp_packet->type = type;
    icmp_packet->code = code;
    icmp_packet->checksum = 0;
    icmp_packet->unreachable_ip = unreachable_ip;
    return icmp_packet;
}

// Function to generate IP packet
ip_packet_t* generate_ip_packet(uint8_t version_and_header_length, uint8_t differentiated_services, uint16_t total_length, uint16_t identification, uint16_t flags_and_fragment_offset, uint8_t time_to_live, uint8_t protocol, uint32_t source_ip, uint32_t destination_ip) {
    ip_packet_t* ip_packet = malloc(sizeof(ip_packet_t));
    ip_packet->version_and_header_length = version_and_header_length;
    ip_packet->differentiated_services = differentiated_services;
    ip_packet->total_length = total_length;
    ip_packet->identification = identification;
    ip_packet->flags_and_fragment_offset = flags_and_fragment_offset;
    ip_packet->time_to_live = time_to_live;
    ip_packet->protocol = protocol;
    ip_packet->header_checksum = 0;
    ip_packet->source_ip = source_ip;
    ip_packet->destination_ip = destination_ip;
    return ip_packet;
}

// Function to generate Ethernet frame
ethernet_frame_t* generate_ethernet_frame(uint8_t* destination_mac, uint8_t* source_mac, uint16_t ethertype, uint8_t* payload, int payload_length) {
    ethernet_frame_t* ethernet_frame = malloc(sizeof(ethernet_frame_t) + payload_length);
    memcpy(ethernet_frame->destination_mac, destination_mac, 6);
    memcpy(ethernet_frame->source_mac, source_mac, 6);
    ethernet_frame->ethertype = ethertype;
    memcpy(ethernet_frame->payload, payload, payload_length);
    return ethernet_frame;
}

int main() {
    // Generate ICMP packet
    icmp_packet_t* icmp_packet = generate_icmp_packet(11, 0, 0x01010101);

    // Generate IP packet
    ip_packet_t* ip_packet = generate_ip_packet(0x45, 0, sizeof(ip_packet_t) + sizeof(icmp_packet_t), 0x1234, 0, 64, 1, 0x02020202, 0x03030303);

    // Generate Ethernet frame
    uint8_t destination_mac[] = {0x00, 0x00, 0x00, 0x00, 0x00, 0x00};
    uint8_t source_mac[] = {0x11, 0x11, 0x11, 0x11, 0x11, 0x11};
    uint8_t payload[sizeof(ip_packet_t) + sizeof(icmp_packet_t)];
    memcpy(payload, ip_packet, sizeof(ip_packet_t));
    memcpy(payload + sizeof(ip_packet_t), icmp_packet, sizeof(icmp_packet_t));
    ethernet_frame_t* ethernet_frame = generate_ethernet_frame(destination_mac, source_mac, 0x0800, payload, sizeof(payload));

    // Print the generated packets
    printf("ICMP Packet:\n");
    printf("Type: %d\n", icmp_packet->type);
    printf("Code: %d\n", icmp_packet->code);
    printf("Checksum: %d\n", icmp_packet->checksum);
    printf("Unreachable IP: %x\n", icmp_packet->unreachable_ip);

    printf("IP Packet:\n");
    printf("Version and Header Length: %d\n", ip_packet->version_and_header_length);
    printf("Differentiated Services: %d\n", ip_packet->differentiated_services);
    printf("Total Length: %d\n", ip_packet->total_length);
    printf("Identification: %d\n", ip_packet->identification);
    printf("Flags and Fragment Offset: %d\n", ip_packet->flags_and_fragment_offset);
    printf("Time to Live: %d\n", ip_packet->time_to_live);
    printf("Protocol: %d\n", ip_packet->protocol);
    printf("Header Checksum: %d\n", ip_packet->header_checksum);
    printf("Source IP: %x\n", ip_packet->source_ip);
    printf("Destination IP: %x\n", ip_packet->destination_ip);

    printf("Ethernet Frame:\n");
    printf("Destination MAC: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", ethernet_frame->destination_mac[i]);
    }
    printf("\n");
    printf("Source MAC: ");
    for (int i = 0; i < 6; i++) {
        printf("%02x:", ethernet_frame->source_mac[i]);
    }
    printf("\n");
    printf("Ethertype: %d\n", ethernet_frame->ethertype);

    // Free the allocated memory
    free(icmp_packet);
    free(ip_packet);
    free(ethernet_frame);

    return 0;
}