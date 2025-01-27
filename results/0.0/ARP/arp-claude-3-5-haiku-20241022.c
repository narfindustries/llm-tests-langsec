#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Define the ARP packet structure
typedef struct {
    uint16_t hardware_type;
    uint16_t protocol_type;
    uint8_t hardware_size;
    uint8_t protocol_size;
    uint16_t opcode;
    uint8_t sender_mac[6];
    uint8_t sender_ip[4];
    uint8_t target_mac[6];
    uint8_t target_ip[4];
} ARPPacket;

// Hammer parser for ARP packet
static HParsedToken* parse_arp_packet(void* context, HParseResult* p) {
    const HArray* arr = p->ast;
    ARPPacket* packet = malloc(sizeof(ARPPacket));
    
    packet->hardware_type = *(uint16_t*)h_array_index(arr, 0);
    packet->protocol_type = *(uint16_t*)h_array_index(arr, 1);
    packet->hardware_size = *(uint8_t*)h_array_index(arr, 2);
    packet->protocol_size = *(uint8_t*)h_array_index(arr, 3);
    packet->opcode = *(uint16_t*)h_array_index(arr, 4);
    
    memcpy(packet->sender_mac, h_array_index(arr, 5), 6);
    memcpy(packet->sender_ip, h_array_index(arr, 6), 4);
    memcpy(packet->target_mac, h_array_index(arr, 7), 6);
    memcpy(packet->target_ip, h_array_index(arr, 8), 4);
    
    return h_make_seqn(packet, arr->used);
}

// Create ARP packet parser
HParser* create_arp_packet_parser() {
    // Define parsers for each field
    HParser* hardware_type = h_uint16();
    HParser* protocol_type = h_uint16();
    HParser* hardware_size = h_uint8();
    HParser* protocol_size = h_uint8();
    HParser* opcode = h_uint16();
    
    // MAC address parser (6 bytes)
    HParser* mac_address = h_repeat_n(h_uint8(), 6);
    
    // IP address parser (4 bytes)
    HParser* ip_address = h_repeat_n(h_uint8(), 4);
    
    // Combine all parsers into a sequence
    HParser* arp_parser = h_sequence(
        hardware_type,   // Hardware type
        protocol_type,   // Protocol type
        hardware_size,   // Hardware size
        protocol_size,   // Protocol size
        opcode,          // Operation code
        mac_address,     // Sender MAC address
        ip_address,      // Sender IP address
        mac_address,     // Target MAC address
        ip_address,      // Target IP address
        NULL
    );
    
    // Set the parser's action to create an ARP packet
    h_act(arp_parser, parse_arp_packet, NULL);
    
    return arp_parser;
}

int main() {
    // Initialize Hammer
    h_init();
    
    // Create ARP packet parser
    HParser* arp_parser = create_arp_packet_parser();
    
    // Example ARP packet data
    uint8_t arp_data[] = {
        0x00, 0x01,       // Hardware type (Ethernet)
        0x08, 0x00,       // Protocol type (IPv4)
        0x06,             // Hardware size (6 bytes)
        0x04,             // Protocol size (4 bytes)
        0x00, 0x01,       // Opcode (request)
        0x00, 0x11, 0x22, 0x33, 0x44, 0x55,  // Sender MAC
        0xC0, 0xA8, 0x01, 0x64,              // Sender IP (192.168.1.100)
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Target MAC (all zeros)
        0xC0, 0xA8, 0x01, 0x01               // Target IP (192.168.1.1)
    };
    
    // Parse the ARP packet
    HParseResult* result = h_parse(arp_parser, arp_data, sizeof(arp_data));
    
    if (result && result->ast) {
        ARPPacket* parsed_packet = (ARPPacket*)result->ast->elements[0].data;
        
        printf("ARP Packet Parsed Successfully:\n");
        printf("Hardware Type: 0x%04X\n", parsed_packet->hardware_type);
        printf("Protocol Type: 0x%04X\n", parsed_packet->protocol_type);
        printf("Hardware Size: %d\n", parsed_packet->hardware_size);
        printf("Protocol Size: %d\n", parsed_packet->protocol_size);
        printf("Opcode: 0x%04X\n", parsed_packet->opcode);
        
        // Free resources
        h_parse_result_free(result);
    } else {
        printf("Failed to parse ARP packet\n");
    }
    
    // Cleanup
    h_destroy(arp_parser);
    
    return 0;
}