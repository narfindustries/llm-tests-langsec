#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Define ARP packet structure
typedef struct {
    uint16_t hardware_type;
    uint16_t protocol_type;
    uint8_t hardware_addr_len;
    uint8_t protocol_addr_len;
    uint16_t operation;
    uint8_t sender_mac[6];
    uint8_t sender_ip[4];
    uint8_t target_mac[6];
    uint8_t target_ip[4];
} ARPPacket;

// Hammer parser for ARP packet
static HParsedToken* parse_arp_packet(void* context, HParseResult* p) {
    ARPPacket* packet = malloc(sizeof(ARPPacket));
    
    packet->hardware_type = *(uint16_t*)h_seq_get_token_index(p->ast, 0);
    packet->protocol_type = *(uint16_t*)h_seq_get_token_index(p->ast, 1);
    packet->hardware_addr_len = *(uint8_t*)h_seq_get_token_index(p->ast, 2);
    packet->protocol_addr_len = *(uint8_t*)h_seq_get_token_index(p->ast, 3);
    packet->operation = *(uint16_t*)h_seq_get_token_index(p->ast, 4);
    
    memcpy(packet->sender_mac, h_seq_get_token_index(p->ast, 5), 6);
    memcpy(packet->sender_ip, h_seq_get_token_index(p->ast, 6), 4);
    memcpy(packet->target_mac, h_seq_get_token_index(p->ast, 7), 6);
    memcpy(packet->target_ip, h_seq_get_token_index(p->ast, 8), 4);
    
    return h_make_seqn(packet, 9);
}

// Create ARP packet parser
HParser* create_arp_parser() {
    // Define individual field parsers
    HParser* hardware_type = h_uint16();
    HParser* protocol_type = h_uint16();
    HParser* hardware_addr_len = h_uint8();
    HParser* protocol_addr_len = h_uint8();
    HParser* operation = h_uint16();
    
    // MAC address parser (6 bytes)
    HParser* mac_addr = h_repeat_n(h_uint8(), 6);
    
    // IP address parser (4 bytes)
    HParser* ip_addr = h_repeat_n(h_uint8(), 4);
    
    // Combine parsers into full ARP packet parser
    HParser* arp_parser = h_sequence(
        hardware_type,
        protocol_type,
        hardware_addr_len,
        protocol_addr_len,
        operation,
        mac_addr,   // sender MAC
        ip_addr,    // sender IP
        mac_addr,   // target MAC
        ip_addr,    // target IP
        NULL
    );
    
    // Set up semantic action
    return h_action(arp_parser, parse_arp_packet, NULL);
}

int main() {
    // Initialize Hammer
    h_init();
    
    // Create ARP parser
    HParser* arp_parser = create_arp_parser();
    
    // Example ARP packet data
    uint8_t sample_packet[] = {
        0x00, 0x01,       // Hardware type (Ethernet)
        0x08, 0x00,       // Protocol type (IPv4)
        0x06,             // Hardware address length
        0x04,             // Protocol address length
        0x00, 0x01,       // Operation (request)
        0x00, 0x11, 0x22, 0x33, 0x44, 0x55,  // Sender MAC
        0xC0, 0xA8, 0x01, 0x64,              // Sender IP (192.168.1.100)
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Target MAC (all zeros)
        0xC0, 0xA8, 0x01, 0x01               // Target IP (192.168.1.1)
    };
    
    // Parse the packet
    HParseResult* result = h_parse(arp_parser, sample_packet, sizeof(sample_packet));
    
    if (result && result->ast) {
        ARPPacket* parsed_packet = (ARPPacket*)h_seq_get_token_index(result->ast, 0);
        
        printf("ARP Packet Parsed Successfully:\n");
        printf("Hardware Type: 0x%04X\n", parsed_packet->hardware_type);
        printf("Protocol Type: 0x%04X\n", parsed_packet->protocol_type);
        printf("Operation: %d\n", parsed_packet->operation);
    } else {
        printf("Parsing failed\n");
    }
    
    // Cleanup
    h_parse_result_free(result);
    h_destroy(arp_parser);
    
    return 0;
}