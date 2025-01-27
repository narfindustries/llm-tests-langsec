#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <stdlib.h>

// Define a structure to hold the parsed data
typedef struct {
    char* protocol;
    char* sender_mac;
    char* sender_ip;
    char* target_mac;
    char* target_ip;
} ARPPacket;

// Parser for MAC address format
static HParsedToken* parse_mac_address(void* p) {
    const HParseResult* result = h_parse(h_re("[0-9A-Fa-f]{2}(:[0-9A-Fa-f]{2}){5}"), p);
    if (result && result->ast) {
        char* mac = h_ast_to_string(result->ast);
        return h_make_str(mac);
    }
    return NULL;
}

// Parser for IP address format
static HParsedToken* parse_ip_address(void* p) {
    const HParseResult* result = h_parse(h_re("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"), p);
    if (result && result->ast) {
        char* ip = h_ast_to_string(result->ast);
        return h_make_str(ip);
    }
    return NULL;
}

// Main ARP packet parser
static HParsedToken* parse_arp_packet(void* p) {
    HParser* protocol = h_choice(h_string("Request"), h_string("Reply"), NULL);
    HParser* mac_address = h_action(h_re("[0-9A-Fa-f]{2}(:[0-9A-Fa-f]{2}){5}"), parse_mac_address, NULL);
    HParser* ip_address = h_action(h_re("\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"), parse_ip_address, NULL);

    HParser* arp_parser = h_sequence(
        protocol,
        h_whitespace(h_ignore(h_ch(' '))),
        mac_address,
        h_whitespace(h_ignore(h_ch(' '))),
        ip_address,
        h_whitespace(h_ignore(h_ch(' '))),
        mac_address,
        h_whitespace(h_ignore(h_ch(' '))),
        ip_address,
        NULL
    );

    const HParseResult* result = h_parse(arp_parser, p);
    if (result && result->ast) {
        ARPPacket* packet = malloc(sizeof(ARPPacket));
        packet->protocol = h_ast_to_string(h_idx(result->ast, 0));
        packet->sender_mac = h_ast_to_string(h_idx(result->ast, 1));
        packet->sender_ip = h_ast_to_string(h_idx(result->ast, 2));
        packet->target_mac = h_ast_to_string(h_idx(result->ast, 3));
        packet->target_ip = h_ast_to_string(h_idx(result->ast, 4));
        return h_make_usr(packet);
    }
    return NULL;
}

int main() {
    // Initialize Hammer
    h_init();

    // Sample ARP packet string
    const char* arp_packet_str = "Request 00:11:22:33:44:55 192.168.1.100 00:AA:BB:CC:DD:EE 192.168.1.1";

    // Create the ARP parser
    HParser* arp_parser = h_action(
        h_re("(Request|Reply) [0-9A-Fa-f]{2}(:[0-9A-Fa-f]{2}){5} \\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3} [0-9A-Fa-f]{2}(:[0-9A-Fa-f]{2}){5} \\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}"),
        parse_arp_packet,
        NULL
    );

    // Parse the ARP packet
    const HParseResult* result = h_parse(arp_parser, arp_packet_str);

    // Process and print the result
    if (result && result->ast) {
        ARPPacket* packet = h_ast_to_usr(result->ast);
        printf("Protocol: %s\n", packet->protocol);
        printf("Sender MAC: %s\n", packet->sender_mac);
        printf("Sender IP: %s\n", packet->sender_ip);
        printf("Target MAC: %s\n", packet->target_mac);
        printf("Target IP: %s\n", packet->target_ip);

        // Free allocated memory
        free(packet->protocol);
        free(packet->sender_mac);
        free(packet->sender_ip);
        free(packet->target_mac);
        free(packet->target_ip);
        free(packet);
    } else {
        printf("Parsing failed\n");
    }

    return 0;
}