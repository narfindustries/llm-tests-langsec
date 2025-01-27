#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

static HParser* arp_parser;

static HParseResult* parse_arp_header(void* data) {
    const uint8_t* input = (const uint8_t*)data;
    
    // ARP Header Fields
    uint16_t hardware_type = (input[0] << 8) | input[1];
    uint16_t protocol_type = (input[2] << 8) | input[3];
    uint8_t hardware_size = input[4];
    uint8_t protocol_size = input[5];
    uint16_t opcode = (input[6] << 8) | input[7];

    // Create parse result with extracted fields
    HParseResult* result = h_make_parse_result(NULL, NULL);
    result->ast = h_wraplist(
        h_uint16(hardware_type),
        h_uint16(protocol_type),
        h_uint8(hardware_size),
        h_uint8(protocol_size),
        h_uint16(opcode)
    );

    return result;
}

HParser* build_arp_parser() {
    // Define ARP header parser
    arp_parser = h_sequence(
        h_uint16(0),     // Hardware Type (Ethernet = 1)
        h_uint16(0x0800),// Protocol Type (IPv4 = 0x0800)
        h_uint8(6),      // Hardware Size (MAC = 6 bytes)
        h_uint8(4),      // Protocol Size (IPv4 = 4 bytes)
        h_uint16(0),     // Opcode (Request/Reply)
        NULL
    );

    return arp_parser;
}

int main() {
    // Initialize Hammer
    h_init();

    // Create ARP parser
    HParser* parser = build_arp_parser();

    return 0;
}