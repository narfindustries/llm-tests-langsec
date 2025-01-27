#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

// ICMP packet structure definition
typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint16_t identifier;
    uint16_t sequence_number;
} ICMPPacket;

// Hammer parser for ICMP packet
static HParser *icmp_parser = NULL;

// Create Hammer parser for ICMP packet
static HParser* make_icmp_parser() {
    // Define individual field parsers
    HParser *type_parser = h_uint8();
    HParser *code_parser = h_uint8();
    HParser *checksum_parser = h_uint16();
    HParser *identifier_parser = h_uint16();
    HParser *sequence_parser = h_uint16();

    // Combine parsers into a struct parser
    return h_struct(
        h_field("type", type_parser),
        h_field("code", code_parser),
        h_field("checksum", checksum_parser),
        h_field("identifier", identifier_parser),
        h_field("sequence_number", sequence_parser),
        NULL
    );
}

// Parse ICMP packet
static HParsedToken* parse_icmp_packet(void* data, size_t len) {
    if (!icmp_parser) {
        icmp_parser = make_icmp_parser();
    }
    return h_parse(icmp_parser, data, len);
}

// Validate ICMP packet
static int validate_icmp_packet(HParsedToken* parsed) {
    if (!parsed) return 0;

    // Add specific validation logic
    HHashTable* packet = (HHashTable*)parsed;
    uint8_t type = *(uint8_t*)h_hashtable_get(packet, "type");
    uint8_t code = *(uint8_t*)h_hashtable_get(packet, "code");

    // Example validation rules
    return (type == 8 && code == 0);  // Echo request
}

// Main parsing and validation function
int parse_and_validate_icmp(uint8_t* data, size_t length) {
    HParsedToken* parsed = parse_icmp_packet(data, length);
    return parsed ? validate_icmp_packet(parsed) : 0;
}

int main() {
    // Example ICMP Echo Request packet
    uint8_t sample_packet[] = {
        0x08, 0x00,   // Type (8), Code (0)
        0x12, 0x34,   // Checksum
        0x56, 0x78,   // Identifier
        0x90, 0xAB    // Sequence Number
    };

    int result = parse_and_validate_icmp(sample_packet, sizeof(sample_packet));
    printf("ICMP Packet Validation Result: %s\n", result ? "VALID" : "INVALID");

    return 0;
}