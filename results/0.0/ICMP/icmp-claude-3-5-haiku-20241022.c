#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>

// Define ICMP packet structure
typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint16_t identifier;
    uint16_t sequence_number;
} ICMPPacket;

// Parser for ICMP packet
static HParser* icmp_parser() {
    return h_sequence(
        h_uint8(),   // type
        h_uint8(),   // code
        h_uint16(),  // checksum
        h_uint16(), // identifier
        h_uint16()  // sequence number
    );
}

// Semantic action to validate ICMP packet
static bool validate_icmp_packet(const HParseResult* result, void* user_data) {
    if (!result || !result->ast) return false;

    HArrayList* list = result->ast;
    if (h_arraylist_length(list) != 5) return false;

    uint8_t type = *(uint8_t*)h_arraylist_get(list, 0);
    uint8_t code = *(uint8_t*)h_arraylist_get(list, 1);

    // Basic ICMP type and code validation
    return (type == 8 && code == 0);  // Echo request
}

int main() {
    // Initialize Hammer
    h_init();

    // Create ICMP parser
    HParser* parser = icmp_parser();

    // Add semantic validation
    HParser* validated_parser = h_semantic_action(parser, validate_icmp_packet, NULL, NULL);

    // Example ICMP packet data
    uint8_t packet_data[] = {
        8, 0,        // Type 8 (Echo Request), Code 0
        0xAB, 0xCD,  // Checksum
        0x12, 0x34,  // Identifier
        0x56, 0x78   // Sequence Number
    };

    // Parse packet
    HParseResult* result = h_parse(validated_parser, packet_data, sizeof(packet_data));

    // Check parsing result
    if (result && result->ast) {
        printf("Valid ICMP Packet Parsed\n");
    } else {
        printf("Invalid ICMP Packet\n");
    }

    // Cleanup
    h_parse_result_free(result);
    h_parser_free(parser);
    h_parser_free(validated_parser);

    return 0;
}