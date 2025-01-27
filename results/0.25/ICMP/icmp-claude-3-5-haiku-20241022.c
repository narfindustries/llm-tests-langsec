#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// ICMP Packet Parser Specification
static HParser* icmp_packet_parser() {
    // ICMP Header Fields
    HParser* type = h_uint8();
    HParser* code = h_uint8();
    HParser* checksum = h_uint16();
    
    // ICMP Type-Specific Fields
    HParser* identifier = h_uint16();
    HParser* sequence_number = h_uint16();
    
    // Payload as variable-length byte array
    HParser* payload = h_many(h_uint8());
    
    // Complete ICMP Packet Structure
    return h_sequence(
        type,
        code, 
        checksum,
        identifier,
        sequence_number,
        payload,
        NULL
    );
}

int main() {
    // Initialize Hammer parser
    h_init();
    
    // Create parser
    HParser* parser = icmp_packet_parser();
    
    // Example ICMP packet data
    uint8_t sample_packet[] = {
        0x08, 0x00,   // Type (Echo Request), Code
        0x1234,       // Checksum 
        0x5678,       // Identifier
        0x9ABC,       // Sequence Number
        0x01, 0x02, 0x03, 0x04  // Payload
    };
    
    // Parse packet
    HParseResult* result = h_parse(parser, sample_packet, sizeof(sample_packet));
    
    // Cleanup
    h_parse_result_free(result);
    h_destroy(parser);
    
    return 0;
}