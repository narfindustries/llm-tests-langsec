#include <hammer/hammer.h>
#include <hammer/glue.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

static HParser* icmp_parser;

typedef struct {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
    uint32_t rest_of_header;
} ICMPPacket;

static HParseResult* parse_icmp_packet(void* data) {
    HParseResult* result = h_parse(icmp_parser, data, sizeof(ICMPPacket));
    return result;
}

static HParsedToken* act_icmp_packet(const HParseResult* p, void* user_data) {
    HParsedToken* tok = h_make_token(TT_STRUCT, sizeof(ICMPPacket));
    ICMPPacket* pkt = (ICMPPacket*)tok->data;
    
    pkt->type = h_seq_get_uint8(p->ast, 0);
    pkt->code = h_seq_get_uint8(p->ast, 1);
    pkt->checksum = h_seq_get_uint16(p->ast, 2);
    pkt->rest_of_header = h_seq_get_uint32(p->ast, 3);
    
    return tok;
}

void setup_icmp_parser() {
    HParser* type = h_uint8();
    HParser* code = h_uint8();
    HParser* checksum = h_uint16();
    HParser* rest_of_header = h_uint32();
    
    icmp_parser = h_sequence(type, code, checksum, rest_of_header, NULL);
    h_act(icmp_parser, act_icmp_packet, NULL);
}

int main() {
    setup_icmp_parser();
    
    // Example ICMP Echo Request packet
    uint8_t sample_packet[] = {
        8,  // Type 8 (Echo Request)
        0,  // Code 0
        0x12, 0x34,  // Checksum 
        0xAA, 0xBB, 0xCC, 0xDD  // Rest of header
    };
    
    HParseResult* result = parse_icmp_packet(sample_packet);
    
    if (result && result->ast) {
        ICMPPacket* packet = (ICMPPacket*)result->ast->data;
        printf("ICMP Packet:\n");
        printf("Type: %d\n", packet->type);
        printf("Code: %d\n", packet->code);
        printf("Checksum: 0x%04X\n", packet->checksum);
        printf("Rest of Header: 0x%08X\n", packet->rest_of_header);
    } else {
        printf("Parsing failed\n");
    }
    
    return 0;
}