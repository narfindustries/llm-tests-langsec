#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_icmp_parser(void) {
    // ICMP header fields
    HParser* type = h_uint8();
    HParser* code = h_uint8();
    HParser* checksum = h_uint16();
    
    // Rest of header fields
    HParser* identifier = h_uint16();
    HParser* sequence = h_uint16();
    
    // Optional payload
    HParser* payload = h_many(h_uint8());
    
    // Combine into full ICMP packet structure
    return h_sequence(type, code, checksum, identifier, sequence, payload, NULL);
}

static HParsedToken* act_icmp(const HParseResult* p) {
    return (HParsedToken*)p->ast;
}

H_RULE(icmp_packet, {
    return h_action(init_icmp_parser(), act_icmp);
});

#ifdef INCLUDE_MAIN
int main(int argc, char** argv) {
    uint8_t input[] = {
        0x08, 0x00,             // Type, Code
        0x4d, 0x2c,             // Checksum
        0x00, 0x01,             // Identifier
        0x00, 0x01,             // Sequence Number
        0x61, 0x62, 0x63, 0x64  // Payload "abcd"
    };

    const HParser* parser = icmp_packet();
    if (!parser) {
        fprintf(stderr, "Failed to create parser\n");
        return 1;
    }

    HParseResult* result = h_parse(parser, input, sizeof(input));
    if (!result) {
        fprintf(stderr, "Parse failed\n");
        return 1;
    }

    h_parse_result_free(result);
    return 0;
}
#endif