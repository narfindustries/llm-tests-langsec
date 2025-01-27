#include <hammer/hammer.h>
#include <hammer/glue.h>

// Define ARP opcode
static const HParser *arp_opcode() {
    return h_uint16();
}

// Define ARP hardware type
static const HParser *arp_hw_type() {
    return h_uint16();
}

// Define ARP protocol type
static const HParser *arp_proto_type() {
    return h_uint16();
}

// Define ARP hardware size
static const HParser *arp_hw_size() {
    return h_uint8();
}

// Define ARP protocol size
static const HParser *arp_proto_size() {
    return h_uint8();
}

// Define ARP sender hardware address
static const HParser *arp_sender_hw_addr(const HParser *hw_size) {
    return h_bits(h_uint_value(hw_size) * 8, false);
}

// Define ARP sender protocol address
static const HParser *arp_sender_proto_addr(const HParser *proto_size) {
    return h_bits(h_uint_value(proto_size) * 8, false);
}

// Define ARP target hardware address
static const HParser *arp_target_hw_addr(const HParser *hw_size) {
    return h_bits(h_uint_value(hw_size) * 8, false);
}

// Define ARP target protocol address
static const HParser *arp_target_proto_addr(const HParser *proto_size) {
    return h_bits(h_uint_value(proto_size) * 8, false);
}

// Define ARP packet
static const HParser *arp_packet() {
    const HParser *hw_type = arp_hw_type();
    const HParser *proto_type = arp_proto_type();
    const HParser *hw_size = arp_hw_size();
    const HParser *proto_size = arp_proto_size();
    return h_sequence(hw_type, proto_type, hw_size, proto_size,
                      arp_opcode(),
                      arp_sender_hw_addr(hw_size),
                      arp_sender_proto_addr(proto_size),
                      arp_target_hw_addr(hw_size),
                      arp_target_proto_addr(proto_size),
                      NULL);
}

int main(int argc, char *argv[]) {
    HParser *parser = arp_packet();
    HParseResult *result = h_parse(parser, (const uint8_t *)"\x00\x01\x08\x00\x06\x04\x00\x01\x01\x02\x03\x04\x05\x06\x0A\x00\x00\x01\x01\x02\x03\x04\x05\x06\x0A\x00\x00\x02", 28);
    if (result) {
        printf("ARP packet parsed successfully.\n");
    } else {
        printf("Failed to parse ARP packet.\n");
    }
    h_parse_result_free(result);
    h_parser_free(parser);
    return 0;
}