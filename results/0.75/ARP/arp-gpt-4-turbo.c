#include <hammer/hammer.h>
#include <hammer/glue.h>

// Forward declarations
static HParsedToken *act_ethertype(const HParseResult *p, void *user_data);
static HParsedToken *act_hardware_type(const HParseResult *p, void *user_data);
static HParsedToken *act_protocol_type(const HParseResult *p, void *user_data);
static HParsedToken *act_hardware_length(const HParseResult *p, void *user_data);
static HParsedToken *act_protocol_length(const HParseResult *p, void *user_data);
static HParsedToken *act_opcode(const HParseResult *p, void *user_data);
static HParsedToken *act_hardware_address(const HParseResult *p, void *user_data);
static HParsedToken *act_protocol_address(const HParseResult *p, void *user_data);

// ARP Packet Parser
static HParser *arp_packet() {
    HParser *ethertype = h_action(h_uint16_be(), act_ethertype, NULL);
    HParser *hardware_type = h_action(h_uint16_be(), act_hardware_type, NULL);
    HParser *protocol_type = h_action(h_uint16_be(), act_protocol_type, NULL);
    HParser *hardware_length = h_action(h_uint8(), act_hardware_length, NULL);
    HParser *protocol_length = h_action(h_uint8(), act_protocol_length, NULL);
    HParser *opcode = h_action(h_uint16_be(), act_opcode, NULL);
    HParser *hardware_address = h_action(h_repeat_n(h_uint8(), 6), act_hardware_address, NULL);
    HParser *protocol_address = h_action(h_repeat_n(h_uint8(), 4), act_protocol_address, NULL);

    return h_sequence(ethertype, hardware_type, protocol_type, hardware_length, protocol_length, opcode,
                      hardware_address, hardware_address, // sender hardware address
                      protocol_address, protocol_address, // sender protocol address
                      hardware_address, protocol_address, // target hardware address and protocol address
                      NULL);
}

// Actions
static HParsedToken *act_ethertype(const HParseResult *p, void *user_data) {
    uint16_t ethertype = H_CAST_UINT(p->ast);
    return H_MAKE_UINT(ethertype);
}

static HParsedToken *act_hardware_type(const HParseResult *p, void *user_data) {
    uint16_t hardware_type = H_CAST_UINT(p->ast);
    return H_MAKE_UINT(hardware_type);
}

static HParsedToken *act_protocol_type(const HParseResult *p, void *user_data) {
    uint16_t protocol_type = H_CAST_UINT(p->ast);
    return H_MAKE_UINT(protocol_type);
}

static HParsedToken *act_hardware_length(const HParseResult *p, void *user_data) {
    uint8_t hardware_length = H_CAST_UINT8(p->ast);
    return H_MAKE_UINT(hardware_length);
}

static HParsedToken *act_protocol_length(const HParseResult *p, void *user_data) {
    uint8_t protocol_length = H_CAST_UINT8(p->ast);
    return H_MAKE_UINT(protocol_length);
}

static HParsedToken *act_opcode(const HParseResult *p, void *user_data) {
    uint16_t opcode = H_CAST_UINT(p->ast);
    return H_MAKE_UINT(opcode);
}

static HParsedToken *act_hardware_address(const HParseResult *p, void *user_data) {
    const uint8_t *hardware_address = H_CAST_SEQ_BYTES(p->ast);
    return h_make_bytes(hardware_address, 6);
}

static HParsedToken *act_protocol_address(const HParseResult *p, void *user_data) {
    const uint8_t *protocol_address = H_CAST_SEQ_BYTES(p->ast);
    return h_make_bytes(protocol_address, 4);
}

int main(int argc, char *argv[]) {
    HParser *parser = arp_packet();
    HParser *parser_ether_frame = h_sequence(h_ignore(12), parser, NULL); // Skip first 12 bytes (MAC addresses)

    const uint8_t test_data[] = {
        0x00, 0x01, 0x08, 0x00, 0x06, 0x04, 0x00, 0x01, // ARP request
        0xde, 0xad, 0xbe, 0xef, 0xde, 0xad, // sender MAC
        0xc0, 0xa8, 0x01, 0x01, // sender IP
        0xde, 0xad, 0xbe, 0xef, 0xde, 0xaf, // target MAC
        0xc0, 0xa8, 0x01, 0x02 // target IP
    };
    
    size_t len = sizeof(test_data);
    HParseResult *res = h_parse(parser_ether_frame, test_data, len);
    if (res) {
        printf("ARP packet parsed successfully.\n");
        h_pprint(stdout, res->ast, 0, 0);
        h_parse_result_free(res);
    } else {
        printf("Failed to parse ARP packet.\n");
    }

    h_parser_free(parser);
    h_parser_free(parser_ether_frame);
    return 0;
}