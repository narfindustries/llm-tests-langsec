#include <hammer/hammer.h>
#include <stdio.h>

static const uint8_t mac_addr_bytes[] = {0x00, 0x0c, 0x29, 0xd3, 0x91, 0x42};

HParser* init_arp_parser(void) {
    HParser* mac_addr = h_repeat_n(h_uint8(), 6);
    HParser* ip_addr = h_repeat_n(h_uint8(), 4);
    
    HParser* hardware_type = h_uint16();
    HParser* protocol_type = h_uint16();
    HParser* hardware_size = h_uint8();
    HParser* protocol_size = h_uint8();
    HParser* opcode = h_uint16();
    
    HParser* arp = h_sequence(hardware_type,
                             protocol_type,
                             hardware_size,
                             protocol_size,
                             opcode,
                             mac_addr,     // Sender MAC
                             ip_addr,      // Sender IP
                             mac_addr,     // Target MAC
                             ip_addr,      // Target IP
                             NULL);
    
    return arp;
}

int main(int argc, char* argv[]) {
    HParser* parser = init_arp_parser();
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    uint8_t test_packet[] = {
        0x00, 0x01,                   // Hardware type (Ethernet)
        0x08, 0x00,                   // Protocol type (IPv4)
        0x06,                         // Hardware size
        0x04,                         // Protocol size
        0x00, 0x01,                   // Opcode (Request)
        0x00, 0x0c, 0x29, 0xd3, 0x91, 0x42,  // Sender MAC
        192, 168, 1, 1,              // Sender IP
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00,  // Target MAC
        192, 168, 1, 2               // Target IP
    };

    HParseResult* result = h_parse(parser, test_packet, sizeof(test_packet));
    if (result) {
        printf("Parsing successful\n");
        h_parse_result_free(result);
    } else {
        printf("Parsing failed\n");
    }
    
    return 0;
}