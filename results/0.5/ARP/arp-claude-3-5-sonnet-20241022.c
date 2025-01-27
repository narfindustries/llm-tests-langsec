#include <hammer/hammer.h>
#include <stdio.h>

static HParser *init_parser(void) {
    // Basic components
    HParser *hex_digit = h_ch_range(hexdigit);
    HParser *colon = h_ch(':');
    HParser *hex_octet = h_repeat_n(hex_digit, 2);
    
    // MAC address format: 6 octets separated by colons
    HParser *mac_addr = h_sequence(hex_octet, 
                                 h_sequence(colon, hex_octet),
                                 h_sequence(colon, hex_octet),
                                 h_sequence(colon, hex_octet),
                                 h_sequence(colon, hex_octet),
                                 h_sequence(colon, hex_octet),
                                 NULL);
    
    // Operation codes
    HParser *opcode = h_int_range(h_uint16(), 1, 2);
    
    // Protocol addresses
    HParser *ip_octet = h_uint8();
    HParser *dot = h_ch('.');
    HParser *ip_addr = h_sequence(ip_octet,
                                h_sequence(dot, ip_octet),
                                h_sequence(dot, ip_octet),
                                h_sequence(dot, ip_octet),
                                NULL);
    
    // Complete ARP packet
    return h_sequence(mac_addr,      // Sender MAC
                     mac_addr,       // Target MAC
                     opcode,         // Operation
                     ip_addr,        // Sender IP
                     ip_addr,        // Target IP
                     NULL);
}

int main(int argc, char *argv[]) {
    HParser *parser = init_parser();
    
    if (!parser) {
        fprintf(stderr, "Failed to initialize parser\n");
        return 1;
    }
    
    const uint8_t input[] = "00:11:22:33:44:55"
                           "66:77:88:99:aa:bb"
                           "0001"
                           "192.168.1.1"
                           "192.168.1.2";
    
    HParseResult *result = h_parse(parser, input, sizeof(input)-1);
    
    if (!result) {
        fprintf(stderr, "Parse failed\n");
        return 1;
    }
    
    h_parse_result_free(result);
    return 0;
}