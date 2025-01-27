#include <hammer/hammer.h>

HParser *create_arp_parser() {
    // Define hardware type parser (16 bits)
    HParser *hw_type = h_bits(16, false);

    // Define protocol type parser (16 bits)
    HParser *proto_type = h_bits(16, false);

    // Define hardware address length parser (8 bits)
    HParser *hw_addr_len = h_bits(8, false);

    // Define protocol address length parser (8 bits)
    HParser *proto_addr_len = h_bits(8, false);

    // Define operation parser (request/reply - 16 bits)
    HParser *operation = h_bits(16, false);

    // Define sender hardware address parser (6 bytes)
    HParser *sender_hw_addr = h_repeat_n(h_bits(8, false), 6);

    // Define sender protocol address parser (4 bytes)
    HParser *sender_proto_addr = h_repeat_n(h_bits(8, false), 4);

    // Define target hardware address parser (6 bytes)
    HParser *target_hw_addr = h_repeat_n(h_bits(8, false), 6);

    // Define target protocol address parser (4 bytes)
    HParser *target_proto_addr = h_repeat_n(h_bits(8, false), 4);

    // Define ARP packet format
    HParser *arp_packet = h_sequence(
        hw_type,
        proto_type,
        hw_addr_len,
        proto_addr_len,
        operation,
        sender_hw_addr,
        sender_proto_addr,
        target_hw_addr,
        target_proto_addr,
        NULL
    );

    return arp_packet;
}

int main(int argc, char **argv) {
  HParser *arp_parser = create_arp_parser();
  // Use the parser...
  // Free the parser
  h_parser_free(arp_parser);
}