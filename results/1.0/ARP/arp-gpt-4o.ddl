define ARP {
    uint16 hardware_type;             // Hardware Type (HTYPE)
    uint16 protocol_type;             // Protocol Type (PTYPE)
    uint8 hardware_addr_length;       // Hardware Address Length (HLEN)
    uint8 protocol_addr_length;       // Protocol Address Length (PLEN)
    uint16 operation;                 // Operation (OPER)

    uint8 sender_hardware_addr[hardware_addr_length]; // Sender Hardware Address (SHA)
    uint8 sender_protocol_addr[protocol_addr_length]; // Sender Protocol Address (SPA)
    uint8 target_hardware_addr[hardware_addr_length]; // Target Hardware Address (THA)
    uint8 target_protocol_addr[protocol_addr_length]; // Target Protocol Address (TPA)
};