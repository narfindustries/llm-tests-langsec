message Arp {
    uint16 hardware_type;
    uint16 protocol_type;
    uint8 hardware_address_length;
    uint8 protocol_address_length;
    uint16 opcode;
    bytes sender_hardware_address;
    bytes sender_protocol_address;
    bytes target_hardware_address;
    bytes target_protocol_address;
}
