type ARP = {
    hardware_type: uint16;
    protocol_type: uint16;
    hardware_address_length: uint8;
    protocol_address_length: uint8;
    operation: uint16;
    sender_hardware_address: bytes(hardware_address_length);
    sender_protocol_address: bytes(protocol_address_length);
    target_hardware_address: bytes(hardware_address_length);
    target_protocol_address: bytes(protocol_address_length)
}

parser parse_arp = { input: stream, endian: big_endian ->
    let arp = ARP {
        hardware_type = input.read_u16();
        protocol_type = input.read_u16();
        hardware_address_length = input.read_u8();
        protocol_address_length = input.read_u8();
        operation = input.read_u16();
        sender_hardware_address = input.read_bytes(hardware_address_length);
        sender_protocol_address = input.read_bytes(protocol_address_length);
        target_hardware_address = input.read_bytes(hardware_address_length);
        target_protocol_address = input.read_bytes(protocol_address_length)
    };
    return arp;
}