type ARP = {
    hardware_type: be_uint16,
    protocol_type: be_uint16,
    hardware_addr_len: uint8,
    protocol_addr_len: uint8,
    operation: be_uint16,
    sender_hardware_addr: bytes(hardware_addr_len),
    sender_protocol_addr: bytes(protocol_addr_len),
    target_hardware_addr: bytes(hardware_addr_len),
    target_protocol_addr: bytes(protocol_addr_len)
}