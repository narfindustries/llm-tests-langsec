def ARP = {
    hardware_type: U16BE;
    protocol_type: U16BE;
    hardware_addr_len: U8;
    protocol_addr_len: U8;
    operation: U16BE;
    sender_hardware_addr: BYTES(hardware_addr_len);
    sender_protocol_addr: BYTES(protocol_addr_len);
    target_hardware_addr: BYTES(hardware_addr_len);
    target_protocol_addr: BYTES(protocol_addr_len);
}

def Main = ARP