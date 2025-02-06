def ARP = {
    hw_type: U16;
    proto_type: U16;
    hw_addr_len: U8;
    proto_addr_len: U8;
    op_code: U16;
    sender_hw_addr: BYTES(hw_addr_len);
    sender_proto_addr: BYTES(proto_addr_len);
    target_hw_addr: BYTES(hw_addr_len);
    target_proto_addr: BYTES(proto_addr_len);
}

def Main = ARP