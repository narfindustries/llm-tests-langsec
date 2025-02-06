def Main = {
    hw_type: u16;
    proto_type: u16;
    hw_addr_len: u8;
    proto_addr_len: u8;
    op_code: u16;
    sender_hw_addr: bytes(hw_addr_len);
    sender_proto_addr: bytes(proto_addr_len);
    target_hw_addr: bytes(hw_addr_len);
    target_proto_addr: bytes(proto_addr_len);
}

def HARDWARE_TYPE = {
    ETHERNET = 1;
    IEEE_802 = 6;
    ARCNET = 7;
    FRAME_RELAY = 15;
    ATM = 16;
    HDLC = 18;
    FIBRE_CHANNEL = 19;
}

def PROTOCOL_TYPE = {
    IPV4 = 0x0800;
}

def OPERATION = {
    REQUEST = 1;
    REPLY = 2;
}