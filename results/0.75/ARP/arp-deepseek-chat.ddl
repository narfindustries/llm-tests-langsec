struct ARPHeader {
    hardware_type: u16;
    protocol_type: u16;
    hardware_size: u8;
    protocol_size: u8;
    opcode: u16;
    sender_mac: u8[6];
    sender_ip: u8[4];
    target_mac: u8[6];
    target_ip: u8[4];
}

struct ARPPacket {
    header: ARPHeader;
    payload: u8[];
}

instance ARPPacketParser: ARPPacket {
    let header = ARPHeader {
        hardware_type: be_u16(),
        protocol_type: be_u16(),
        hardware_size: u8(),
        protocol_size: u8(),
        opcode: be_u16(),
        sender_mac: u8[6](),
        sender_ip: u8[4](),
        target_mac: u8[6](),
        target_ip: u8[4](),
    };
    let payload = u8[..]();
}