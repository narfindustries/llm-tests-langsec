def Main = ICMP

def ICMP = {
    type: uint8;
    code: uint8;
    checksum: uint16;
    rest_of_header: ICMPPayload(type);
    data: bytes
}

def ICMPPayload(msg_type: uint8) = {
    if msg_type == 0 || msg_type == 8 || msg_type == 15 || msg_type == 16 {
        identifier: uint16;
        sequence_number: uint16
    }
    if msg_type == 3 || msg_type == 4 || msg_type == 11 {
        unused: uint32;
        ip_header: bytes;
        original_datagram: bytes[8]
    }
    if msg_type == 5 {
        gateway_address: uint32;
        ip_header: bytes;
        original_datagram: bytes[8]
    }
    if msg_type == 12 {
        pointer: uint8;
        unused: bytes[3];
        ip_header: bytes;
        original_datagram: bytes[8]
    }
    if msg_type == 13 || msg_type == 14 {
        identifier: uint16;
        sequence_number: uint16;
        originate_timestamp: uint32;
        receive_timestamp: uint32;
        transmit_timestamp: uint32
    }
}