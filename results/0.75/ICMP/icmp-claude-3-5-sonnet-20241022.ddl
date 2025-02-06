def ICMP = {
    type: uint8;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
    data: bytes
}

def ICMP_Type0 = {
    type: uint8 where type == 0;
    code: uint8;
    checksum: uint16;
    identifier: uint16;
    sequence_number: uint16;
    data: bytes
}

def ICMP_Type3 = {
    type: uint8 where type == 3;
    code: uint8;
    checksum: uint16;
    unused: uint32;
    ip_header: bytes(20);
    original_datagram: bytes(8)
}

def ICMP_Type4 = {
    type: uint8 where type == 4;
    code: uint8;
    checksum: uint16;
    unused: uint32;
    ip_header: bytes(20);
    original_datagram: bytes(8)
}

def ICMP_Type5 = {
    type: uint8 where type == 5;
    code: uint8;
    checksum: uint16;
    gateway_address: uint32;
    ip_header: bytes(20);
    original_datagram: bytes(8)
}

def ICMP_Type8 = {
    type: uint8 where type == 8;
    code: uint8;
    checksum: uint16;
    identifier: uint16;
    sequence_number: uint16;
    data: bytes
}

def ICMP_Type11 = {
    type: uint8 where type == 11;
    code: uint8;
    checksum: uint16;
    unused: uint32;
    ip_header: bytes(20);
    original_datagram: bytes(8)
}

def ICMP_Type12 = {
    type: uint8 where type == 12;
    code: uint8;
    checksum: uint16;
    pointer: uint8;
    unused: bytes(3);
    ip_header: bytes(20);
    original_datagram: bytes(8)
}

def ICMP_Type13 = {
    type: uint8 where type == 13;
    code: uint8;
    checksum: uint16;
    identifier: uint16;
    sequence_number: uint16;
    original_timestamp: uint32;
    receive_timestamp: uint32;
    transmit_timestamp: uint32
}

def ICMP_Type14 = {
    type: uint8 where type == 14;
    code: uint8;
    checksum: uint16;
    identifier: uint16;
    sequence_number: uint16;
    original_timestamp: uint32;
    receive_timestamp: uint32;
    transmit_timestamp: uint32
}

def ICMP_Type15 = {
    type: uint8 where type == 15;
    code: uint8;
    checksum: uint16;
    identifier: uint16;
    sequence_number: uint16
}

def ICMP_Type16 = {
    type: uint8 where type == 16;
    code: uint8;
    checksum: uint16;
    identifier: uint16;
    sequence_number: uint16
}