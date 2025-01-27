struct ICMPHeader {
    type: uint8;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
}

struct ICMPEchoRequest {
    header: ICMPHeader;
    identifier: uint16;
    sequence_number: uint16;
    data: bytes;
}

struct ICMPEchoReply {
    header: ICMPHeader;
    identifier: uint16;
    sequence_number: uint16;
    data: bytes;
}

struct ICMPPacket {
    header: ICMPHeader;
    switch (header.type) {
        case 8: echo_request: ICMPEchoRequest;
        case 0: echo_reply: ICMPEchoReply;
        default: unknown: bytes;
    }
}