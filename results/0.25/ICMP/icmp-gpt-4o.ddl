module ICMP

type ICMPHeader = struct {
    type: uint8;
    code: uint8;
    checksum: uint16;
    rest_of_header: uint32;
}

type ICMPPacket = struct {
    header: ICMPHeader;
    data: bytes;
}

root = ICMPPacket