module ICMP;

enum ICMPType {
    EchoReply = 0,
    DestinationUnreachable = 3,
    SourceQuench = 4,
    Redirect = 5,
    Echo = 8,
    RouterAdvertisement = 9,
    RouterSolicitation = 10,
    TimeExceeded = 11,
    ParameterProblem = 12,
    Timestamp = 13,
    TimestampReply = 14,
    InformationRequest = 15,
    InformationReply = 16
}

enum ICMPCode {
    Default = 0,
    NetworkUnreachable = 0,
    HostUnreachable = 1,
    ProtocolUnreachable = 2,
    PortUnreachable = 3,
    FragmentationNeeded = 4,
    SourceRouteFailed = 5
}

struct ICMPHeader {
    type: ICMPType,
    code: ICMPCode,
    checksum: uint16,
    identifier: uint16,
    sequence_number: uint16,
    payload: [uint8]
}

parse ICMPPacket = {
    header: ICMPHeader
}