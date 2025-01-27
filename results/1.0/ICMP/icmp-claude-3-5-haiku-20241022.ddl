module ICMP

enum ICMPType:
    EchoReply = 0
    DestinationUnreachable = 3
    SourceQuench = 4
    Redirect = 5
    Echo = 8
    RouterAdvertisement = 9
    RouterSolicitation = 10
    TimeExceeded = 11
    ParameterProblem = 12
    Timestamp = 13
    TimestampReply = 14
    InformationRequest = 15
    InformationReply = 16

enum ICMPCode:
    NetworkUnreachable = 0
    HostUnreachable = 1
    ProtocolUnreachable = 2
    PortUnreachable = 3
    FragmentationNeeded = 4
    SourceRouteFailed = 5

type ICMPHeader:
    type: ICMPType
    code: ICMPCode
    checksum: uint16
    rest_of_header: uint32

parser ICMPPacket:
    header: ICMPHeader
    payload: bytes(*)