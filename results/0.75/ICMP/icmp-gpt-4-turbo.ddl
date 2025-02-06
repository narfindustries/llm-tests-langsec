type ICMP struct {
    Type        uint8
    Code        uint8
    Checksum    uint16
    RestOfHeader bytes (switch Type {
        case 0, 8          => size ICMPEcho
        case 3             => size ICMPUnreachable
        case 11            => size ICMPTimeExceeded
        case 5             => size ICMPRedirect
        case 12            => size ICMPParameterProblem
        case 13, 14        => size ICMPTimestamp
        default            => 0
    })
}

type ICMPEcho struct {
    Identifier  uint16
    SequenceNum uint16
}

type ICMPUnreachable struct {
    Unused      uint32
    IPHeader    IP
    First8Bytes bytes : 8
}

type ICMPTimeExceeded struct {
    Unused      uint32
    IPHeader    IP
    First8Bytes bytes : 8
}

type ICMPRedirect struct {
    GatewayInternetAddress uint32
    IPHeader               IP
    First8Bytes            bytes : 8
}

type ICMPParameterProblem struct {
    Pointer     uint8
    Unused      bytes : 3
    IPHeader    IP
    First8Bytes bytes : 8
}

type ICMPTimestamp struct {
    Identifier          uint16
    SequenceNum         uint16
    OriginateTimestamp  uint32
    ReceiveTimestamp    uint32
    TransmitTimestamp   uint32
}

type IP struct {
    VersionIHL        uint8
    TypeOfService     uint8
    TotalLength       uint16
    Identification    uint16
    FlagsFragment     uint16
    TimeToLive        uint8
    Protocol          uint8
    HeaderChecksum    uint16
    SourceIP          uint32
    DestinationIP     uint32
    Options           bytes ((VersionIHL & 0x0F) - 5) * 4
}