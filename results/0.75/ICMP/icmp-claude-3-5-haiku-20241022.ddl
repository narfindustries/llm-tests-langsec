typedef uint8 IpProtocol;

// ICMP Protocol Constants
const IpProtocol ICMP = 1;

// ICMP Message Types
enum IcmpType : uint8 {
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

// ICMP Header Structure
struct IcmpHeader {
    IcmpType type;
    uint8 code;
    uint16 checksum;
}

// ICMP Echo Request/Reply Payload
struct IcmpEchoPayload {
    uint16 identifier;
    uint16 sequenceNumber;
    [remaining] uint8[] data;
}

// Complete ICMP Packet
struct IcmpPacket {
    IcmpHeader header;
    variant Payload {
        EchoRequest: IcmpEchoPayload when header.type == IcmpType.Echo,
        EchoReply: IcmpEchoPayload when header.type == IcmpType.EchoReply,
        Default: [remaining] uint8[]
    } payload;
}

// ICMP Packet Parser
let parse_icmp = (input: [uint8]) -> IcmpPacket {
    let header = parse IcmpHeader from input;
    let payload = parse Payload from input[sizeof(IcmpHeader)..];
    IcmpPacket { header, payload }
}