protocol ICMP {
    type ICMPType = enum uint8 {
        EchoReply = 0,
        DestinationUnreachable = 3,
        SourceQuench = 4,
        Redirect = 5,
        EchoRequest = 8,
        RouterAdvertisement = 9,
        RouterSolicitation = 10,
        TimeExceeded = 11,
        ParameterProblem = 12,
        Timestamp = 13,
        TimestampReply = 14,
        InformationRequest = 15,
        InformationReply = 16
    };

    type ICMPCode = enum uint8 {
        Default = 0,
        NetUnreachable = 0,
        HostUnreachable = 1,
        ProtocolUnreachable = 2,
        PortUnreachable = 3,
        FragmentationNeeded = 4,
        SourceRouteFailed = 5
    };

    type ICMPHeader = struct {
        type: ICMPType,
        code: ICMPCode,
        checksum: uint16,
        payload: select(type) {
            EchoReply => EchoReplyPayload,
            EchoRequest => EchoRequestPayload,
            DestinationUnreachable => DestUnreachablePayload,
            TimeExceeded => TimeExceededPayload,
            Timestamp => TimestampPayload,
            TimestampReply => TimestampReplyPayload
        }
    };

    type EchoReplyPayload = struct {
        identifier: uint16,
        sequence_number: uint16,
        data: bytes
    };

    type EchoRequestPayload = struct {
        identifier: uint16,
        sequence_number: uint16,
        data: bytes
    };

    type DestUnreachablePayload = struct {
        unused: uint32,
        original_datagram: bytes
    };

    type TimeExceededPayload = struct {
        unused: uint32,
        original_datagram: bytes
    };

    type TimestampPayload = struct {
        identifier: uint16,
        sequence_number: uint16,
        originate_timestamp: uint32,
        receive_timestamp: uint32,
        transmit_timestamp: uint32
    };

    type TimestampReplyPayload = struct {
        identifier: uint16,
        sequence_number: uint16,
        originate_timestamp: uint32,
        receive_timestamp: uint32,
        transmit_timestamp: uint32
    };

    public function parse_icmp: ICMPHeader = {
        type: read ICMPType,
        code: read ICMPCode,
        checksum: read uint16,
        payload: select(type) {
            EchoReply => read EchoReplyPayload,
            EchoRequest => read EchoRequestPayload,
            DestinationUnreachable => read DestUnreachablePayload,
            TimeExceeded => read TimeExceededPayload,
            Timestamp => read TimestampPayload,
            TimestampReply => read TimestampReplyPayload,
            _ => read bytes
        }
    };
}