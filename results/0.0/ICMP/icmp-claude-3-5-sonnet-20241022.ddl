def Main = ICMP

def ICMP = {
    type: uint8;
    code: uint8;
    checksum: uint16;
    rest_of_header: switch type {
        case 0: EchoReply,
        case 3: DestUnreachable,
        case 4: SourceQuench,
        case 5: Redirect,
        case 8: EchoRequest,
        case 11: TimeExceeded,
        case 12: ParameterProblem,
        case 13: Timestamp,
        case 14: TimestampReply,
        case 15: InfoRequest,
        case 16: InfoReply,
        default: Unknown
    }
}

def EchoReply = {
    identifier: uint16;
    sequence_number: uint16;
    data: bytes
}

def DestUnreachable = {
    unused: uint32;
    ip_header: bytes(20);
    original_datagram: bytes(8)
}

def SourceQuench = {
    unused: uint32;
    ip_header: bytes(20);
    original_datagram: bytes(8)
}

def Redirect = {
    gateway_address: uint32;
    ip_header: bytes(20);
    original_datagram: bytes(8)
}

def EchoRequest = {
    identifier: uint16;
    sequence_number: uint16;
    data: bytes
}

def TimeExceeded = {
    unused: uint32;
    ip_header: bytes(20);
    original_datagram: bytes(8)
}

def ParameterProblem = {
    pointer: uint8;
    unused: bits(24);
    ip_header: bytes(20);
    original_datagram: bytes(8)
}

def Timestamp = {
    identifier: uint16;
    sequence_number: uint16;
    originate_timestamp: uint32;
    receive_timestamp: uint32;
    transmit_timestamp: uint32
}

def TimestampReply = {
    identifier: uint16;
    sequence_number: uint16;
    originate_timestamp: uint32;
    receive_timestamp: uint32;
    transmit_timestamp: uint32
}

def InfoRequest = {
    identifier: uint16;
    sequence_number: uint16
}

def InfoReply = {
    identifier: uint16;
    sequence_number: uint16
}

def Unknown = {
    data: bytes(4)
}