def Main = ICMPMessage

def ICMPMessage = {
    type: uint8
    code: uint8
    checksum: uint16
    rest_header: select(type) {
        case 0, 8, 13, 14, 15, 16: EchoRestHeader
        case 3, 4: UnusedRestHeader
        case 5: RedirectRestHeader
        case 12: ProblemRestHeader
        default: EmptyRestHeader
    }
    message_data: select(type) {
        case 3, 4, 5, 11, 12: ErrorMessageData
        case 13, 14: TimestampData
        case 0, 8: EchoData
        default: EmptyData
    }
}

def EchoRestHeader = {
    identifier: uint16
    sequence: uint16
}

def UnusedRestHeader = {
    unused: uint32
}

def RedirectRestHeader = {
    gateway_addr: uint32
}

def ProblemRestHeader = {
    pointer: uint8
    unused: bytes(3)
}

def EmptyRestHeader = {
}

def ErrorMessageData = {
    ip_header: bytes
    original_data: bytes(8)
}

def TimestampData = {
    orig_timestamp: uint32
    recv_timestamp: uint32
    trans_timestamp: uint32
}

def EchoData = {
    echo_data: bytes
}

def EmptyData = {
}

def ICMPTypes = {
    ECHO_REPLY = 0
    DEST_UNREACHABLE = 3
    SOURCE_QUENCH = 4
    REDIRECT = 5
    ECHO_REQUEST = 8
    TIME_EXCEEDED = 11
    PARAM_PROBLEM = 12
    TIMESTAMP = 13
    TIMESTAMP_REPLY = 14
    INFO_REQUEST = 15
    INFO_REPLY = 16
}

def DestUnreachableCodes = {
    NET_UNREACHABLE = 0
    HOST_UNREACHABLE = 1
    PROTOCOL_UNREACHABLE = 2
    PORT_UNREACHABLE = 3
    FRAG_NEEDED = 4
    SOURCE_ROUTE_FAILED = 5
}

def RedirectCodes = {
    NETWORK = 0
    HOST = 1
    TOS_NETWORK = 2
    TOS_HOST = 3
}

def TimeExceededCodes = {
    TTL_EXCEEDED = 0
    REASSEMBLY_EXCEEDED = 1
}