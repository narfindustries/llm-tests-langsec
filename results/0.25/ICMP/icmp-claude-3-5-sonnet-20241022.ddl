def Main = ICMP

def ICMP = {
    type: uint8;
    code: uint8;
    checksum: uint16;
    rest_of_header: union {
        echo_reply: EchoReply if type == 0,
        dest_unreachable: DestUnreachable if type == 3,
        source_quench: SourceQuench if type == 4,
        redirect: Redirect if type == 5,
        echo_request: EchoRequest if type == 8,
        time_exceeded: TimeExceeded if type == 11,
        parameter_problem: ParameterProblem if type == 12,
        timestamp: Timestamp if type == 13,
        timestamp_reply: TimestampReply if type == 14,
        info_request: InfoRequest if type == 15,
        info_reply: InfoReply if type == 16
    };
    data: bytes
}

def EchoReply = {
    identifier: uint16;
    sequence_number: uint16
}

def DestUnreachable = {
    unused: uint32
}

def SourceQuench = {
    unused: uint32
}

def Redirect = {
    gateway_address: uint32
}

def EchoRequest = {
    identifier: uint16;
    sequence_number: uint16
}

def TimeExceeded = {
    unused: uint32
}

def ParameterProblem = {
    pointer: uint8;
    unused: uint24
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