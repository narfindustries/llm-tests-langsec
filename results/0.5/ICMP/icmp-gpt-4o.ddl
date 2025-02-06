ICMP : record {
    type : uint8;
    code : uint8;
    checksum : uint16;
    header : union {
        when (type == 0 || type == 8) : EchoHeader,
        when (type == 3) : DestinationUnreachableHeader,
        when (type == 4) : SourceQuenchHeader,
        when (type == 5) : RedirectHeader,
        when (type == 11) : TimeExceededHeader,
        when (type == 12) : ParameterProblemHeader,
        when (type == 13 || type == 14) : TimestampHeader,
        when (type == 15 || type == 16) : InformationHeader,
        when (type == 17 || type == 18) : AddressMaskHeader,
        default : UnknownHeader
    };
    data : bytes;
};

EchoHeader : record {
    identifier : uint16;
    sequence_number : uint16;
};

DestinationUnreachableHeader : record {
    unused : uint32;
};

SourceQuenchHeader : record {
    unused : uint32;
};

RedirectHeader : record {
    gateway_internet_address : uint32;
};

TimeExceededHeader : record {
    unused : uint32;
};

ParameterProblemHeader : record {
    pointer : uint8;
    unused : uint24;
};

TimestampHeader : record {
    identifier : uint16;
    sequence_number : uint16;
    originate_timestamp : uint32;
    receive_timestamp : uint32;
    transmit_timestamp : uint32;
};

InformationHeader : record {
    identifier : uint16;
    sequence_number : uint16;
};

AddressMaskHeader : record {
    identifier : uint16;
    sequence_number : uint16;
    address_mask : uint32;
};

UnknownHeader : record {
    rest_of_header : uint32;
};