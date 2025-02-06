ICMP : packet {
    type: uint8;
    code: uint8;
    checksum: uint16;

    body: switch(type) {
        0, 8 => echo;
        3 => destination_unreachable;
        4 => source_quench;
        5 => redirect;
        11 => time_exceeded;
        12 => parameter_problem;
        13, 14 => timestamp;
        17, 18 => address_mask;
        else => raw_data;
    };
};

echo : packet {
    identifier: uint16;
    sequence_number: uint16;
    data: bytes;
};

destination_unreachable : packet {
    unused: uint32;
    original_datagram: bytes;
};

source_quench : packet {
    unused: uint32;
    original_datagram: bytes;
};

redirect : packet {
    gateway_internet_address: uint32;
    original_datagram: bytes;
};

time_exceeded : packet {
    unused: uint32;
    original_datagram: bytes;
};

parameter_problem : packet {
    pointer: uint8;
    unused: uint24;
    original_datagram: bytes;
};

timestamp : packet {
    identifier: uint16;
    sequence_number: uint16;
    originate_timestamp: uint32;
    receive_timestamp: uint32;
    transmit_timestamp: uint32;
};

address_mask : packet {
    identifier: uint16;
    sequence_number: uint16;
    address_mask: uint32;
};

raw_data : packet {
    data: bytes;
};