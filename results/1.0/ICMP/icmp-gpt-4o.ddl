icmp {
    type: uint8;
    code: uint8;
    checksum: uint16;

    data: switch (type) {
        0, 8 => echo;
        3 => destination_unreachable;
        4 => source_quench;
        5 => redirect;
        11 => time_exceeded;
        12 => parameter_problem;
        13, 14 => timestamp;
        17, 18 => address_mask;
        * => payload;
    };
}

type echo = struct {
    identifier: uint16;
    sequence_number: uint16;
    data: bytes[];
};

type destination_unreachable = struct {
    unused: uint32;
    original_datagram: original_datagram;
};

type source_quench = struct {
    unused: uint32;
    original_datagram: original_datagram;
};

type redirect = struct {
    gateway_internet_address: uint32;
    original_datagram: original_datagram;
};

type time_exceeded = struct {
    unused: uint32;
    original_datagram: original_datagram;
};

type parameter_problem = struct {
    pointer: uint8;
    unused: bytes[3];
    original_datagram: original_datagram;
};

type timestamp = struct {
    identifier: uint16;
    sequence_number: uint16;
    originate_timestamp: uint32;
    receive_timestamp: uint32;
    transmit_timestamp: uint32;
};

type address_mask = struct {
    identifier: uint16;
    sequence_number: uint16;
    address_mask: uint32;
};

type original_datagram = struct {
    internet_header: bytes[20]; // Minimum Internet Header size
    data: bytes[8];
};

type payload = bytes[];