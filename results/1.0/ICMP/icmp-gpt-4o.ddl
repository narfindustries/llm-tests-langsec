@format(endianness=Endian::Big)
type ICMP {
    version: u4,
    ihl: u4,
    type_of_service: u8,
    total_length: u16,
    identification: u16,
    flags: u3,
    fragment_offset: u13,
    time_to_live: u8,
    protocol: u8,
    header_checksum: u16,
    source_address: u32,
    destination_address: u32,
    
    @if (protocol == 1) {
        icmp: ICMPPacket
    }
}

type ICMPPacket {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,

    @switch (icmp_type) {
        0 => echo_reply: EchoHeader,
        8 => echo_request: EchoHeader,
        3 => destination_unreachable: DestinationUnreachableHeader,
        _: raw_data: bytes(rest_of_header_length)
    },

    rest_of_header_length: u16
}

type EchoHeader {
    identifier: u16,
    sequence_number: u16
}

type DestinationUnreachableHeader {
    unused: u32
}