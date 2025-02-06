ICMP : packet {
    uint8 type;
    uint8 code;
    uint16 checksum;

    switch (type) {
        case 0, 8:  // Echo Reply, Echo Request
            uint16 identifier;
            uint16 sequence_number;
            // Optional data follows

        case 3:  // Destination Unreachable
            uint32 unused;
            // Original IP header + first 8 bytes of original datagram's data follows

        case 4:  // Source Quench (Deprecated)
            uint32 unused;
            // Original IP header + first 8 bytes of original datagram's data follows

        case 5:  // Redirect
            uint32 gateway_internet_address;
            // Original IP header + first 8 bytes of original datagram's data follows

        case 11:  // Time Exceeded
            uint32 unused;
            // Original IP header + first 8 bytes of original datagram's data follows

        case 12:  // Parameter Problem
            uint8 pointer;
            uint24 unused;
            // Original IP header + first 8 bytes of original datagram's data follows

        case 13, 14:  // Timestamp, Timestamp Reply
            uint16 identifier;
            uint16 sequence_number;
            uint32 originate_timestamp;
            uint32 receive_timestamp;
            uint32 transmit_timestamp;

        case 17, 18:  // Address Mask Request, Address Mask Reply
            uint16 identifier;
            uint16 sequence_number;
            uint32 address_mask;

        default:
            // Handle other types if necessary
    }
}