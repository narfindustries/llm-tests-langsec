module ICMP {
    ICMPMessage {
        type       : u8;   // Type of ICMP message
        code       : u8;   // Code of the ICMP message
        checksum   : u16;  // Checksum to verify the ICMP header and data

        // Define a switch for the rest of the header based on the ICMP type
        switch (type) {
            0, 8: {  // Echo Reply (0) and Echo Request (8)
                identifier     : u16;
                sequence_number: u16;
                data           : bytes;  // Variable length data
            }
            3: {  // Destination Unreachable
                unused        : u32;
                original_ip_header : bytes(20); // Original IP header
                original_data : bytes(8);  // First 8 bytes of the original datagram
            }
            5: {  // Redirect
                gateway       : ipv4_addr; // IPv4 address of the gateway
                original_ip_header : bytes(20); // Original IP header
                original_data : bytes(8);  // First 8 bytes of the original datagram
            }
            11: { // Time Exceeded
                unused        : u32;
                original_ip_header : bytes(20); // Original IP header
                original_data : bytes(8);  // First 8 bytes of the original datagram
            }
            12: { // Parameter Problem
                pointer       : u8;  // Pointer to the error
                unused        : bytes(3);
                original_ip_header : bytes(20); // Original IP header
                original_data : bytes(8);  // First 8 bytes of the original datagram
            }
            13, 14: { // Timestamp Request (13) and Timestamp Reply (14)
                identifier     : u16;
                sequence_number: u16;
                originate_timestamp : u32;
                receive_timestamp   : u32;
                transmit_timestamp  : u32;
            }
            17, 18: { // Address Mask Request (17) and Address Mask Reply (18)
                identifier     : u16;
                sequence_number: u16;
                address_mask   : u32;
            }
            default: {
                // Default case for unsupported or unknown types
                rest_of_header : bytes(4);
                data : bytes;
            }
        }
    }
}