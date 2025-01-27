// Network Time Protocol (NTP) Version 4 Specification
module NetworkTimeProtocolV4

// NTP Packet Structure
type NtpPacket = {
    leap_indicator: 2 bits,  // Leap second indicator
    version: 3 bits,         // NTP protocol version
    mode: 3 bits,            // Client/server mode
    stratum: 8 bits,         // Server stratum level
    poll_interval: 8 bits,   // Poll interval 
    precision: 8 bits signed,// Clock precision
    root_delay: 32 bits,     // Root delay
    root_dispersion: 32 bits,// Root dispersion
    reference_id: 32 bits,   // Reference clock identifier
    reference_timestamp: 64 bits, // Reference timestamp
    originate_timestamp: 64 bits, // Client send timestamp
    receive_timestamp: 64 bits,   // Server receive timestamp
    transmit_timestamp: 64 bits   // Server transmit timestamp
}

// Main parsing function
parse_ntp_packet(input: [uint8]) -> NtpPacket = {
    let packet = {
        leap_indicator: input[0] >> 6,
        version: (input[0] >> 3) & 0b111,
        mode: input[0] & 0b111,
        stratum: input[1],
        poll_interval: input[2],
        precision: input[3] as int8,
        root_delay: bytes_to_uint32(input[4:8]),
        root_dispersion: bytes_to_uint32(input[8:12]),
        reference_id: bytes_to_uint32(input[12:16]),
        reference_timestamp: bytes_to_uint64(input[16:24]),
        originate_timestamp: bytes_to_uint64(input[24:32]),
        receive_timestamp: bytes_to_uint64(input[32:40]),
        transmit_timestamp: bytes_to_uint64(input[40:48])
    }
    return packet
}

// Helper function to convert byte array to 32-bit unsigned integer
bytes_to_uint32(bytes: [uint8]) -> uint32 = {
    return (bytes[0] << 24) | 
           (bytes[1] << 16) | 
           (bytes[2] << 8)  | 
           bytes[3]
}

// Helper function to convert byte array to 64-bit unsigned integer
bytes_to_uint64(bytes: [uint8]) -> uint64 = {
    return (bytes[0] as uint64 << 56) | 
           (bytes[1] as uint64 << 48) | 
           (bytes[2] as uint64 << 40) | 
           (bytes[3] as uint64 << 32) |
           (bytes[4] as uint64 << 24) | 
           (bytes[5] as uint64 << 16) | 
           (bytes[6] as uint64 << 8)  | 
           bytes[7] as uint64
}