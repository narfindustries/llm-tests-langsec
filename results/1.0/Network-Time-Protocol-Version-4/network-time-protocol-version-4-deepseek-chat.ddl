// Network Time Protocol Version 4 (NTPv4) Daedalus Specification

struct NTPHeader {
    leap_indicator: uint2;      // Leap indicator (LI)
    version: uint3;             // Version number (VN)
    mode: uint3;                // Mode
    stratum: uint8;             // Stratum
    poll: uint8;                // Poll interval
    precision: uint8;           // Precision
    root_delay: uint32;         // Root delay
    root_dispersion: uint32;    // Root dispersion
    reference_id: uint32;       // Reference ID
    reference_timestamp: uint64; // Reference timestamp
    origin_timestamp: uint64;   // Origin timestamp
    receive_timestamp: uint64;  // Receive timestamp
    transmit_timestamp: uint64; // Transmit timestamp
}

struct NTPPacket {
    header: NTPHeader;
    // Optional extension fields can be added here if needed
}

// Main entry point for parsing NTP packets
fn parse_ntp_packet(data: bytes) -> NTPPacket {
    let packet = NTPPacket {
        header: parse_ntp_header(data),
    };
    return packet;
}

fn parse_ntp_header(data: bytes) -> NTPHeader {
    let header = NTPHeader {
        leap_indicator: data[0] >> 6,
        version: (data[0] >> 3) & 0x07,
        mode: data[0] & 0x07,
        stratum: data[1],
        poll: data[2],
        precision: data[3],
        root_delay: read_uint32(data, 4),
        root_dispersion: read_uint32(data, 8),
        reference_id: read_uint32(data, 12),
        reference_timestamp: read_uint64(data, 16),
        origin_timestamp: read_uint64(data, 24),
        receive_timestamp: read_uint64(data, 32),
        transmit_timestamp: read_uint64(data, 40),
    };
    return header;
}

fn read_uint32(data: bytes, offset: uint) -> uint32 {
    return (data[offset] << 24) | (data[offset + 1] << 16) | (data[offset + 2] << 8) | data[offset + 3];
}

fn read_uint64(data: bytes, offset: uint) -> uint64 {
    return (read_uint32(data, offset) << 32) | read_uint32(data, offset + 4);
}