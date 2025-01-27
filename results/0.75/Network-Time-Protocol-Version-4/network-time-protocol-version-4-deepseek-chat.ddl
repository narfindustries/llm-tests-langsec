// Network Time Protocol Version 4 (NTPv4) Daedalus Specification

struct NTPHeader {
    leap_indicator: uint2;      // Leap indicator (LI)
    version: uint3;             // Protocol version (VN)
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
    // Optional: Add extension fields if needed
}

// Main entry point
func parse_ntp_packet(data: bytes): NTPPacket {
    return parse NTPPacket from data;
}