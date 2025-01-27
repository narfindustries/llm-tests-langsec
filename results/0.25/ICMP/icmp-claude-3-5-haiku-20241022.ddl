typedef uint8 = bitvector[8]
typedef uint16 = bitvector[16]
typedef uint32 = bitvector[32]

struct ICMPHeader {
    type: uint8,
    code: uint8,
    checksum: uint16,
    rest_of_header: uint32
}

parser ICMPPacket {
    header: ICMPHeader,
    payload: remainder
}

format ICMPPacket {
    header {
        type: type,
        code: code,
        checksum: checksum,
        rest_of_header: rest_of_header
    },
    payload: payload
}

constraints ICMPPacket {
    // ICMP type constraints
    type in [0, 3, 8, 11, 12, 13, 14, 15, 16, 17, 18],
    
    // Checksum validation (simplified)
    checksum != 0x0000,
    
    // Payload size constraints
    len(payload) <= 576
}