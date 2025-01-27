// ICMP Packet Specification
// Comprehensive ICMP packet parsing and generation

struct ICMPHeader {
    type: uint8,
    code: uint8,
    checksum: uint16,
}

struct ICMPEchoRequest {
    header: ICMPHeader,
    identifier: uint16,
    sequence_number: uint16,
    payload: [uint8],
}

struct ICMPEchoReply {
    header: ICMPHeader,
    identifier: uint16, 
    sequence_number: uint16,
    payload: [uint8],
}

let icmp_type_echo_request = 8;
let icmp_type_echo_reply = 0;

parser ICMPPacket {
    header: ICMPHeader,
    payload: match header.type {
        icmp_type_echo_request => ICMPEchoRequest,
        icmp_type_echo_reply => ICMPEchoReply,
        _ => [uint8]
    }
}

generator ICMPPacketGenerator {
    generate(packet: ICMPPacket) {
        header: packet.header,
        payload: packet.payload
    }
}

// Checksum calculation function
function calculate_checksum(data: [uint8]) -> uint16 {
    let sum: uint32 = 0;
    for (let i = 0; i < data.length; i += 2) {
        let value = (data[i] << 8) | (data[i+1] || 0);
        sum += value;
    }
    
    while (sum >> 16 != 0) {
        sum = (sum & 0xFFFF) + (sum >> 16);
    }
    
    return ~sum & 0xFFFF;
}