struct ARPHeader {
    htype: uint16; // Hardware type
    ptype: uint16; // Protocol type
    hlen: uint8;   // Hardware address length
    plen: uint8;   // Protocol address length
    oper: uint16;  // Operation code
    sha: bytes[hlen]; // Sender hardware address
    spa: bytes[plen]; // Sender protocol address
    tha: bytes[hlen]; // Target hardware address
    tpa: bytes[plen]; // Target protocol address
}

struct ARPPacket {
    header: ARPHeader;
    payload: bytes[];
}

// Main entry point
struct ARP {
    packets: ARPPacket[];
}