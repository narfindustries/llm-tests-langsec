struct ARPHeader {
    htype: U16Be;  // Hardware type
    ptype: U16Be;  // Protocol type
    hlen: U8;      // Hardware address length
    plen: U8;      // Protocol address length
    oper: U16Be;   // Operation code
    sha: Bytes[hlen];  // Sender hardware address
    spa: Bytes[plen];  // Sender protocol address
    tha: Bytes[hlen];  // Target hardware address
    tpa: Bytes[plen];  // Target protocol address
}

struct ARPPacket {
    header: ARPHeader;
}