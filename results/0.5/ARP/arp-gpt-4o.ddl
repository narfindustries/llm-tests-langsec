ARP :: {
    htype: uint16,          // Hardware Type
    ptype: uint16,          // Protocol Type
    hlen: uint8,            // Hardware Address Length
    plen: uint8,            // Protocol Address Length
    oper: uint16,           // Operation
    sha: bytes[hlen],       // Sender Hardware Address
    spa: bytes[plen],       // Sender Protocol Address
    tha: bytes[hlen],       // Target Hardware Address
    tpa: bytes[plen]        // Target Protocol Address
}