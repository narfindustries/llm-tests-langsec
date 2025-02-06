struct ARP {
    uint16 htype;          // Hardware Type
    uint16 ptype;          // Protocol Type
    uint8 hlen;            // Hardware Address Length
    uint8 plen;            // Protocol Address Length
    uint16 oper;           // Operation
    uint8[hlen] sha;       // Sender Hardware Address
    uint8[plen] spa;       // Sender Protocol Address
    uint8[hlen] tha;       // Target Hardware Address
    uint8[plen] tpa;       // Target Protocol Address
}