struct ARP {
    // Hardware Type
    uint16 htype;

    // Protocol Type
    uint16 ptype;

    // Hardware Address Length
    uint8 hlen;

    // Protocol Address Length
    uint8 plen;

    // Operation
    uint16 oper;

    // Sender Hardware Address (variable length based on hlen)
    byte sha[hlen];

    // Sender Protocol Address (variable length based on plen)
    byte spa[plen];

    // Target Hardware Address (variable length based on hlen)
    byte tha[hlen];

    // Target Protocol Address (variable length based on plen)
    byte tpa[plen];
}