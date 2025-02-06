type ARP_Packet = struct {
    htype : uint16,  // Hardware type
    ptype : uint16,  // Protocol type
    hlen  : uint8,   // Hardware address length
    plen  : uint8,   // Protocol address length
    oper  : uint16,  // Operation
    sha   : array(uint8, hlen), // Sender hardware address
    spa   : array(uint8, plen), // Sender protocol address
    tha   : array(uint8, hlen), // Target hardware address
    tpa   : array(uint8, plen)  // Target protocol address
};