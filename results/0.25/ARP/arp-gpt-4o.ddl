ARP : struct {
    htype : uint16,                  // Hardware Type
    ptype : uint16,                  // Protocol Type
    hlen : uint8,                    // Hardware Address Length
    plen : uint8,                    // Protocol Address Length
    oper : uint16,                   // Operation
    sender_hw_addr : bytes[hlen],    // Sender Hardware Address
    sender_proto_addr : bytes[plen], // Sender Protocol Address
    target_hw_addr : bytes[hlen],    // Target Hardware Address
    target_proto_addr : bytes[plen]  // Target Protocol Address
}