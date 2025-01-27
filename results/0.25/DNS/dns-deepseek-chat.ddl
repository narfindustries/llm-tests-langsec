// DNS Packet Header
struct DNSHeader {
    id: uint16;
    flags: uint16;
    qdcount: uint16;
    ancount: uint16;
    nscount: uint16;
    arcount: uint16;
}

// DNS Question Section
struct DNSQuestion {
    qname: stringz;
    qtype: uint16;
    qclass: uint16;
}

// DNS Resource Record
struct DNSResourceRecord {
    name: stringz;
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: bytes[rdlength];
}

// DNS Packet
struct DNSPacket {
    header: DNSHeader;
    questions: DNSQuestion[header.qdcount];
    answers: DNSResourceRecord[header.ancount];
    authorities: DNSResourceRecord[header.nscount];
    additional: DNSResourceRecord[header.arcount];
}

// Main DNS Packet Parser
struct DNSParser {
    packet: DNSPacket;
}