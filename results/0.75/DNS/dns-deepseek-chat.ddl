// DNS Packet Specification in Daedalus
struct DNSHeader {
    id: uint16;
    flags: uint16;
    qdcount: uint16;
    ancount: uint16;
    nscount: uint16;
    arcount: uint16;
}

struct DNSQuestion {
    qname: stringz;
    qtype: uint16;
    qclass: uint16;
}

struct DNSResourceRecord {
    name: stringz;
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: bytes[rdlength];
}

struct DNSPacket {
    header: DNSHeader;
    questions: DNSQuestion[header.qdcount];
    answers: DNSResourceRecord[header.ancount];
    authorities: DNSResourceRecord[header.nscount];
    additionals: DNSResourceRecord[header.arcount];
}