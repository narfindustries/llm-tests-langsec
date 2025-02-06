type DNSHeader = struct {
    id          : uint16;
    qr          : uint1;
    opcode      : uint4;
    aa          : uint1;
    tc          : uint1;
    rd          : uint1;
    ra          : uint1;
    z           : uint3;
    rcode       : uint4;
    qdcount     : uint16;
    ancount     : uint16;
    nscount     : uint16;
    arcount     : uint16;
};

type DNSQuestion = struct {
    qname       : DomainName;
    qtype       : uint16;
    qclass      : uint16;
};

type DomainName = struct {
    labels      : DomainLabel[];
};

type DomainLabel = struct {
    length      : uint8;
    label       : uint8[length];
} until length == 0;

type ResourceRecord = struct {
    name        : DomainName;
    type        : uint16;
    class       : uint16;
    ttl         : uint32;
    rdlength    : uint16;
    rdata       : uint8[rdlength];
};

type DNSPacket = struct {
    header      : DNSHeader;
    questions   : DNSQuestion[header.qdcount];
    answers     : ResourceRecord[header.ancount];
    authorities : ResourceRecord[header.nscount];
    additionals : ResourceRecord[header.arcount];
};