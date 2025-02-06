struct DNS {
    ID: uint16;
    QR: uint1;
    OPCODE: uint4;
    AA: uint1;
    TC: uint1;
    RD: uint1;
    RA: uint1;
    Z: uint3;
    RCODE: uint4;
    QDCOUNT: uint16;
    ANCOUNT: uint16;
    NSCOUNT: uint16;
    ARCOUNT: uint16;
    questions: Question[QDCOUNT];
    answers: ResourceRecord[ANCOUNT];
    authorities: ResourceRecord[NSCOUNT];
    additionals: ResourceRecord[ARCOUNT];
}

struct Label {
    length: uint8;
    name: bytes(length);
}

union DomainName {
    struct {
        labels: Label[];
        terminator: uint8;
    }
    struct {
        pointer: uint16;
    }
}

struct Question {
    name: DomainName;
    qtype: uint16;
    qclass: uint16;
}

struct ResourceRecord {
    name: DomainName;
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: bytes(rdlength);
}

enum TYPE {
    A = 1,
    NS = 2,
    CNAME = 5,
    SOA = 6,
    PTR = 12,
    HINFO = 13,
    MX = 15,
    AAAA = 28,
    AXFR = 252,
    ANY = 255
}

enum CLASS {
    IN = 1,
    CS = 2,
    CH = 3,
    HS = 4,
    ANY = 255
}

enum RCODE {
    NOERROR = 0,
    FORMERR = 1,
    SERVFAIL = 2,
    NXDOMAIN = 3,
    NOTIMP = 4,
    REFUSED = 5
}

enum OPCODE {
    QUERY = 0,
    IQUERY = 1,
    STATUS = 2
}