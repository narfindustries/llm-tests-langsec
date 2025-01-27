type byte = uint8
type uint16 = uint16_t
type uint32 = uint32_t

struct DNSHeader {
    id: uint16,
    qr: bool,
    opcode: uint4,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: uint3,
    rcode: uint4,
    qdcount: uint16,
    ancount: uint16,
    nscount: uint16,
    arcount: uint16
}

struct DNSQuestion {
    qname: list<byte>,
    qtype: uint16,
    qclass: uint16
}

struct DNSResourceRecord {
    name: list<byte>,
    type: uint16,
    class: uint16,
    ttl: uint32,
    rdlength: uint16,
    rdata: list<byte>
}

struct DNSMessage {
    header: DNSHeader,
    questions: list<DNSQuestion>,
    answers: list<DNSResourceRecord>,
    authorities: list<DNSResourceRecord>,
    additionals: list<DNSResourceRecord>
}

parse DNSMessage {
    header = parse_struct DNSHeader,
    questions = repeat(parse_struct DNSQuestion, header.qdcount),
    answers = repeat(parse_struct DNSResourceRecord, header.ancount),
    authorities = repeat(parse_struct DNSResourceRecord, header.nscount),
    additionals = repeat(parse_struct DNSResourceRecord, header.arcount)
}