type Byte = [0..255]

type DnsName = String

enum OpCode {
    Query = 0,
    IQuery = 1,
    Status = 2,
    Notify = 4,
    Update = 5
}

enum RCode {
    NoError = 0,
    FormatError = 1,
    ServerFailure = 2,
    NameError = 3,
    NotImplemented = 4,
    Refused = 5
}

enum RecordType {
    A = 1,
    NS = 2,
    CNAME = 5,
    SOA = 6,
    PTR = 12,
    MX = 15,
    TXT = 16,
    AAAA = 28
}

struct DnsHeader {
    id: [0..65535],
    qr: bool,
    opcode: OpCode,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: [0..7],
    rcode: RCode,
    qdcount: [0..65535],
    ancount: [0..65535], 
    nscount: [0..65535],
    arcount: [0..65535]
}

struct DnsQuestion {
    qname: DnsName,
    qtype: RecordType,
    qclass: [0..65535]
}

struct ResourceRecord {
    name: DnsName,
    type: RecordType,
    class: [0..65535],
    ttl: [0..4294967295],
    rdlength: [0..65535],
    rdata: [Byte]
}

struct DnsMessage {
    header: DnsHeader,
    questions: [DnsQuestion],
    answers: [ResourceRecord],
    authority: [ResourceRecord],
    additional: [ResourceRecord]
}