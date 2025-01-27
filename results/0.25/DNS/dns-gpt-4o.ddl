module DNS;

type DNSMessage = struct {
    id: uint16;
    flags: DNSFlags;
    qdcount: uint16;
    ancount: uint16;
    nscount: uint16;
    arcount: uint16;
    questions: array[qdcount] of DNSQuestion;
    answers: array[ancount] of DNSRecord;
    authorities: array[nscount] of DNSRecord;
    additionals: array[arcount] of DNSRecord;
};

type DNSFlags = struct {
    qr: bit;
    opcode: uint4;
    aa: bit;
    tc: bit;
    rd: bit;
    ra: bit;
    z: uint3;
    rcode: uint4;
};

type DNSQuestion = struct {
    qname: DNSName;
    qtype: uint16;
    qclass: uint16;
};

type DNSRecord = struct {
    name: DNSName;
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: bytes[rdlength];
};

type DNSName = array of DNSLabel;

type DNSLabel = struct {
    length: uint8;
    label: bytes[length];
};