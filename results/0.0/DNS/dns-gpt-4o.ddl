module DNS

type DNSMessage = struct {
    id: uint16;
    flags: DNSFlags;
    qdcount: uint16;
    ancount: uint16;
    nscount: uint16;
    arcount: uint16;
    questions: array[qdcount] of DNSQuestion;
    answers: array[ancount] of DNSResourceRecord;
    authorities: array[nscount] of DNSResourceRecord;
    additionals: array[arcount] of DNSResourceRecord;
}

type DNSFlags = struct {
    qr: bit; // Query/Response
    opcode: bits[4];
    aa: bit; // Authoritative Answer
    tc: bit; // Truncation
    rd: bit; // Recursion Desired
    ra: bit; // Recursion Available
    z: bits[3]; // Reserved
    rcode: bits[4]; // Response Code
}

type DNSQuestion = struct {
    qname: DNSName;
    qtype: uint16;
    qclass: uint16;
}

type DNSResourceRecord = struct {
    name: DNSName;
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: bytes[rdlength];
}

type DNSName = array of DNSLabel;

type DNSLabel = struct {
    length: uint8;
    label: bytes[length];
}