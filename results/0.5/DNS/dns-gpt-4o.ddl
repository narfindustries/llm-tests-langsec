module DNS;

type DNSMessage = struct {
    id: uint16;
    flags: DNSFlags;
    qdcount: uint16;
    ancount: uint16;
    nscount: uint16;
    arcount: uint16;
    questions: Questions[qdcount];
    answers: ResourceRecords[ancount];
    authorities: ResourceRecords[nscount];
    additionals: ResourceRecords[arcount];
};

type DNSFlags = bitfield {
    qr: uint1;
    opcode: uint4;
    aa: uint1;
    tc: uint1;
    rd: uint1;
    ra: uint1;
    z: uint3;
    rcode: uint4;
};

type Questions = struct {
    qname: DomainName;
    qtype: uint16;
    qclass: uint16;
};

type ResourceRecords = struct {
    name: DomainName;
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: bytes[rdlength];
};

type DomainName = list(DomainLabel);

type DomainLabel = union {
    case length: uint8 {
        0: NullLabel;
        default: NonNullLabel;
    }
};

type NullLabel = struct {
    length: uint8 = 0;
};

type NonNullLabel = struct {
    length: uint8;
    label: bytes[length];
};