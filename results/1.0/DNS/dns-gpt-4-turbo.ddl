module DNS;

type uint8 : UInt(8);
type uint16 : UInt(16);
type uint32 : UInt(32);

type uint4 : UInt(4);
type uint1 : UInt(1);

struct QName {
    labels: List(PreLen(UInt(8), Bytes));
}

type QType : uint16;
type QClass : uint16;

struct Question {
    qname: QName;
    qtype: QType;
    qclass: QClass;
}

struct RR {
    name: QName;
    rrtype: QType;
    rrclass: QClass;
    ttl: uint32;
    rdlength: uint16;
    rdata: SizedBytes(rdlength);
}

struct Header {
    id: uint16;
    qr: uint1;
    opcode: uint4;
    aa: uint1;
    tc: uint1;
    rd: uint1;
    ra: uint1;
    z: uint3;
    rcode: uint4;
    qdcount: uint16;
    ancount: uint16;
    nscount: uint16;
    arcount: uint16;
}

struct DNS {
    header: Header;
    questions: List(PreLen(uint16, Question));
    answers: List(PreLen(uint16, RR));
    authorities: List(PreLen(uint16, RR));
    additional: List(PreLen(uint16, RR));
}

rule root = DNS;