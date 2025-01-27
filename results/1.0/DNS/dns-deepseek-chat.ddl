// DNS Packet Structure
struct DNS_Packet {
    header: DNS_Header;
    questions: DNS_Question[];
    answers: DNS_ResourceRecord[];
    authorities: DNS_ResourceRecord[];
    additional: DNS_ResourceRecord[];
}

// DNS Header Structure
struct DNS_Header {
    id: uint16;
    flags: uint16;
    qdcount: uint16;
    ancount: uint16;
    nscount: uint16;
    arcount: uint16;
}

// DNS Question Section Structure
struct DNS_Question {
    qname: string;
    qtype: uint16;
    qclass: uint16;
}

// DNS Resource Record Structure
struct DNS_ResourceRecord {
    name: string;
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: uint8[rdlength];
}

// DNS Flags Structure
struct DNS_Flags {
    qr: uint1;
    opcode: uint4;
    aa: uint1;
    tc: uint1;
    rd: uint1;
    ra: uint1;
    z: uint3;
    rcode: uint4;
}

// DNS Packet Parser
parser DNS_Packet_Parser {
    packet: DNS_Packet;
}

// DNS Header Parser
parser DNS_Header_Parser {
    header: DNS_Header;
}

// DNS Question Parser
parser DNS_Question_Parser {
    question: DNS_Question;
}

// DNS Resource Record Parser
parser DNS_ResourceRecord_Parser {
    record: DNS_ResourceRecord;
}

// DNS Flags Parser
parser DNS_Flags_Parser {
    flags: DNS_Flags;
}

// Main Parser
parser Main {
    dns_packet: DNS_Packet_Parser;
}