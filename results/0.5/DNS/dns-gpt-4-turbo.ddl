module DNS;

import DAEDALUS::BitFields;

type U8 = UInt 8;
type U16 = UInt 16;
type U32 = UInt 32;

record Header {
    id          : U16;
    qr          : U1;
    opcode      : U4;
    aa          : U1;
    tc          : U1;
    rd          : U1;
    ra          : U1;
    z           : U3;
    rcode       : U4;
    qdcount     : U16;
    ancount     : U16;
    nscount     : U16;
    arcount     : U16;
}

record Question {
    qname       : List<U8> &until(v -> v == 0);
    qtype       : U16;
    qclass      : U16;
}

record ResourceRecord {
    name        : List<U8> &until(v -> v == 0);
    rtype       : U16;
    rclass      : U16;
    ttl         : U32;
    rdlength    : U16;
    rdata       : List<U8> &length(rdlength);
}

record DNSPacket {
    header      : Header;
    questions   : List<Question> &length(header.qdcount);
    answers     : List<ResourceRecord> &length(header.ancount);
    authorities : List<ResourceRecord> &length(header.nscount);
    additionals : List<ResourceRecord> &length(header.arcount);
}

entrypoint DNSPacket;