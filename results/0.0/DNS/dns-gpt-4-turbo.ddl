module DNS;

import Binary;

type uint1 = Bits:1;
type uint3 = Bits:3;
type uint4 = Bits:4;
type uint8 = UInt:8;
type uint16 = UInt:16;
type uint32 = UInt:32;

type Header = struct {
    id : uint16;
    qr : uint1;
    opcode : uint4;
    aa : uint1;
    tc : uint1;
    rd : uint1;
    ra : uint1;
    z : uint3;
    rcode : uint4;
    qdcount : uint16;
    ancount : uint16;
    nscount : uint16;
    arcount : uint16;
};

type Question = struct {
    qname : DomainName;
    qtype : uint16;
    qclass : uint16;
};

type RR = struct {
    name : DomainName;
    type : uint16;
    class : uint16;
    ttl : uint32;
    rdlength : uint16;
    rdata : bytes &size=rdlength;
};

type DomainName = struct {
    labels : Label[];
};

type Label = struct {
    length : uint8;
    data : bytes &size=length;
};

type Message = struct {
    header : Header;
    questions : Question[header.qdcount];
    answers : RR[header.ancount];
    authorities : RR[header.nscount];
    additionals : RR[header.arcount];
};

type DNSPacket = struct {
    message : Message;
};