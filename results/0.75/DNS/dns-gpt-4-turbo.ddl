type U16be = uint16 : bigendian;
type U32be = uint32 : bigendian;

enum QR : uint1 {
  Query = 0,
  Response = 1
}

enum Opcode : uint4 {
  StandardQuery = 0,
  InverseQuery = 1,
  StatusRequest = 2
}

enum RCode : uint4 {
  NoError = 0,
  FormatError = 1,
  ServerFailure = 2,
  NameError = 3,
  NotImplemented = 4,
  Refused = 5
}

struct Flags {
  qr : QR;
  opcode : Opcode;
  aa : uint1;
  tc : uint1;
  rd : uint1;
  ra : uint1;
  z : uint3 = 0;
  rcode : RCode;
}

struct Header {
  id : U16be;
  flags : Flags;
  qdcount : U16be;
  ancount : U16be;
  nscount : U16be;
  arcount : U16be;
}

type Label = seq [uint8] (len, uint8, String : utf8);
type DomainName = list(Label);

struct Question {
  qname : DomainName;
  qtype : U16be;
  qclass : U16be;
}

struct ResourceRecord {
  name : DomainName;
  type : U16be;
  class : U16be;
  ttl : U32be;
  rdlength : U16be;
  rdata : bytes (size = rdlength);
}

struct DNSPacket {
  header : Header;
  questions : list(size = header.qdcount) of Question;
  answers : list(size = header.ancount) of ResourceRecord;
  authority : list(size = header.nscount) of ResourceRecord;
  additional : list(size = header.arcount) of ResourceRecord;
}