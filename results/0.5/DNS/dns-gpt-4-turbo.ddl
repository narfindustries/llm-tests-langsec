type uint8 = U8;
type uint16 = U16 : BigEndian;
type uint32 = U32 : BigEndian;

type DomainName = List(uint8) using
  | [] -> []
  | len -> len :: (Array(len, uint8) ++ DomainName);

type Header = Struct {
  id : uint16,
  qr : Bits(1),
  opcode : Bits(4),
  aa : Bits(1),
  tc : Bits(1),
  rd : Bits(1),
  ra : Bits(1),
  z : Bits(3),
  rcode : Bits(4),
  qdcount : uint16,
  ancount : uint16,
  nscount : uint16,
  arcount : uint16
};

type Question = Struct {
  qname : DomainName,
  qtype : uint16,
  qclass : uint16
};

type RRData = Match(uint16) {
  | 1 -> Struct { a : uint32 }
  | 2 -> Struct { ns : DomainName }
  | 5 -> Struct { cname : DomainName }
  | 6 -> Struct {
      mname : DomainName,
      rname : DomainName,
      serial : uint32,
      refresh : uint32,
      retry : uint32,
      expire : uint32,
      minimum : uint32
    }
  | 15 -> Struct {
      preference : uint16,
      exchange : DomainName
    }
  | 16 -> Array(uint8)
  | _ -> Array(uint8)
};

type ResourceRecord = Struct {
  name : DomainName,
  type : uint16,
  class : uint16,
  ttl : uint32,
  rdlength : uint16,
  rdata : RRData @ Length(rdlength)
};

type DNSPacket = Struct {
  header : Header,
  questions : Array(header.qdcount, Question),
  answers : Array(header.ancount, ResourceRecord),
  authority : Array(header.nscount, ResourceRecord),
  additional : Array(header.arcount, ResourceRecord)
};