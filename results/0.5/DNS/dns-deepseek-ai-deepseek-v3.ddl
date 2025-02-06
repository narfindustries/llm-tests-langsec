dns_message = struct {
  header: struct {
    id: UInt16,
    qr: Bit,
    opcode: Bit[4],
    aa: Bit,
    tc: Bit,
    rd: Bit,
    ra: Bit,
    z: Bit[3],
    rcode: Bit[4],
    qdcount: UInt16,
    ancount: UInt16,
    nscount: UInt16,
    arcount: UInt16
  },
  question: Question[header.qdcount],
  answer: ResourceRecord[header.ancount],
  authority: ResourceRecord[header.nscount],
  additional: ResourceRecord[header.arcount]
};

Question = struct {
  qname: DomainName,
  qtype: UInt16,
  qclass: UInt16
};

ResourceRecord = struct {
  name: DomainName,
  type: UInt16,
  class: UInt16,
  ttl: UInt32,
  rdlength: UInt16,
  rdata: UInt8[rdlength]
};

DomainName = struct {
  labels: Label[]
};

Label = struct {
  length: UInt8,
  label: UInt8[length]
};