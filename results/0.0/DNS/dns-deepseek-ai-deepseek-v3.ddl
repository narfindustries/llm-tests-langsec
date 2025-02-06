DNSMessage = struct {
  header: struct {
    id: UInt16,
    flags: struct {
      qr: Bit,
      opcode: Bits(4),
      aa: Bit,
      tc: Bit,
      rd: Bit,
      ra: Bit,
      z: Bits(3),
      rcode: Bits(4)
    },
    qdcount: UInt16,
    ancount: UInt16,
    nscount: UInt16,
    arcount: UInt16
  },
  questions: Question[header.qdcount],
  answers: ResourceRecord[header.ancount],
  authorities: ResourceRecord[header.nscount],
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
  rdata: Bytes(rdlength)
};

DomainName = struct {
  labels: Label[],
  null_terminator: UInt8
};

Label = struct {
  length: UInt8,
  data: Bytes(length)
};