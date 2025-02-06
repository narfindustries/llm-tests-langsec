DNS_Message ::= {
  header: DNS_Header,
  question: DNS_Question[header.qdcount],
  answer: DNS_RR[header.ancount],
  authority: DNS_RR[header.nscount],
  additional: DNS_RR[header.arcount],
};

DNS_Header ::= {
  id: uint16,
  flags: DNS_Flags,
  qdcount: uint16,
  ancount: uint16,
  nscount: uint16,
  arcount: uint16,
};

DNS_Flags ::= {
  qr: uint1,
  opcode: uint4,
  aa: uint1,
  tc: uint1,
  rd: uint1,
  ra: uint1,
  z: uint3,
  rcode: uint4,
};

DNS_Question ::= {
  qname: DNS_Name,
  qtype: uint16,
  qclass: uint16,
};

DNS_RR ::= {
  name: DNS_Name,
  type: uint16,
  class: uint16,
  ttl: uint32,
  rdlength: uint16,
  rdata: uint8[rdlength],
};

DNS_Name ::= {
  labels: DNS_Label[],
  terminator: uint8(0),
};

DNS_Label ::= {
  length: uint8,
  data: uint8[length],
};