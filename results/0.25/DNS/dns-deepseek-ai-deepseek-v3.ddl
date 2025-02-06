dns {
  header {
    id: uint16;
    qr: bit;
    opcode: bits[4];
    aa: bit;
    tc: bit;
    rd: bit;
    ra: bit;
    z: bits[3];
    rcode: bits[4];
    qdcount: uint16;
    ancount: uint16;
    nscount: uint16;
    arcount: uint16;
  }
  question {
    qname: string;
    qtype: uint16;
    qclass: uint16;
  }
  resource_record {
    name: string;
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: bytes[rdlength];
  }
  answer: resource_record[header.ancount];
  authority: resource_record[header.nscount];
  additional: resource_record[header.arcount];
}