dns = {
  header: {
    id: uint16;
    flags: {
      qr: bit;
      opcode: bits(4);
      aa: bit;
      tc: bit;
      rd: bit;
      ra: bit;
      z: bits(3);
      rcode: bits(4);
    };
    qdcount: uint16;
    ancount: uint16;
    nscount: uint16;
    arcount: uint16;
  };
  questions: {
    qname: {
      labels: {
        length: byte;
        label: bytes(length);
      }[];
      null: byte(0x00);
    };
    qtype: uint16;
    qclass: uint16;
  }[header.qdcount];
  answers: {
    name: {
      labels: {
        length: byte;
        label: bytes(length);
      }[];
      null: byte(0x00);
    };
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: bytes(rdlength);
  }[header.ancount];
  authorities: {
    name: {
      labels: {
        length: byte;
        label: bytes(length);
      }[];
      null: byte(0x00);
    };
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: bytes(rdlength);
  }[header.nscount];
  additional: {
    name: {
      labels: {
        length: byte;
        label: bytes(length);
      }[];
      null: byte(0x00);
    };
    type: uint16;
    class: uint16;
    ttl: uint32;
    rdlength: uint16;
    rdata: bytes(rdlength);
  }[header.arcount];
};