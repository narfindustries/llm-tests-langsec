domain dns {
  header: {
    uint16 id;
    bits flags: {
      bool qr;
      uint4 opcode;
      bool aa;
      bool tc;
      bool rd;
      bool ra;
      uint3 reserved;
      uint4 rcode;
    }
    uint16 qdcount;
    uint16 ancount;
    uint16 nscount;
    uint16 arcount;
  }

  question: {
    string qname;
    uint16 qtype;
    uint16 qclass;
  }

  resource_record: {
    string name;
    uint16 type;
    uint16 class;
    uint32 ttl;
    uint16 rdlength;
    bytes rdata[rdlength];
  }

  message: {
    header header;
    question questions[header.qdcount];
    resource_record answers[header.ancount];
    resource_record authorities[header.nscount];
    resource_record additionals[header.arcount];
  }

  string: {
    seq label: {
      uint8 len;
      char data[len];
    }
    label* labels;
  }
}
