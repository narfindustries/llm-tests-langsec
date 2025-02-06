module DNS {
  import std.bitops;

  record Header {
    uint16 id;
    uint16 flags;
    uint16 qdcount;
    uint16 ancount;
    uint16 nscount;
    uint16 arcount;
  }

  record Question {
    bytes name;
    uint16 type;
    uint16 class;
  }

  record ResourceRecord {
    bytes name;
    uint16 type;
    uint16 class;
    uint32 ttl;
    uint16 rdlength;
    bytes rdata;
  }

  record DNSMessage {
    Header header;
    array[qdcount] Question questions;
    array[ancount] ResourceRecord answers;
    array[nscount] ResourceRecord authority;
    array[arcount] ResourceRecord additional;
  }

  function parseDomainName(bytes data) -> string {
    string name = "";
    int pos = 0;
    while (pos < data.length) {
      int len = data[pos];
      if (len == 0) break;
      name += data.slice(pos + 1, pos + len + 1).decode("ascii") + ".";
      pos += len + 1;
    }
    return name.trimEnd(".");
  }

  record RDATA_A {
    uint32 address;
  }

  record RDATA_AAAA {
    uint128 address;
  }

  record RDATA_NS {
    bytes nameserver;
  }

  record RDATA_MX {
    uint16 preference;
    bytes exchange;
  }

  record RDATA_CNAME {
    bytes canonicalName;
  }

  record RDATA_TXT {
    bytes text; 
  }

  record RDATA_SOA {
    bytes mname;
    bytes rname;
    uint32 serial;
    uint32 refresh;
    uint32 retry;
    uint32 expire;
    uint32 minimum;
  }

  function parseDNS(bytes data) -> DNSMessage {
    DNSMessage message = data.parse(DNSMessage);
    for (q in message.questions) {
      q.name = parseDomainName(q.name);
    }
    for (a in message.answers) {
      a.name = parseDomainName(a.name);
      switch (a.type) {
        case 1: a.rdata = a.rdata.parse(RDATA_A); break;
        case 28: a.rdata = a.rdata.parse(RDATA_AAAA); break;
        case 2: a.rdata = a.rdata.parse(RDATA_NS); break;
        case 15: a.rdata = a.rdata.parse(RDATA_MX); break;
        case 5: a.rdata = a.rdata.parse(RDATA_CNAME); break;
        case 16: a.rdata = a.rdata.parse(RDATA_TXT); break;
        case 6: a.rdata = a.rdata.parse(RDATA_SOA); break;
        default: break;
      }
    }
    //Process authority and additional records similarly if needed.
    return message;
  }
}
