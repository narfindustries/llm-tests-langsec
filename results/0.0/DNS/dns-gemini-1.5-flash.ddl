grammar DNS {
  header = ID:uint16, QR:uint1, OPCODE:uint4, AA:uint1, TC:uint1, RD:uint1, RA:uint1, Z:uint3, RCODE:uint4, QDCOUNT:uint16, ANCOUNT:uint16, NSCOUNT:uint16, ARCOUNT:uint16;

  question = QNAME:domain, QTYPE:uint16, QCLASS:uint16;

  resourceRecord = NAME:domain, TYPE:uint16, CLASS:uint16, TTL:uint32, RDLENGTH:uint16, RDATA:rData;

  domain = label*;

  label = length:uint8, data:bytes:length;

  rData = TYPE:uint16, data:switch TYPE {
    case 1: A_RDATA;
    case 28: AAAA_RDATA;
    case 15: MX_RDATA;
    case 2: NS_RDATA;
    case 5: CNAME_RDATA;
    case 6: SOA_RDATA;
    case 16: TXT_RDATA;
    default: bytes:RDLENGTH;
  };

  A_RDATA = address:ipv4;
  AAAA_RDATA = address:ipv6;
  MX_RDATA = preference:uint16, exchange:domain;
  NS_RDATA = nameserver:domain;
  CNAME_RDATA = canonicalName:domain;
  SOA_RDATA = mname:domain, rname:domain, serial:uint32, refresh:uint32, retry:uint32, expire:uint32, minimum:uint32;
  TXT_RDATA = text:string;

  message = header, questions:question*, answers:resourceRecord*, authorities:resourceRecord*, additionals:resourceRecord*;

  ipv4 = octet1:uint8, octet2:uint8, octet3:uint8, octet4:uint8;
  ipv6 = hextet1:uint16, hextet2:uint16, hextet3:uint16, hextet4:uint16, hextet5:uint16, hextet6:uint16, hextet7:uint16, hextet8:uint16;
  string = length:uint8, data:bytes:length;
}
