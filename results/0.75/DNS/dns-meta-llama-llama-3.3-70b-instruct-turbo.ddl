type dns = struct {
  header: header;
  question: question;
  answer: answer[];
  authority: authority[];
  additional: additional[];
}

type header = struct {
  id: uint16;
  qr: bool;
  opcode: opcode;
  aa: bool;
  tc: bool;
  rd: bool;
  ra: bool;
  z: uint3;
  rcode: rcode;
}

type question = struct {
  qname: domain_name;
  qtype: qtype;
  qclass: qclass;
}

type answer = struct {
  name: domain_name;
  type: qtype;
  class: qclass;
  ttl: uint32;
  rdlength: uint16;
  rdata: bytes;
}

type authority = struct {
  name: domain_name;
  type: qtype;
  class: qclass;
  ttl: uint32;
  rdlength: uint16;
  rdata: bytes;
}

type additional = struct {
  name: domain_name;
  type: qtype;
  class: qclass;
  ttl: uint32;
  rdlength: uint16;
  rdata: bytes;
}

type domain_name = bytes

type bytes = sequence uint8

type qtype = enum {
  a = 1;
  ns = 2;
  md = 3;
  mf = 4;
  cname = 5;
  soa = 6;
  mb = 7;
  mg = 8;
  mr = 9;
  null = 10;
  wks = 11;
  ptr = 12;
  hinfo = 13;
  minfo = 14;
  mx = 15;
  txt = 16;
  rp = 17;
  afsdb = 18;
  x25 = 19;
  isdn = 20;
  rt = 21;
  nsap = 22;
  nsap_ptr = 23;
  sig = 24;
  key = 25;
  px = 26;
  gpos = 27;
  aaaa = 28;
  loc = 29;
  nxt = 30;
  eid = 31;
  nimloc = 32;
  srv = 33;
  atma = 34;
  naptr = 35;
  kx = 36;
  cert = 37;
  a6 = 38;
  dname = 39;
  sink = 40;
  opt = 41;
  apl = 42;
  ds = 43;
  sshfp = 44;
  ipseckey = 45;
  rrsig = 46;
  nsec = 47;
  dnskey = 48;
  dhcid = 49;
  nsec3 = 50;
  nsec3param = 51;
  tlsa = 52;
  smimea = 53;
  hip = 54;
  ninfo = 55;
  rkey = 56;
  talink = 57;
  cds = 58;
  cdnskey = 59;
  openpgpkey = 60;
  csync = 61;
  zonemd = 62
}

type qclass = enum {
  in = 1;
  cs = 2;
  ch = 3;
  hs = 4
}

type opcode = enum {
  query = 0;
  iquery = 1;
  status = 2
}

type rcode = enum {
  no_error = 0;
  format_error = 1;
  server_failure = 2;
  name_error = 3;
  not_implemented = 4;
  refused = 5
}