type dns = struct {
  header: header,
  questions: array of question,
  answers: array of answer,
  authorities: array of authority,
  additionals: array of additional
}

type header = struct {
  id: uint16,
  flags: flags,
  qdcount: uint16,
  ancount: uint16,
  nscount: uint16,
  arcount: uint16
}

type flags = struct {
  qr: bool,
  opcode: uint4,
  aa: bool,
  tc: bool,
  rd: bool,
  ra: bool,
  z: uint3,
  rcode: uint4
}

type question = struct {
  qname: domain_name,
  qtype: uint16,
  qclass: uint16
}

type answer = struct {
  name: domain_name,
  type: uint16,
  class: uint16,
  ttl: uint32,
  rdlength: uint16,
  rdata: bytes
}

type authority = struct {
  name: domain_name,
  type: uint16,
  class: uint16,
  ttl: uint32,
  rdlength: uint16,
  rdata: bytes
}

type additional = struct {
  name: domain_name,
  type: uint16,
  class: uint16,
  ttl: uint32,
  rdlength: uint16,
  rdata: bytes
}

type domain_name = struct {
  labels: array of label
}

type label = struct {
  length: uint8,
  data: bytes
}

enum qtype = {
  A = 1,
  NS = 2,
  MD = 3,
  MF = 4,
  CNAME = 5,
  SOA = 6,
  MB = 7,
  MG = 8,
  MR = 9,
  NULL = 10,
  WKS = 11,
  PTR = 12,
  HINFO = 13,
  MINFO = 14,
  MX = 15,
  TXT = 16,
  ANY = 255
}

enum qclass = {
  IN = 1,
  CS = 2,
  CH = 3,
  HS = 4,
  ANY = 255
}

enum opcode = {
  QUERY = 0,
  IQUERY = 1,
  STATUS = 2,
  NOTIFY = 4,
  UPDATE = 5
}

enum rcode = {
  NOERROR = 0,
  FORMERR = 1,
  SERVFAIL = 2,
  NXDOMAIN = 3,
  NOTIMP = 4,
  REFUSED = 5
}