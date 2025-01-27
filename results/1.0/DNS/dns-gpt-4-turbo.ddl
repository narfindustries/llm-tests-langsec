namespace DNS;

-- Define constants for simplicity in the specification
const TRANSACTION_ID_SIZE: UInt16 = 16;
const FLAGS_SIZE: UInt16 = 16;
const QUESTION_COUNT_SIZE: UInt16 = 16;
const ANSWER_RR_SIZE: UInt16 = 16;
const AUTHORITY_RR_SIZE: UInt16 = 16;
const ADDITIONAL_RR_SIZE: UInt16 = 16;

-- Main DNS packet structure
type DNSPacket = struct {
  transactionID: UInt16,
  flags: UInt16,
  question_count: UInt16,
  answer_rr: UInt16,
  authority_rr: UInt16,
  additional_rr: UInt16,
  
  questions: [Question] | length = question_count,
  answers: [ResourceRecord] | length = answer_rr,
  authorities: [ResourceRecord] | length = authority_rr,
  additionals: [ResourceRecord] | length = additional_rr
};

-- Questions within the DNS Query
type Question = struct {
  qname: Qname,
  qtype: UInt16,
  qclass: UInt16
};

-- Domain name split into labels
type Qname = struct {
  labels: [string] *,
  end: UInt8 @(value == 0)
};

-- Resource records used in sections answering, authority, additional information
type ResourceRecord = struct {
  name: Qname,
  type: UInt16,
  class: UInt16,
  ttl: UInt32,
  data_length: UInt16,
  data: RData @(length = data_length)
};

-- Data content varying based on the type of record
type RData = switch {
  case _.type == 1: RD_A;            -- A (IPv4 address)
  case _.type == 5: RD_CNAME;        -- CNAME (Canonical Name)
  case _.type == 28: RD_AAAA;        -- AAAA (IPv6 address)
  default: Bytes;
};

-- Resource Data for A record (IPv4 address)
type RD_A = struct {
  address: IPv4
};

-- Resource Data for CNAME record
type RD_CNAME = struct {
  cname: Qname
};

-- Resource Data for AAAA record (IPv6 address)
type RD_AAAA = struct {
  address: IPv6
};