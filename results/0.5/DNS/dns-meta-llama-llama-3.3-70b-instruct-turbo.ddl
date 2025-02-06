format dns 1.0

domain_name = 
  label: 
    length: uint8,
    data: string,
  switch length
    case 0: 
      null_label: null
    case 192: 
      pointer: uint8
    default: 
      label: label,
      domain_name: domain_name

ipv4_address = 
  address: uint32

txt_data = 
  length: uint8,
  data: string

any_data = 
  length: uint8,
  data: string

dns_header = 
  id: uint16,
  flags: 
    qr: bool,
    opcode: uint4,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: uint3,
    rcode: uint4,
  qdcount: uint16,
  ancount: uint16,
  nscount: uint16,
  arcount: uint16

dns_question = 
  qname: domain_name,
  qtype: uint16,
  qclass: uint16

dns_answer = 
  name: domain_name,
  type: uint16,
  class: uint16,
  ttl: uint32,
  rdlength: uint16,
  rdata: 
    type: uint16,
    switch type
      case 1: 
        address: ipv4_address
      case 2: 
        nsdname: domain_name
      case 5: 
        cname: domain_name
      case 6: 
        mname: domain_name,
        rname: domain_name,
        serial: uint32,
        refresh: uint32,
        retry: uint32,
        expire: uint32,
        minimum: uint32
      case 15: 
        preference: uint16,
        exchange: domain_name
      case 16: 
        txt_data: txt_data
      case 255: 
        any_data: any_data

dns_authority = 
  name: domain_name,
  type: uint16,
  class: uint16,
  ttl: uint32,
  rdlength: uint16,
  rdata: 
    type: uint16,
    switch type
      case 2: 
        nsdname: domain_name
      case 6: 
        mname: domain_name,
        rname: domain_name,
        serial: uint32,
        refresh: uint32,
        retry: uint32,
        expire: uint32,
        minimum: uint32

dns_additional = 
  name: domain_name,
  type: uint16,
  class: uint16,
  ttl: uint32,
  rdlength: uint16,
  rdata: 
    type: uint16,
    switch type
      case 1: 
        address: ipv4_address
      case 2: 
        nsdname: domain_name
      case 5: 
        cname: domain_name

dns_message = 
  header: dns_header,
  question: array(dns_question),
  answer: array(dns_answer),
  authority: array(dns_authority),
  additional: array(dns_additional)