format dns {
  dns_message: 
    header: dns_header,
    questions: repeat(dns_question, header.qdcount),
    answers: repeat(dns_rr, header.ancount),
    authorities: repeat(dns_rr, header.nscount),
    additionals: repeat(dns_rr, header.arcount)
  
  dns_header: 
    id: uint16,
    qr: uint1,
    opcode: uint4,
    aa: uint1,
    tc: uint1,
    rd: uint1,
    ra: uint1,
    z: uint3,
    rcode: uint4,
    qdcount: uint16,
    ancount: uint16,
    nscount: uint16,
    arcount: uint16
  
  dns_question: 
    qname: domain_name,
    qtype: uint16,
    qclass: uint16
  
  dns_rr: 
    name: domain_name,
    type: uint16,
    class: uint16,
    ttl: uint32,
    rdlength: uint16,
    rdata: bytes with length rdlength
  
  domain_name: 
    repeat(label, 1..)
  
  label: 
    length: uint8,
    data: bytes with length length
}