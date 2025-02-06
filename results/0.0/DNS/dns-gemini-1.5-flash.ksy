types:
  domain_name:
    seq:
      - id: length
        type: u2be
      - id: data
        type: bytes
        size: length
  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2be
      - id: class
        type: u2be
      - id: ttl
        type: u4be
      - id: rdlength
        type: u2be
      - id: rdata
        type: bytes
        size: rdlength
  dns_header:
    seq:
      - id: id
        type: u2be
      - id: flags
        type: u2be
      - id: qdcount
        type: u2be
      - id: ancount
        type: u2be
      - id: nscount
        type: u2be
      - id: arcount
        type: u2be
  dns_question:
    seq:
      - id: qname
        type: domain_name
      - id: qtype
        type: u2be
      - id: qclass
        type: u2be
  dns_message:
    seq:
      - id: header
        type: dns_header
      - id: questions
        type: dns_question
        repeat: expr
        repeat-expr: header.qdcount
      - id: answers
        type: resource_record
        repeat: expr
        repeat-expr: header.ancount
      - id: authorities
        type: resource_record
        repeat: expr
        repeat-expr: header.nscount
      - id: additionals
        type: resource_record
        repeat: expr
        repeat-expr: header.arcount

