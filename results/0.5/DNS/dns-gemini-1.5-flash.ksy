types:
  domain_name:
    seq:
      - id: length
        type: u2le
      - id: data
        type: bytes
        size: length
  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2le
      - id: class
        type: u2le
      - id: ttl
        type: u4le
      - id: rdlength
        type: u2le
      - id: rdata
        type: bytes
        size: rdlength
  header:
    seq:
      - id: id
        type: u2le
      - id: flags
        type: u2le
      - id: qdcount
        type: u2le
      - id: ancount
        type: u2le
      - id: nscount
        type: u2le
      - id: arcount
        type: u2le
  question:
    seq:
      - id: qname
        type: domain_name
      - id: qtype
        type: u2le
      - id: qclass
        type: u2le
  dns_message:
    seq:
      - id: header
        type: header
      - id: questions
        type: question
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

