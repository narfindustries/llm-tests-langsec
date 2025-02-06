meta:
  id: dns_packet
  title: DNS Packet
  application: DNS
  endian: be
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

types:
  header:
    seq:
      - id: id
        type: u2
      - id: flags
        type: flags
      - id: qdcount
        type: u2
      - id: ancount
        type: u2
      - id: nscount
        type: u2
      - id: arcount
        type: u2

  flags:
    seq:
      - id: qr
        type: b1
        doc: "Query/Response Flag"
      - id: opcode
        type: b4
        doc: "Operation Code"
      - id: aa
        type: b1
        doc: "Authoritative Answer"
      - id: tc
        type: b1
        doc: "Truncation Flag"
      - id: rd
        type: b1
        doc: "Recursion Desired"
      - id: ra
        type: b1
        doc: "Recursion Available"
      - id: z
        type: b3
        doc: "Reserved for future use"
      - id: rcode
        type: b4
        doc: "Response Code"

  question:
    seq:
      - id: qname
        type: domain_name
      - id: qtype
        type: u2
      - id: qclass
        type: u2

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
      - id: class
        type: u2
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        size: rdlength

  domain_name:
    seq:
      - id: labels
        type: label
        repeat: until
        repeat-until: _.length == 0

  label:
    seq:
      - id: length
        type: u1
      - id: name
        size: length
        if: length > 0