meta:
  id: dns_packet
  title: DNS Packet
  endian: be
  xref:
    rfc: 1035
seq:
  - id: header
    type: header
  - id: queries
    type: query
    repeat: expr
    repeat-expr: header.qdcount
  - id: answers
    type: rr
    repeat: expr
    repeat-expr: header.ancount
  - id: authorities
    type: rr
    repeat: expr
    repeat-expr: header.nscount
  - id: additionals
    type: rr
    repeat: expr
    repeat-expr: header.arcount
types:
  header:
    seq:
      - id: transaction_id
        type: u2
      - id: flags
        type: u2
      - id: qdcount
        type: u2
      - id: ancount
        type: u2
      - id: nscount
        type: u2
      - id: arcount
        type: u2
  query:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
      - id: class
        type: u2
  rr:
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
      - id: name
        type: domain_name_label
        repeat: eos
      - id: terminator
        type: u1
        value: 0

  domain_name_label:
    seq:
      - id: length
        type: u1
      - id: label
        type: str
        size: length
        encoding: ASCII  

enums:
  type_type:
    1: a
    2: ns
    5: cname
    6: soa
    12: ptr
    15: mx
    16: txt
    28: aaaa

  class_type:
    1: inet
    2: cs
    3: chaos
    4: hesiod
    254: none
    255: any