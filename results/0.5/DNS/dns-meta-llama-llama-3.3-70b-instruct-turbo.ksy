meta:
  id: dns
  title: Domain Name System (DNS) message
  endian: le

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

  - id: questions
    type: dns_query
    repeat: expr
    repeat-expr: qdcount

  - id: answers
    type: dns_rr
    repeat: expr
    repeat-expr: ancount

  - id: authorities
    type: dns_rr
    repeat: expr
    repeat-expr: nscount

  - id: additionals
    type: dns_rr
    repeat: expr
    repeat-expr: arcount

types:
  dns_query:
    seq:
      - id: name
        type: dns_domain

      - id: type
        type: u2

      - id: class
        type: u2

  dns_domain:
    seq:
      - id: len
        type: u1

      - id: domain
        type: str
        size: len
        encoding: ascii

      - id: next_len
        type: u1
        if: len != 0

      - id: next_domain
        type: dns_domain
        if: len != 0

  dns_rr:
    seq:
      - id: name
        type: u2

      - id: type
        type: u2

      - id: class
        type: u2

      - id: TTL
        type: u4

      - id: rd_length
        type: u2

      - id: rdata
        type: str
        size: rd_length
        encoding: ascii