meta:
  id: dns_packet
  title: DNS (Domain Name System) Protocol
  endian: be
doc: |
  DNS is a protocol used for resolving hostnames to IP addresses.

seq:
  - id: transaction_id
    type: u2
  - id: flags
    type: dns_flags
  - id: qdcount
    type: u2
  - id: ancount
    type: u2
  - id: nscount
    type: u2
  - id: arcount
    type: u2
  - id: queries
    type: dns_query
    repeat: expr
    repeat-expr: qdcount
  - id: answers
    type: dns_answer
    repeat: expr
    repeat-expr: ancount
  - id: authority_rrs
    type: dns_answer
    repeat: expr
    repeat-expr: nscount
  - id: additional_rrs
    type: dns_answer
    repeat: expr
    repeat-expr: arcount

types:
  dns_flags:
    seq:
      - id: qr
        type: b1
      - id: opcode
        type: b4
      - id: aa
        type: b1
      - id: tc
        type: b1
      - id: rd
        type: b1
      - id: ra
        type: b1
      - id: z
        type: b3
      - id: rcode
        type: b4

  dns_query:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
      - id: query_class
        type: u2

  dns_answer:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
      - id: answer_class
        type: u2
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        size: rdlength
        type:
          switch-on: type
          cases:
            1: ipv4_addr  # TYPE A
            5: cname      # TYPE CNAME
            28: ipv6_addr  # TYPE AAAA

  domain_name:
    seq:
      - id: length
        type: u1
      - id: name
        type: str
        size: length
        encoding: ASCII
        if: length > 0
      - id: remainder
        type: domain_name
        if: length > 0

  ipv4_addr:
    seq:
      - id: ip
        type: u4

  ipv6_addr:
    seq:
      - id: ip_part
        type: u2
        repeat: expr
        repeat-expr: 8

  cname:
    seq:
      - id: target
        type: domain_name
