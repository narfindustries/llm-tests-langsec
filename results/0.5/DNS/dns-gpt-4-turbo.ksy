meta:
  id: dns_packet
  title: DNS Packet (RFC 1035)
  license: CC0-1.0
  endian: be

doc: |
  A DNS (Domain Name Service) packet is a protocol used for Internet
  name resolution.

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
    instances:
      qr:
        value: (flags & 0x8000) >> 15
      opcode:
        value: (flags & 0x7800) >> 11
      aa:
        value: (flags & 0x0400) >> 10
      tc:
        value: (flags & 0x0200) >> 9
      rd:
        value: (flags & 0x0100) >> 8
      ra:
        value: (flags & 0x0080) >> 7
      z:
        value: (flags & 0x0070) >> 4
      rcode:
        value: flags & 0x000f

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
        type:
          switch-on: type
          cases:
            1: ipv4_addr
            5: domain_name
            28: ipv6_addr

  domain_name:
    seq:
      - id: name
        type: label
        repeat: until
        repeat-until: _.length == 0

  label:
    seq:
      - id: length
        type: u1
      - id: name
        type: str
        size: length
        encoding: ASCII

  ipv4_addr:
    seq:
      - id: ip
        type: u4

  ipv6_addr:
    seq:
      - id: ip
        type: u8
        repeat: expr
        repeat-expr: 2