meta:
  id: dns_packet
  title: DNS Packet
  license: CC0-1.0
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
    doc: |
      DNS domain name, represented as a series of labels. Each label is prefixed by its length.
    seq:
      - id: labels
        type: label
        repeat: until
        repeat-until: _.length == 0

  label:
    seq:
      - id: length
        type: u1

      - id: value
        size: length
        type: str
        encoding: utf-8

      - id: terminator
        size: 0
        if: length == 0