meta:
  id: dns_packet
  title: DNS Packet
  application: Domain Name System
  file-extension: dns
  endian: be

seq:
  - id: transaction_id
    type: u2

  - id: flags
    type: flags

  - id: qdcount
    type: u2
    doc: Number of questions

  - id: ancount
    type: u2
    doc: Number of answer RRs

  - id: nscount
    type: u2
    doc: Number of authority RRs

  - id: arcount
    type: u2
    doc: Number of additional RRs

  - id: queries
    type: query
    repeat: expr
    repeat-expr: qdcount

  - id: answers
    type: resource_record
    repeat: expr
    repeat-expr: ancount

  - id: authorities
    type: resource_record
    repeat: expr
    repeat-expr: nscount

  - id: additionals
    type: resource_record
    repeat: expr
    repeat-expr: arcount

types:
  flags:
    seq:
      - id: qr
        type: b1
        doc: Query/Response Flag

      - id: opcode
        type: b4
        doc: Operation Code

      - id: aa
        type: b1
        doc: Authoritative Answer

      - id: tc
        type: b1
        doc: Truncation

      - id: rd
        type: b1
        doc: Recursion Desired

      - id: ra
        type: b1
        doc: Recursion Available

      - id: z
        type: b3
        doc: Reserved for future use

      - id: rcode
        type: b4
        doc: Response Code

  query:
    seq:
      - id: name
        type: domain_name

      - id: type
        type: u2

      - id: class
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
      - id: name_parts
        type: name_part
        repeat: until
        repeat-until: _.length == 0

  name_part:
    seq:
      - id: length
        type: u1

      - id: name
        size: length