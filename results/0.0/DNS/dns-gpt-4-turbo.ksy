meta:
  id: dns_packet
  title: DNS Packet
  endian: be
doc: |
  DNS (Domain Name System) is a hierarchical decentralized naming system for computers,
  services, or other resources connected to the Internet or a private network. It associates
  various information with domain names assigned to each of the participating entities.
  Most prominently, it translates more readily memorized domain names to the numerical
  IP addresses needed for locating and identifying computer services and devices with
  the underlying network protocols. By providing a worldwide, distributed directory service,
  the Domain Name System has been an essential component of the functionality of the Internet since 1985.
  This spec covers the structure of DNS packets, both query and response types.

seq:
  - id: header
    type: header

  - id: queries
    type: query
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
        enum: flags
      - id: qdcount
        type: u2
      - id: ancount
        type: u2
      - id: nscount
        type: u2
      - id: arcount
        type: u2
    enums:
      flags:
        0x8000: qr
        0x7800: opcode
        0x0400: aa
        0x0200: tc
        0x0100: rd
        0x0080: ra
        0x0070: z
        0x000F: rcode

  query:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type_enum
      - id: class
        type: u2
        enum: class_enum

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type_enum
      - id: class
        type: u2
        enum: class_enum
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        size: rdlength

  domain_name:
    seq:
      - id: parts
        type: label
        repeat: until
        repeat-until: _.length == 0
    types:
      label:
        seq:
          - id: length
            type: u1
          - id: name
            type: str
            size: length
            encoding: ASCII

enums:
  type_enum:
    1: a
    2: ns
    5: cname
    6: soa
    12: ptr
    15: mx
    16: txt
    28: aaaa
    33: srv
    255: any

  class_enum:
    1: in
    3: ch
    4: hs
    255: any