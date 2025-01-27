meta:
  id: dns_packet
  title: DNS (Domain Name System) Protocol
  endian: be
  license: CC0-1.0

seq:
  - id: transaction_id
    type: u2
  - id: flags
    type: u2
    enum: flags_type
  - id: qdcount
    type: u2
  - id: ancount
    type: u2
  - id: nscount
    type: u2
  - id: arcount
    type: u2
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
  query:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type_enum
      - id: query_class
        type: u2
        enum: class_enum

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type_enum
      - id: rr_class
        type: u2
        enum: class_enum
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        size: rdlength
        type:
          switch-on: type
          cases:
            'type_enum::a': address_record
            'type_enum::cname': cname_record
            'type_enum::mx': mx_record
            'type_enum::txt': txt_record

  domain_name:
    seq:
      - id: parts
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

  address_record:
    seq:
      - id: address
        type: u4

  cname_record:
    seq:
      - id: cname
        type: domain_name

  mx_record:
    seq:
      - id: preference
        type: u2
      - id: exchange
        type: domain_name

  txt_record:
    seq:
      - id: text
        type: str
        size-eos: true
        encoding: ASCII

enums:
  flags_type:
    0x0100: qr
    0x0200: opcode
    0x0400: aa
    0x0800: tc
    0x1000: rd
    0x2000: ra
    0x4000: z
    0x8000: ad
    0x0001: cd

  type_enum:
    1: a
    2: ns
    5: cname
    6: soa
    15: mx
    16: txt
    28: aaaa

  class_enum:
    1: in
    2: cs
    3: ch
    4: hs