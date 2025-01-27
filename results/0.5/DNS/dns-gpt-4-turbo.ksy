meta:
  id: dns_packet
  title: DNS (Domain Name System) Protocol
  license: CC0-1.0
  endian: be

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

  domain_name:
    seq:
      - id: name_parts
        type: name_part
        repeat: until
        repeat-until: _.length == 0
    types:
      name_part:
        seq:
          - id: length
            type: u1
          - id: data
            type: str
            size: length
            encoding: ASCII

enums:
  flags_type:
    0x0100: query_response
    0x0800: auth_answer
    0x0400: truncation
    0x0200: recursion_desired
    0x0100: recursion_available
    0x0080: z_reserved
    0x0040: authenticated_data
    0x0020: checking_disabled

  type_enum:
    1: a
    2: ns
    5: cname
    6: soa
    15: mx
    28: aaaa

  class_enum:
    1: internet
    3: chaos
    4: hesiod
    254: none
    255: any