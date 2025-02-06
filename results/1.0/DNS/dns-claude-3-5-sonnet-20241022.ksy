meta:
  id: dns_packet
  title: DNS packet
  file-extension: dns
  endian: be

seq:
  - id: header
    type: packet_header
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
  packet_header:
    seq:
      - id: id
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
        value: (flags >> 15) & 1
      opcode:
        value: (flags >> 11) & 0xf
      aa:
        value: (flags >> 10) & 1
      tc:
        value: (flags >> 9) & 1
      rd:
        value: (flags >> 8) & 1
      ra:
        value: (flags >> 7) & 1
      z:
        value: (flags >> 4) & 7
      rcode:
        value: flags & 0xf

  query:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type_type
      - id: class
        type: u2
        enum: class_type

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type_type
      - id: class
        type: u2
        enum: class_type
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        size: rdlength
        type:
          switch-on: type
          cases:
            'type_type::a': rdata_a
            'type_type::ns': domain_name
            'type_type::cname': domain_name
            'type_type::soa': rdata_soa
            'type_type::ptr': domain_name
            'type_type::mx': rdata_mx
            'type_type::aaaa': rdata_aaaa
            _: raw_rdata

  domain_name:
    seq:
      - id: labels
        type: label
        repeat: until
        repeat-until: _.length == 0 or _.length >= 0xc0
    types:
      label:
        seq:
          - id: length
            type: u1
          - id: pointer
            type: u1
            if: length >= 0xc0
          - id: data
            type: str
            size: length
            encoding: ascii
            if: length > 0 and length < 0xc0

  rdata_a:
    seq:
      - id: ip
        size: 4

  rdata_aaaa:
    seq:
      - id: ip
        size: 16

  rdata_mx:
    seq:
      - id: preference
        type: u2
      - id: exchange
        type: domain_name

  rdata_soa:
    seq:
      - id: mname
        type: domain_name
      - id: rname
        type: domain_name
      - id: serial
        type: u4
      - id: refresh
        type: u4
      - id: retry
        type: u4
      - id: expire
        type: u4
      - id: minimum
        type: u4

  raw_rdata:
    seq:
      - id: data
        size-eos: true

enums:
  type_type:
    1: a
    2: ns
    5: cname
    6: soa
    12: ptr
    13: hinfo
    15: mx
    28: aaaa
    252: axfr
    255: any

  class_type:
    1: in
    2: cs
    3: ch
    4: hs
    255: any