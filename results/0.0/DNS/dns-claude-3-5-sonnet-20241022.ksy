meta:
  id: dns_packet
  title: DNS (Domain Name System) packet
  file-extension: dns
  endian: be
  bit-endian: be

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
    meta:
      bit-endian: be
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
    enums:
      opcode:
        0: query
        1: iquery
        2: status
      rcode:
        0: no_error
        1: format_error
        2: server_failure
        3: name_error
        4: not_implemented
        5: refused

  question:
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
            'type_type::txt': rdata_txt
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

  rdata_txt:
    seq:
      - id: strings
        type: txt_string
        repeat: eos
    types:
      txt_string:
        seq:
          - id: length
            type: u1
          - id: text
            type: str
            size: length
            encoding: ascii

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
    16: txt
    28: aaaa
    252: axfr
    255: any

  class_type:
    1: in
    2: cs
    3: ch
    4: hs
    255: any