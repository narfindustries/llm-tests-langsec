meta:
  id: dns_packet
  title: DNS (Domain Name System) packet
  file-extension: dns
  xref:
    rfc: 1035
  license: CC0-1.0
  endian: be

seq:
  - id: transaction_id
    type: u2
  - id: flags
    type: packet_flags
  - id: qdcount
    type: u2
    doc: Number of questions
  - id: ancount
    type: u2
    doc: Number of answers
  - id: nscount
    type: u2
    doc: Number of authority records
  - id: arcount
    type: u2
    doc: Number of additional records
  - id: questions
    type: question
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
  packet_flags:
    seq:
      - id: flag_response
        type: b1
      - id: opcode
        type: b4
      - id: flag_authoritative
        type: b1
      - id: flag_truncated
        type: b1
      - id: flag_recursion_desired
        type: b1
      - id: flag_recursion_available
        type: b1
      - id: reserved
        type: b3
      - id: flag_authentic_data
        type: b1
      - id: flag_checking_disabled
        type: b1
      - id: response_code
        type: b4

  question:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: record_type
      - id: class
        type: u2
        enum: class_type

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: record_type
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
            'record_type::a': rdata_a
            'record_type::ns': domain_name
            'record_type::cname': domain_name
            'record_type::soa': rdata_soa
            'record_type::ptr': domain_name
            'record_type::mx': rdata_mx
            'record_type::txt': rdata_txt
            'record_type::aaaa': rdata_aaaa
            'record_type::srv': rdata_srv
            _: raw_rdata

  domain_name:
    seq:
      - id: name
        type: label
        repeat: until
        repeat-until: _.length == 0
    types:
      label:
        seq:
          - id: length
            type: u1
          - id: data
            type: str
            size: length
            encoding: ascii
            if: length < 0xc0
          - id: pointer
            type: u1
            if: length >= 0xc0

  rdata_a:
    seq:
      - id: ip_addr
        size: 4

  rdata_aaaa:
    seq:
      - id: ip_addr
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

  rdata_srv:
    seq:
      - id: priority
        type: u2
      - id: weight
        type: u2
      - id: port
        type: u2
      - id: target
        type: domain_name

  rdata_txt:
    seq:
      - id: texts
        type: txt_record
        repeat: eos
    types:
      txt_record:
        seq:
          - id: len
            type: u1
          - id: text
            type: str
            size: len
            encoding: ascii

  raw_rdata:
    seq:
      - id: data
        size-eos: true

enums:
  record_type:
    1: a
    2: ns
    5: cname
    6: soa
    12: ptr
    15: mx
    16: txt
    28: aaaa
    33: srv

  class_type:
    1: in
    3: ch
    4: hs