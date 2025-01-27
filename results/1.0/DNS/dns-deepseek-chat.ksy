meta:
  id: dns
  title: DNS (Domain Name System) packet format
  license: MIT
  endian: be
  ks-version: 0.9
seq:
  - id: transaction_id
    type: u2
  - id: flags
    type: u2
  - id: questions
    type: u2
  - id: answer_rrs
    type: u2
  - id: authority_rrs
    type: u2
  - id: additional_rrs
    type: u2
  - id: queries
    type: query
    repeat: expr
    repeat-expr: questions
  - id: answers
    type: resource_record
    repeat: expr
    repeat-expr: answer_rrs
  - id: authorities
    type: resource_record
    repeat: expr
    repeat-expr: authority_rrs
  - id: additionals
    type: resource_record
    repeat: expr
    repeat-expr: additional_rrs
types:
  query:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
      - id: query_class
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
        type:
          switch-on: type
          cases:
            1: ipv4_address
            28: ipv6_address
            5: domain_name
            _: raw_data
  domain_name:
    seq:
      - id: labels
        type: label
        repeat: until
        repeat-until: _.length == 0
    instances:
      full_name:
        value: labels.join('.')
  label:
    seq:
      - id: length
        type: u1
      - id: name
        type: str
        size: length
  ipv4_address:
    seq:
      - id: address
        type: u4
  ipv6_address:
    seq:
      - id: address
        type: u16
  raw_data:
    seq:
      - id: data
        type: str
        size: rdlength