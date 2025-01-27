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
        type: dns_name
      - id: type
        type: u2
      - id: query_class
        type: u2
  resource_record:
    seq:
      - id: name
        type: dns_name
      - id: type
        type: u2
      - id: rr_class
        type: u2
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        size: rdlength
  dns_name:
    seq:
      - id: labels
        type: label
        repeat: until
        repeat-until: _.length == 0
    instances:
      name:
        value: '".".join(labels[:-1])'
  label:
    seq:
      - id: length
        type: u1
      - id: value
        type: str
        size: length
        encoding: UTF-8