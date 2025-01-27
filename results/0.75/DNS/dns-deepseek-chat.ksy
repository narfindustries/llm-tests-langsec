meta:
  id: dns
  title: DNS (Domain Name System) packet format
  license: MIT
  endian: be
  file-extension: dns
  ks-version: 0.9

seq:
  - id: transaction_id
    type: u2
    doc: Transaction ID
  - id: flags
    type: u2
    doc: DNS flags
  - id: questions
    type: u2
    doc: Number of questions
  - id: answer_rrs
    type: u2
    doc: Number of answer resource records
  - id: authority_rrs
    type: u2
    doc: Number of authority resource records
  - id: additional_rrs
    type: u2
    doc: Number of additional resource records
  - id: queries
    type: query
    repeat: expr
    repeat-expr: questions
    doc: Queries section
  - id: answers
    type: resource_record
    repeat: expr
    repeat-expr: answer_rrs
    doc: Answers section
  - id: authorities
    type: resource_record
    repeat: expr
    repeat-expr: authority_rrs
    doc: Authority section
  - id: additionals
    type: resource_record
    repeat: expr
    repeat-expr: additional_rrs
    doc: Additional section

types:
  query:
    seq:
      - id: name
        type: dns_name
        doc: Domain name
      - id: type
        type: u2
        doc: Query type
      - id: query_class
        type: u2
        doc: Query class

  resource_record:
    seq:
      - id: name
        type: dns_name
        doc: Domain name
      - id: type
        type: u2
        doc: Resource record type
      - id: rr_class
        type: u2
        doc: Resource record class
      - id: ttl
        type: u4
        doc: Time to live
      - id: rdlength
        type: u2
        doc: Length of the resource data
      - id: rdata
        size: rdlength
        doc: Resource data

  dns_name:
    seq:
      - id: labels
        type: label
        repeat: until
        repeat-until: _.length == 0
        doc: Labels in the domain name
    instances:
      name:
        value: '".".join(labels)'

  label:
    seq:
      - id: length
        type: u1
        doc: Length of the label
      - id: data
        size: length
        type: str
        encoding: UTF-8
        doc: Label data