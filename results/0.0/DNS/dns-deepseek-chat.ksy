meta:
  id: dns_deepseek_chat
  title: DNS DeepSeek Chat Protocol
  license: MIT
  ks-version: 0.9
  endian: be

seq:
  - id: transaction_id
    type: u2
    doc: "Transaction ID"
  - id: flags
    type: u2
    doc: "Flags"
  - id: questions
    type: u2
    doc: "Number of questions"
  - id: answer_rrs
    type: u2
    doc: "Number of answer resource records"
  - id: authority_rrs
    type: u2
    doc: "Number of authority resource records"
  - id: additional_rrs
    type: u2
    doc: "Number of additional resource records"
  - id: queries
    type: query
    repeat: expr
    repeat-expr: questions
    doc: "Queries section"
  - id: answers
    type: resource_record
    repeat: expr
    repeat-expr: answer_rrs
    doc: "Answers section"
  - id: authorities
    type: resource_record
    repeat: expr
    repeat-expr: authority_rrs
    doc: "Authorities section"
  - id: additionals
    type: resource_record
    repeat: expr
    repeat-expr: additional_rrs
    doc: "Additional records section"

types:
  query:
    seq:
      - id: name
        type: str
        encoding: UTF-8
        size-eos: true
        doc: "Domain name"
      - id: type
        type: u2
        doc: "Type of query"
      - id: qclass
        type: u2
        doc: "Class of query"

  resource_record:
    seq:
      - id: name
        type: str
        encoding: UTF-8
        size-eos: true
        doc: "Domain name"
      - id: type
        type: u2
        doc: "Type of resource record"
      - id: rclass
        type: u2
        doc: "Class of resource record"
      - id: ttl
        type: u4
        doc: "Time to live"
      - id: rdlength
        type: u2
        doc: "Length of resource data"
      - id: rdata
        size: rdlength
        type:
          switch-on: type
          cases:
            1: ipv4_address
            28: ipv6_address
            _: raw_data
        doc: "Resource data"

  ipv4_address:
    seq:
      - id: address
        type: u4
        doc: "IPv4 address"

  ipv6_address:
    seq:
      - id: address
        type: u16
        doc: "IPv6 address"

  raw_data:
    seq:
      - id: data
        type: str
        encoding: UTF-8
        size-eos: true
        doc: "Raw data"