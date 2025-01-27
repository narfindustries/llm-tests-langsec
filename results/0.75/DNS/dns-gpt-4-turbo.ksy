meta:
  id: dns_packet
  title: DNS (Domain Name System) Protocol
  file-extension: dns
  endian: be
  license: CC0-1.0
  ks-version: 0.9

doc: |
  DNS is a protocol used for converting human-readable domain names
  into machine-readable IP addresses.

seq:
  - id: transaction_id
    type: u2
    doc: Unique identifier for this query/response pair.

  - id: flags
    type: u2
    doc: |
      Flags control the behavior of the DNS query and response.
      This field is further broken down into various flags.

  - id: qdcount
    type: u2
    doc: Number of questions in the Question section.

  - id: ancount
    type: u2
    doc: Number of resource records in the Answer section.

  - id: nscount
    type: u2
    doc: Number of name server resource records in the Authority records section.

  - id: arcount
    type: u2
    doc: Number of resource records in the Additional records section.

  - id: questions
    type: query
    repeat: expr
    repeat-expr: qdcount
    doc: List of questions.

  - id: answers
    type: resource_record
    repeat: expr
    repeat-expr: ancount
    doc: List of answers.

  - id: authorities
    type: resource_record
    repeat: expr
    repeat-expr: nscount
    doc: List of authority records.

  - id: additionals
    type: resource_record
    repeat: expr
    repeat-expr: arcount
    doc: List of additional records.

types:
  query:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: query_type
      - id: query_class
        type: u2
        enum: class_type

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: query_type
      - id: rr_class
        type: u2
        enum: class_type
      - id: ttl
        type: u4
        doc: Time to live in seconds.
      - id: rdlength
        type: u2
        doc: Length of the RDATA field.
      - id: rdata
        size: rdlength

  domain_name:
    seq:
      - id: parts
        type: label
        repeat: eos

    types:
      label:
        seq:
          - id: length
            type: u1
          - id: body
            type: str
            size: length
            encoding: ASCII

enums:
  query_type:
    1: a
    2: ns
    3: md
    4: mf
    5: cname
    6: soa
    7: mb
    8: mg
    9: mr
    10: null
    11: wks
    12: ptr
    13: hinfo
    14: minfo
    15: mx
    16: txt
    28: aaaa
    33: srv
    255: any

  class_type:
    1: inet
    2: cs
    3: ch
    4: hs
    254: none
    255: any