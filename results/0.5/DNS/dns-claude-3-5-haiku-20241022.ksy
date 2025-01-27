meta:
  id: dns
  title: DNS Protocol
  endian: be
seq:
  - id: transaction_id
    type: u2
    doc: Unique identifier for the DNS query/response
  - id: flags
    type: flags_struct
  - id: questions_count
    type: u2
    doc: Number of questions in the query
  - id: answer_count
    type: u2
    doc: Number of answer records
  - id: authority_count
    type: u2
    doc: Number of authority records
  - id: additional_count
    type: u2
    doc: Number of additional records
  - id: queries
    type: query
    repeat: expr
    repeat-expr: questions_count
  - id: answers
    type: resource_record
    repeat: expr
    repeat-expr: answer_count
  - id: authorities
    type: resource_record
    repeat: expr
    repeat-expr: authority_count
  - id: additionals
    type: resource_record
    repeat: expr
    repeat-expr: additional_count

types:
  flags_struct:
    seq:
      - id: qr
        type: b1
        doc: Query (0) or Response (1)
      - id: opcode
        type: b4
        doc: Type of query
      - id: aa
        type: b1
        doc: Authoritative Answer
      - id: tc
        type: b1
        doc: Truncated
      - id: rd
        type: b1
        doc: Recursion Desired
      - id: ra
        type: b1
        doc: Recursion Available
      - id: z
        type: b3
        doc: Reserved
      - id: rcode
        type: b4
        doc: Response Code

  query:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: query_type
      - id: query_class
        type: u2
        enum: query_class

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: query_type
      - id: record_class
        type: u2
        enum: query_class
      - id: ttl
        type: u4
        doc: Time to live in seconds
      - id: rdlength
        type: u2
        doc: Length of record data
      - id: rdata
        size: rdlength
        type: 
          switch-on: type
          cases:
            'query_type::a': ipv4_address
            'query_type::aaaa': ipv6_address
            'query_type::cname': domain_name
            'query_type::mx': mx_record
            _: raw_data

  domain_name:
    seq:
      - id: name_parts
        type: name_part
        repeat: until
        repeat-until: _.length == 0

  name_part:
    seq:
      - id: length
        type: u1
      - id: part
        type: str
        size: length
        encoding: ascii
        if: length > 0

  ipv4_address:
    seq:
      - id: address
        type: u1
        repeat: expr
        repeat-expr: 4

  ipv6_address:
    seq:
      - id: address
        type: u2
        repeat: expr
        repeat-expr: 8

  mx_record:
    seq:
      - id: preference
        type: u2
      - id: exchange
        type: domain_name

  raw_data:
    seq:
      - id: data
        type: u1
        repeat: eos

enums:
  query_type:
    1: a
    2: ns
    5: cname
    6: soa
    12: ptr
    15: mx
    28: aaaa
    252: axfr
    255: any

  query_class:
    1: in
    3: ch
    4: hs