meta:
  id: dns
  endian: be
seq:
  - id: transaction_id
    type: u2
  - id: flags
    type: flags_struct
  - id: question_count
    type: u2
  - id: answer_count
    type: u2
  - id: authority_count
    type: u2
  - id: additional_count
    type: u2
  - id: questions
    type: question
    repeat: expr
    repeat-expr: question_count
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
        doc: Operation type
      - id: aa
        type: b1
        doc: Authoritative Answer
      - id: tc
        type: b1
        doc: Truncation
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
        type: rdata
        size: rdlength

  domain_name:
    seq:
      - id: name_parts
        type: name_part
        repeat: until
        repeat-until: _.length == 0 or _.pointer != 0

  name_part:
    seq:
      - id: length
        type: u1
      - id: pointer
        type: b2
        if: length >= 0xc0
      - id: part
        type: str
        size: length
        if: length < 0xc0 and length > 0
        encoding: ascii

  rdata:
    seq:
      - id: content
        type: byte_array
        size-eos: true

  byte_array:
    seq:
      - id: data
        type: u1
        repeat: eos

enums:
  record_type:
    1: a
    2: ns
    5: cname
    6: soa
    12: ptr
    15: mx
    28: aaaa

  class_type:
    1: in
    3: ch
    4: hs