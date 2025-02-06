meta:
  id: dns
  endian: be
seq:
  - id: transaction_id
    type: u2
  - id: flags
    type: flags
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
  flags:
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
        type: b1
      - id: rcode
        type: b4

  domain_name:
    seq:
      - id: name_parts
        type: name_part
        repeat: until
        repeat-until: _.length == 0
    types:
      name_part:
        seq:
          - id: length
            type: u1
          - id: part
            type: str
            size: length
            encoding: ASCII
            if: length > 0

  question:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
      - id: class
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
        size: rdlength
        type:
          switch-on: type
          cases:
            1: a_record
            2: ns_record
            5: cname_record
            6: soa_record
            12: ptr_record
            15: mx_record
            28: aaaa_record

  a_record:
    seq:
      - id: address
        type: u1
        repeat: expr
        repeat-expr: 4

  aaaa_record:
    seq:
      - id: address
        type: u1
        repeat: expr
        repeat-expr: 16

  ns_record:
    seq:
      - id: name
        type: domain_name

  cname_record:
    seq:
      - id: name
        type: domain_name

  ptr_record:
    seq:
      - id: name
        type: domain_name

  mx_record:
    seq:
      - id: priority
        type: u2
      - id: name
        type: domain_name

  soa_record:
    seq:
      - id: primary_ns
        type: domain_name
      - id: responsible_mailbox
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