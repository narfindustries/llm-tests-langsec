meta:
  id: dns
  file-extension: dns
  endian: be

seq:
  - id: transaction_id
    type: u2
  - id: flags
    type: flags_struct
  - id: qdcount
    type: u2
  - id: ancount
    type: u2
  - id: nscount
    type: u2
  - id: arcount
    type: u2
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
  - id: additional
    type: resource_record
    repeat: expr
    repeat-expr: arcount

types:
  flags_struct:
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

  domain_name:
    seq:
      - id: parts
        type: domain_part
        repeat: until
        repeat-until: _.length == 0
    types:
      domain_part:
        seq:
          - id: length
            type: u1
          - id: part
            type: str
            size: length
            encoding: ascii
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
        type: s4
      - id: rdlength
        type: u2
      - id: rdata
        type:
          switch-on: type
          cases:
            1: a_record     # A record
            2: ns_record    # Name Server
            5: cname_record # Canonical Name
            6: soa_record   # Start of Authority
            12: ptr_record  # Pointer
            15: mx_record   # Mail Exchange
            28: aaaa_record # AAAA record

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
      - id: preference
        type: u2
      - id: exchange
        type: domain_name

  soa_record:
    seq:
      - id: primary_ns
        type: domain_name
      - id: responsible_person
        type: domain_name
      - id: serial
        type: u4
      - id: refresh
        type: s4
      - id: retry
        type: s4
      - id: expire
        type: s4
      - id: minimum
        type: u4