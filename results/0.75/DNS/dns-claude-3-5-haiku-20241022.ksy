meta:
  id: dns_packet
  endian: be
seq:
  - id: header
    type: header
  - id: questions
    type: question
    repeat: expr
    repeat-expr: header.qdcount
  - id: answers
    type: resource_record
    repeat: expr
    repeat-expr: header.ancount
  - id: authorities
    type: resource_record
    repeat: expr
    repeat-expr: header.nscount
  - id: additionals
    type: resource_record
    repeat: expr
    repeat-expr: header.arcount

types:
  header:
    seq:
      - id: transaction_id
        type: u2
      - id: flags
        type: flags
      - id: qdcount
        type: u2
      - id: ancount
        type: u2
      - id: nscount
        type: u2
      - id: arcount
        type: u2

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
            1: a_record     # A record
            2: ns_record    # Name Server
            5: cname_record # Canonical Name
            6: soa_record   # Start of Authority
            12: ptr_record  # Pointer
            15: mx_record   # Mail Exchange
            28: aaaa_record # AAAA record

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
            if: length != 0

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