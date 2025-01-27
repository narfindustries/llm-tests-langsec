meta:
  id: dns_packet
  title: DNS Packet
  endian: be

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

  - id: queries
    type: query
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
        type: b3

      - id: rcode
        type: b4

  query:
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

  domain_name:
    seq:
      - id: name
        type: str
        encoding: ASCII
        size-eos: true