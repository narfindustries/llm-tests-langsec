meta:
  id: dns_packet
  title: DNS (Domain Name System) packet
  file-extension: dns
  xref:
    rfc: 1035
  license: CC0-1.0
  endian: be

seq:
  - id: transaction_id
    type: u2
  - id: flags
    type: packet_flags
  - id: qdcount
    type: u2
    doc: Number of questions
  - id: ancount
    type: u2
    doc: Number of answers
  - id: nscount
    type: u2
    doc: Number of authority records
  - id: arcount
    type: u2
    doc: Number of additional records
  - id: questions
    type: question
    repeat: expr
    repeat-expr: qdcount
  - id: answers
    type: answer
    repeat: expr
    repeat-expr: ancount
  - id: authorities
    type: answer
    repeat: expr
    repeat-expr: nscount
  - id: additionals
    type: answer
    repeat: expr
    repeat-expr: arcount

types:
  packet_flags:
    seq:
      - id: qr
        type: b1
        doc: Query (0) or Response (1)
      - id: opcode
        type: b4
        doc: Operation code
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
        doc: Response code

  question:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type_type
      - id: class
        type: u2
        enum: class_type

  answer:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type_type
      - id: class
        type: u2
        enum: class_type
      - id: ttl
        type: u4
        doc: Time to live (seconds)
      - id: rdlength
        type: u2
        doc: Length of rdata field
      - id: rdata
        size: rdlength
        type:
          switch-on: type
          cases:
            'type_type::a': ipv4_addr
            'type_type::aaaa': ipv6_addr
            'type_type::cname': domain_name
            'type_type::mx': mx_record
            'type_type::ns': domain_name
            'type_type::ptr': domain_name
            'type_type::soa': soa_record
            'type_type::txt': txt_record
            _: raw_bytes

  domain_name:
    seq:
      - id: labels
        type: label
        repeat: until
        repeat-until: _.length == 0 or _.is_pointer

  label:
    seq:
      - id: length
        type: u1
      - id: pointer
        type: u1
        if: length & 0xc0 == 0xc0
      - id: name
        type: str
        encoding: ASCII
        size: length
        if: length & 0xc0 != 0xc0
    instances:
      is_pointer:
        value: length & 0xc0 == 0xc0

  ipv4_addr:
    seq:
      - id: ip
        size: 4

  ipv6_addr:
    seq:
      - id: ip
        size: 16

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

  txt_record:
    seq:
      - id: texts
        type: txt_string
        repeat: eos

  txt_string:
    seq:
      - id: length
        type: u1
      - id: text
        type: str
        encoding: ASCII
        size: length

  raw_bytes:
    seq:
      - id: data
        size-eos: true

enums:
  type_type:
    1: a
    2: ns
    5: cname
    6: soa
    12: ptr
    15: mx
    16: txt
    28: aaaa

  class_type:
    1: in
    2: cs
    3: ch
    4: hs