meta:
  id: dns_message
  title: DNS Message
  endian: be
seq:
  - id: transaction_id
    type: u2
  - id: flags
    type: dns_flags
  - id: qdcount
    type: u2
    doc: Number of questions
  - id: ancount
    type: u2
    doc: Number of answer resource records
  - id: nscount
    type: u2
    doc: Number of authority resource records
  - id: arcount
    type: u2
    doc: Number of additional resource records
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
  - id: additionals
    type: resource_record
    repeat: expr
    repeat-expr: arcount

types:
  dns_flags:
    seq:
      - id: flags_raw
        type: u2
    instances:
      qr:
        value: (flags_raw >> 15) & 1
      opcode:
        value: (flags_raw >> 11) & 0xF
      aa:
        value: (flags_raw >> 10) & 1
      tc:
        value: (flags_raw >> 9) & 1
      rd:
        value: (flags_raw >> 8) & 1
      ra:
        value: (flags_raw >> 7) & 1
      z_reserved:
        value: (flags_raw >> 4) & 0x7
      rcode:
        value: flags_raw & 0xF

  question:
    seq:
      - id: qname
        type: domain_name
      - id: qtype
        type: u2
      - id: qclass
        type: u2
  
  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
      - id: class_
        type: u2
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        size: rdlength
  
  domain_name:
    seq:
      - id: parts
        type: domain_label
        repeat: until
        repeat-until: _.is_root
    types:
      domain_label:
        seq:
          - id: length
            type: u1
          - id: name
            size: length
        instances:
          is_root:
            value: length == 0