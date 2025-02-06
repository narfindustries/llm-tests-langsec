meta:
  id: dns
  endian: be

seq:
  - id: header
    type: dns_header
  - id: questions
    type: dns_question
    repeat: expr
    repeat-expr: header.qdcount
  - id: answers
    type: dns_rr
    repeat: expr
    repeat-expr: header.ancount
  - id: authorities
    type: dns_rr
    repeat: expr
    repeat-expr: header.nscount
  - id: additionals
    type: dns_rr
    repeat: expr
    repeat-expr: header.arcount

types:
  dns_header:
    seq:
      - id: id
        type: u2
      - id: flags
        type: dns_flags
      - id: qdcount
        type: u2
      - id: ancount
        type: u2
      - id: nscount
        type: u2
      - id: arcount
        type: u2

  dns_flags:
    bits: 16
    seq:
      - id: qr
        type: u1
      - id: opcode
        type: u4
      - id: aa
        type: u1
      - id: tc
        type: u1
      - id: rd
        type: u1
      - id: ra
        type: u1
      - id: z
        type: u3
      - id: rcode
        type: u4

  dns_question:
    seq:
      - id: name
        type: dns_name
      - id: type
        type: u2
      - id: class
        type: u2

  dns_name:
    seq:
      - id: labels
        type: dns_label
        repeat: until
        until: label == 0
      - id: label
        type: u1

  dns_label:
    seq:
      - id: len
        type: u1
      - id: label
        type: str
        size: len

  dns_rr:
    seq:
      - id: name
        type: dns_name
      - id: type
        type: u2
      - id: class
        type: u2
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        type: switch
        cases:
          - when: type == 1
            type: ip_addr
          - when: type == 2
            type: dns_name
          - when: type == 5
            type: dns_name
          - when: type == 6
            type: soa
          - when: type == 12
            type: dns_name
          - when: type == 15
            type: mx
          - when: type == 16
            type: txt
          - else:
            type: bytes
            size: rdlength

  ip_addr:
    type: bytes
    size: 4

  soa:
    seq:
      - id: mname
        type: dns_name
      - id: rname
        type: dns_name
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

  mx:
    seq:
      - id: preference
        type: u2
      - id: exchange
        type: dns_name

  txt:
    seq:
      - id: txt_data
        type: str
        size: rdlength