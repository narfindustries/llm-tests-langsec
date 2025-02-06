meta:
  endian: le

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
        type: u2
      - id: qdcount
        type: u2
      - id: ancount
        type: u2
      - id: nscount
        type: u2
      - id: arcount
        type: u2

    instances:
      qr:
        value: flags & 0x8000
      opcode:
        value: (flags & 0x7800) >> 11
      aa:
        value: flags & 0x0400
      tc:
        value: flags & 0x0200
      rd:
        value: flags & 0x0100
      ra:
        value: flags & 0x0080
      z:
        value: (flags & 0x0070) >> 4
      rcode:
        value: flags & 0x000f

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
        until: labels.length == 0

  dns_label:
    seq:
      - id: length
        type: u1
      - id: text
        type: str
        size: length

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
        type:
          switch-on: type
          cases:
            1: dns_a
            2: dns_ns
            5: dns_cname
            6: dns_soa
            12: dns_ptr
            15: dns_mx
            16: dns_txt
            else: dns_rdata_unknown

  dns_a:
    seq:
      - id: address
        type: u4

  dns_ns:
    seq:
      - id: nsdname
        type: dns_name

  dns_cname:
    seq:
      - id: cname
        type: dns_name

  dns_soa:
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

  dns_ptr:
    seq:
      - id: ptrdname
        type: dns_name

  dns_mx:
    seq:
      - id: preference
        type: u2
      - id: exchange
        type: dns_name

  dns_txt:
    seq:
      - id: txt
        type: str
        size: rdlength

  dns_rdata_unknown:
    seq:
      - id: rdata
        type: bytes
        size: rdlength