meta:
  id: dns
  title: Domain Name System (DNS) packet
  file-extension: dns
  license: MIT
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
        value: (flags >> 15) & 0x1
      opcode:
        value: (flags >> 11) & 0xF
      aa:
        value: (flags >> 10) & 0x1
      tc:
        value: (flags >> 9) & 0x1
      rd:
        value: (flags >> 8) & 0x1
      ra:
        value: (flags >> 7) & 0x1
      z:
        value: (flags >> 4) & 0x7
      rcode:
        value: flags & 0xF
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
            1: ipv4
            5: domain_name
            12: domain_name
            15: mx
            16: txt
            28: ipv6
  domain_name:
    seq:
      - id: labels
        type: label
        repeat: until
        repeat-until: _.length == 0
    instances:
      name:
        value: labels.map(&:value).join('.')
  label:
    seq:
      - id: length
        type: u1
      - id: value
        size: length
        type: str
        encoding: UTF-8
  ipv4:
    seq:
      - id: address
        type: u4
  ipv6:
    seq:
      - id: address
        type: u8
  mx:
    seq:
      - id: preference
        type: u2
      - id: exchange
        type: domain_name
  txt:
    seq:
      - id: text
        type: str
        encoding: UTF-8
        size-eos: true