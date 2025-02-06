meta:
  id: dns_packet
  title: DNS Packet (RFC 1035)
  endian: be

seq:
  - id: header
    type: header

  - id: queries
    type: query
    repeat: expr
    repeat-expr: header.qdcount

  - id: answers
    type: rr
    repeat: expr
    repeat-expr: header.ancount

  - id: authorities
    type: rr
    repeat: expr
    repeat-expr: header.nscount

  - id: additionals
    type: rr
    repeat: expr
    repeat-expr: header.arcount

types:
  header:
    seq:
      - id: transaction_id
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
        value: flags >> 15
      opcode:
        value: (flags >> 11) & 0x0f
      aa:
        value: (flags >> 10) & 0x01
      tc:
        value: (flags >> 9) & 0x01
      rd:
        value: (flags >> 8) & 0x01
      ra:
        value: (flags >> 7) & 0x01
      z:
        value: (flags >> 4) & 0x07
      rcode:
        value: flags & 0x0f

  query:
    seq:
      - id: qname
        type: domain_name
      - id: qtype
        type: u2
      - id: qclass
        type: u2

  rr:
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
        type: label
        repeat: until
        repeat-until: _.length == 0
    types:
      label:
        seq:
          - id: length
            type: u1
          - id: name
            type: str
            size: length
            encoding: ASCII
            if: length > 0
