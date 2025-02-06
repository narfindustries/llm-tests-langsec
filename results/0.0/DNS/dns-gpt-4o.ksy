meta:
  id: dns_packet
  title: DNS Packet
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
        doc: |
          QR (1 bit), Opcode (4 bits), AA (1 bit), TC (1 bit), RD (1 bit),
          RA (1 bit), Z (3 bits), RCode (4 bits)

      - id: qdcount
        type: u2
        doc: Number of entries in the question section

      - id: ancount
        type: u2
        doc: Number of resource records in the answer section

      - id: nscount
        type: u2
        doc: Number of name server resource records in the authority section

      - id: arcount
        type: u2
        doc: Number of resource records in the additional records section

  question:
    seq:
      - id: qname
        type: domain_name

      - id: qtype
        type: u2
        enum: qtype

      - id: qclass
        type: u2
        enum: qclass

  resource_record:
    seq:
      - id: name
        type: domain_name

      - id: type
        type: u2
        enum: qtype

      - id: class
        type: u2
        enum: qclass

      - id: ttl
        type: u4

      - id: rdlength
        type: u2

      - id: rdata
        size: rdlength

  domain_name:
    seq:
      - id: name
        type: strz
        encoding: ascii
        terminator: 0
        repeat: eos
        doc: Sequence of labels, each label is a length octet followed by that number of octets

enums:
  qtype:
    1: a
    2: ns
    5: cname
    6: soa
    12: ptr
    15: mx
    28: aaaa
    255: all

  qclass:
    1: in
    3: ch
    4: hs
    255: any