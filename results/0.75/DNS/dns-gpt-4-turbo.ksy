meta:
  id: dns_packet
  title: DNS Packet
  endian: be
doc: |
  DNS (Domain Name System) is a hierarchical decentralized naming system for computers,
  services, or other resources connected to the Internet or a private network. It associates
  various information with domain names assigned to each of the participating entities.
  Most prominently, it translates more readily memorized domain names to the numerical
  IP addresses needed for locating and identifying computer services and devices with
  the underlying network protocols. By providing a worldwide keyword-based redirection
  service, the Domain Name System is an essential component of the functionality
  of the Internet. This Kaitai Struct specification describes the structure of DNS packets.
seq:
  - id: header
    type: header
  - id: queries
    type: query
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
        enum: qr_type
      opcode:
        value: (flags >> 11) & 0b1111
        enum: opcode_type
      aa:
        value: (flags >> 10) & 1
      tc:
        value: (flags >> 9) & 1
      rd:
        value: (flags >> 8) & 1
      ra:
        value: (flags >> 7) & 1
      z:
        value: (flags >> 4) & 0b111
      rcode:
        value: flags & 0b1111
        enum: rcode_type
  query:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type_type
      - id: class
        type: u2
        enum: class_type
  resource_record:
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
      - id: rdlength
        type: u2
      - id: rdata
        size: rdlength
  domain_name:
    seq:
      - id: parts
        type: label
        repeat: eos
  label:
    seq:
      - id: len
        type: u1
        valid:
          min: 1
      - id: name
        type: str
        size: len
        encoding: ASCII
enums:
  qr_type:
    0: query
    1: response
  opcode_type:
    0: query
    1: iquery
    2: status
    3: reserved
    4: notify
    5: update
  rcode_type:
    0: no_error
    1: format_error
    2: server_failure
    3: name_error
    4: not_implemented
    5: refused
  type_type:
    1: a
    2: ns
    5: cname
    6: soa
    12: ptr
    15: mx
    16: txt
    28: aaaa
    33: srv
    255: any
  class_type:
    1: in
    3: ch
    4: hs
    255: any