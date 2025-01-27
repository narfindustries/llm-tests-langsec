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
  packet_flags:
    seq:
      - id: qr
        type: b1
        doc: Query (0) or Response (1)
      - id: opcode
        type: b4
        enum: opcode
      - id: aa
        type: b1
        doc: Authoritative Answer
      - id: tc
        type: b1
        doc: Truncation
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
        enum: rcode

  question:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type
      - id: qclass
        type: u2
        enum: class

  resource_record:
    seq:
      - id: name
        type: domain_name
      - id: type
        type: u2
        enum: type
      - id: class
        type: u2
        enum: class
      - id: ttl
        type: u4
      - id: rdlength
        type: u2
      - id: rdata
        size: rdlength
        type:
          switch-on: type
          cases:
            'type::a': rdata_a
            'type::ns': domain_name
            'type::cname': domain_name
            'type::soa': rdata_soa
            'type::ptr': domain_name
            'type::mx': rdata_mx
            'type::txt': rdata_txt
            'type::aaaa': rdata_aaaa
            _: raw_rdata

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
        if: 'length & 0xc0 == 0xc0'
      - id: name
        type: str
        size: length & 0x3f
        encoding: ASCII
        if: 'length & 0xc0 == 0'
    instances:
      is_pointer:
        value: length & 0xc0 == 0xc0

  rdata_a:
    seq:
      - id: ip_addr
        size: 4

  rdata_aaaa:
    seq:
      - id: ip_addr
        size: 16

  rdata_mx:
    seq:
      - id: preference
        type: u2
      - id: exchange
        type: domain_name

  rdata_soa:
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

  rdata_txt:
    seq:
      - id: texts
        type: txt_entry
        repeat: eos

  txt_entry:
    seq:
      - id: length
        type: u1
      - id: text
        type: str
        size: length
        encoding: ASCII

  raw_rdata:
    seq:
      - id: data
        size-eos: true

enums:
  type:
    1: a
    2: ns
    5: cname
    6: soa
    12: ptr
    15: mx
    16: txt
    28: aaaa

  class:
    1: in
    3: ch
    4: hs

  opcode:
    0: query
    1: iquery
    2: status
    4: notify
    5: update

  rcode:
    0: no_error
    1: format_error
    2: server_failure
    3: name_error
    4: not_implemented
    5: refused