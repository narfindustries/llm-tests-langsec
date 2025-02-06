meta:
  id: dns
  file-extension: dns
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
        type: flags
      - id: qdcount
        type: u2
      - id: ancount
        type: u2
      - id: nscount
        type: u2
      - id: arcount
        type: u2
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
            1: ipv4_address
            28: ipv6_address
            5: domain_name
            2: domain_name
            12: domain_name
            15: mx_record
            16: txt_record
            6: soa_record
            _: raw_data
  domain_name:
    seq:
      - id: labels
        type: label
        repeat: until
        repeat-until: _.length == 0
    instances:
      name:
        value: labels.join('.')
  label:
    seq:
      - id: length
        type: u1
      - id: data
        size: length
        type: str
        encoding: ASCII
  ipv4_address:
    seq:
      - id: address
        type: u4
  ipv6_address:
    seq:
      - id: address
        type: u1
        repeat: expr
        repeat-expr: 16
  mx_record:
    seq:
      - id: preference
        type: u2
      - id: exchange
        type: domain_name
  txt_record:
    seq:
      - id: texts
        type: txt_segment
        repeat: until
        repeat-until: _.length == 0
  txt_segment:
    seq:
      - id: length
        type: u1
      - id: text
        size: length
        type: str
        encoding: ASCII
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
  raw_data:
    seq:
      - id: data
        size-eos: true
        type: str
        encoding: ASCII