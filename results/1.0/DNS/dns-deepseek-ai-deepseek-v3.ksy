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
        type: name
      - id: qtype
        type: u2
      - id: qclass
        type: u2
  resource_record:
    seq:
      - id: name
        type: name
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
  name:
    seq:
      - id: labels
        type: label
        repeat: until
        repeat-until: _.length == 0
    instances:
      full_name:
        value: '"".join(labels)'
  label:
    seq:
      - id: length
        type: u1
      - id: name
        size: length
        if: length != 0
    instances:
      is_pointer:
        value: (length & 0xC0) == 0xC0
      pointer_offset:
        value: ((length & 0x3F) << 8) | _io.read_u1()
        if: is_pointer
      label_str:
        value: name.decode('ascii') if is_pointer == false else null