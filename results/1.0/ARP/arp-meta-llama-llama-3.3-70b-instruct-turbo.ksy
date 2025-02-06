meta:
  id: arp
  endian: be
types:
  byte_array:
    type: seq
    repeat: expr
    seq:
      - id: byte
        type: u1
seq:
  - id: htype
    type: u2
  - id: ptype
    type: u2
  - id: hlen
    type: u1
  - id: plen
    type: u1
  - id: oper
    type: u2
  - id: sha
    type: byte_array
    repeat: expr
    seq:
      - id: byte
        type: u1
    repeat-expr: hlen
  - id: spa
    type: byte_array
    repeat: expr
    seq:
      - id: byte
        type: u1
    repeat-expr: plen
  - id: tha
    type: byte_array
    repeat: expr
    seq:
      - id: byte
        type: u1
    repeat-expr: hlen
  - id: tpa
    type: byte_array
    repeat: expr
    seq:
      - id: byte
        type: u1
    repeat-expr: plen