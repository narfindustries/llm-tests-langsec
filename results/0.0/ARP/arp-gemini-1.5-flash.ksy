$schema: https://kaitai.io/schema/draft-04
type: struct
endian: big
fields:
  - id: htype
    type: u2
  - id: ptype
    type: u2
  - id: hlen
    type: u1
  - id: plen
    type: u1
  - id: opcode
    type: u2
  - id: sha
    type: seq
    elements:
      - type: u1
    length: hlen
  - id: spa
    type: seq
    elements:
      - type: u1
    length: plen
  - id: tha
    type: seq
    elements:
      - type: u1
    length: hlen
  - id: tpa
    type: seq
    elements:
      - type: u1
    length: plen

