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
  - id: op
    type: u2
  - id: sha
    type: bytes
    size: hlen
  - id: spa
    type: bytes
    size: plen
  - id: tha
    type: bytes
    size: hlen
  - id: tpa
    type: bytes
    size: plen
