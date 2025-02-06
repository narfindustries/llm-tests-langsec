meta:
  endian: be
  id: arp
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