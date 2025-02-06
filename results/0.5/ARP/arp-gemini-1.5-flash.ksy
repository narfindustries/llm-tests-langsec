meta:
  id: arp_packet
types:
  arp_packet:
    seq:
      - id: htype
        type: u2be
      - id: ptype
        type: u2be
      - id: hlen
        type: u1
      - id: plen
        type: u1
      - id: opcode
        type: u2be
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

