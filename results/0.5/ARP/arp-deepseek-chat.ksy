meta:
  id: arp
  title: Address Resolution Protocol (ARP)
  license: CC0-1.0
  endian: be
seq:
  - id: hardware_type
    type: u2
    enum: arp_hardware_type
  - id: protocol_type
    type: u2
    enum: ethertype
  - id: hardware_size
    type: u1
  - id: protocol_size
    type: u1
  - id: opcode
    type: u2
    enum: arp_opcode
  - id: sender_hardware_addr
    type: hardware_addr
    size: hardware_size
  - id: sender_protocol_addr
    type: protocol_addr
    size: protocol_size
  - id: target_hardware_addr
    type: hardware_addr
    size: hardware_size
  - id: target_protocol_addr
    type: protocol_addr
    size: protocol_size
types:
  hardware_addr:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _root.hardware_size
  protocol_addr:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _root.protocol_size
enums:
  arp_hardware_type:
    1: ethernet
    6: ieee802
    7: arcnet
    15: frame_relay
    16: atm
    17: hdlc
    18: fibre_channel
    19: atm2
    20: serial_line
  ethertype:
    2048: ipv4
    2054: arp
    32821: rarp
    33079: ipv6
  arp_opcode:
    1: request
    2: reply
    3: request_reverse
    4: reply_reverse
    5: drarp_request
    6: drarp_reply
    7: drarp_error
    8: inarp_request
    9: inarp_reply
    10: arp_nak
    11: mars_request
    12: mars_multi
    13: mars_mserv
    14: mars_join
    15: mars_leave
    16: mars_nak
    17: mars_unsupported
    18: mars_serval
    19: mars_serval_reply