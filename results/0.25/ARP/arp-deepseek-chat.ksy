meta:
  id: arp
  title: Address Resolution Protocol
  license: CC0-1.0
  endian: be
seq:
  - id: hardware_type
    type: u2
    enum: hardware_types
  - id: protocol_type
    type: u2
    enum: ether_types
  - id: hardware_size
    type: u1
  - id: protocol_size
    type: u1
  - id: opcode
    type: u2
    enum: opcodes
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
  hardware_types:
    1: ethernet
    6: ieee802
    7: arcnet
    15: frame_relay
    16: atm
    17: hdlc
    18: fibre_channel
    19: atm2
    20: serial_line
  ether_types:
    2048: ipv4
    2054: arp
    32821: rarp
    33079: ipv6
  opcodes:
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