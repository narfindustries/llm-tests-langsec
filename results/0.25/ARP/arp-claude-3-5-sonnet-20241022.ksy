meta:
  id: arp
  title: Address Resolution Protocol
  file-extension: arp
  endian: be
seq:
  - id: hardware_type
    type: u2
    enum: hardware_types
  - id: protocol_type
    type: u2
  - id: hardware_size
    type: u1
  - id: protocol_size
    type: u1
  - id: opcode
    type: u2
    enum: opcodes
  - id: sender_mac
    size: hardware_size
  - id: sender_ip
    size: protocol_size
  - id: target_mac
    size: hardware_size
  - id: target_ip
    size: protocol_size
enums:
  hardware_types:
    1: ethernet
    6: ieee_802
    7: arcnet
    15: frame_relay
    16: atm
    17: hdlc
    18: fibre_channel
    19: atm2
    20: serial_line
  opcodes:
    1: request
    2: reply
    3: rarp_request
    4: rarp_reply
    5: drarp_request
    6: drarp_reply
    7: drarp_error
    8: inarp_request
    9: inarp_reply