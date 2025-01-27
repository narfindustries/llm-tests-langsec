meta:
  id: arp
  title: Address Resolution Protocol (ARP)
  file-extension: arp
  endian: be
  license: CC0-1.0
  ks-version: 0.9

seq:
  - id: hardware_type
    type: u2
    enum: hw_type
  - id: protocol_type
    type: u2
  - id: hardware_size
    type: u1
  - id: protocol_size
    type: u1
  - id: opcode
    type: u2
    enum: operation
  - id: sender_hardware_addr
    size: hardware_size
  - id: sender_protocol_addr
    size: protocol_size
  - id: target_hardware_addr
    size: hardware_size
  - id: target_protocol_addr
    size: protocol_size

enums:
  hw_type:
    1: ethernet
    6: ieee_802
    7: arcnet
    15: frame_relay
    16: atm
    17: hdlc
    18: fibre_channel
    19: atm2
    20: serial_line
  operation:
    1: request
    2: reply
    3: rarp_request
    4: rarp_reply
    5: drarp_request
    6: drarp_reply
    7: drarp_error
    8: inarp_request
    9: inarp_reply