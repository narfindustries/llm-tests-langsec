meta:
  id: arp
  title: Address Resolution Protocol
  file-extension: arp
  license: CC0-1.0
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
  - id: operation
    type: u2
    enum: operations
  - id: sender_hardware_address
    size: hardware_size
  - id: sender_protocol_address
    size: protocol_size
  - id: target_hardware_address
    size: hardware_size
  - id: target_protocol_address
    size: protocol_size

enums:
  hardware_types:
    1: ethernet
    6: ieee_802_networks
    7: arcnet
    15: frame_relay
    16: atm
    17: hdlc
    18: fibre_channel
    19: atm2
    20: serial_line

  operations:
    1: request
    2: reply
    3: rarp_request
    4: rarp_reply
    8: inarp_request
    9: inarp_reply
    10: arp_nak