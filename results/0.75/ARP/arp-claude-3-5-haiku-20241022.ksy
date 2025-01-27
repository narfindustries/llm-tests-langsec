meta:
  id: arp
  title: Address Resolution Protocol (ARP)
  file-extension: arp
  endian: be

seq:
  - id: hardware_type
    type: u2
    enum: hardware_types
  - id: protocol_type
    type: u2
    enum: protocol_types
  - id: hardware_address_length
    type: u1
  - id: protocol_address_length
    type: u1
  - id: operation
    type: u2
    enum: arp_operation
  - id: sender_hardware_address
    type: hardware_address
    size: hardware_address_length
  - id: sender_protocol_address
    type: protocol_address
    size: protocol_address_length
  - id: target_hardware_address
    type: hardware_address
    size: hardware_address_length
  - id: target_protocol_address
    type: protocol_address
    size: protocol_address_length

types:
  hardware_address:
    seq:
      - id: address
        type: u1
        repeat: expr
        repeat-expr: _parent.hardware_address_length

  protocol_address:
    seq:
      - id: address
        type: u1
        repeat: expr
        repeat-expr: _parent.protocol_address_length

enums:
  hardware_types:
    1: ethernet
    6: ieee_802
    15: frame_relay
    16: atm
    17: hdlc
    18: fibre_channel

  protocol_types:
    0x0800: ipv4
    0x0806: arp
    0x86dd: ipv6

  arp_operation:
    1: request
    2: reply