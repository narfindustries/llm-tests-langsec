meta:
  id: arp
  title: Address Resolution Protocol
  endian: be

seq:
  - id: hardware_type
    type: u2
    enum: hardware_type_enum

  - id: protocol_type
    type: u2
    enum: protocol_type_enum

  - id: hardware_addr_length
    type: u1

  - id: protocol_addr_length
    type: u1

  - id: operation
    type: u2
    enum: operation_enum

  - id: sender_hardware_addr
    type: hardware_addr
    size: hardware_addr_length

  - id: sender_protocol_addr
    type: protocol_addr
    size: protocol_addr_length

  - id: target_hardware_addr
    type: hardware_addr
    size: hardware_addr_length

  - id: target_protocol_addr
    type: protocol_addr
    size: protocol_addr_length

types:
  hardware_addr:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _parent.hardware_addr_length

  protocol_addr:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _parent.protocol_addr_length

enums:
  hardware_type_enum:
    1: ethernet
    6: ieee_802
    15: frame_relay

  protocol_type_enum:
    0x0800: ipv4
    0x86dd: ipv6
    0x0806: arp

  operation_enum:
    1: request
    2: reply
    3: reverse_request
    4: reverse_reply
    5: drarp_request
    6: drarp_reply
    7: drarp_error
    8: inarp_request
    9: inarp_reply
    10: arp_nam