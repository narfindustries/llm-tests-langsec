meta:
  id: arp
  title: Address Resolution Protocol
  endian: be

seq:
  - id: htype
    type: u2
    enum: hardware_type
  - id: ptype
    type: u2
    enum: protocol_type
  - id: hlen
    type: u1
  - id: plen
    type: u1
  - id: oper
    type: u2
    enum: operation_type
  - id: sender_hardware_addr
    type: hardware_addr
    size: hlen
  - id: sender_protocol_addr
    type: protocol_addr
    size: plen
  - id: target_hardware_addr
    type: hardware_addr
    size: hlen
  - id: target_protocol_addr
    type: protocol_addr
    size: plen

types:
  hardware_addr:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _parent.hlen

  protocol_addr:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _parent.plen

enums:
  hardware_type:
    1: ethernet
    6: ieee_802
    15: frame_relay
    16: async_transfer_mode
    17: hdlc
    18: fibre_channel
    19: async_transfer_mode_2

  protocol_type:
    0x0800: ipv4
    0x0806: arp
    0x86dd: ipv6

  operation_type:
    1: request
    2: reply