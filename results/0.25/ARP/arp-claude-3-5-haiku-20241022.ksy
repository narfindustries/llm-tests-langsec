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
    type: hardware_address
    size: hlen
  - id: sender_protocol_addr
    type: protocol_address
    size: plen
  - id: target_hardware_addr
    type: hardware_address
    size: hlen
  - id: target_protocol_addr
    type: protocol_address
    size: plen

enums:
  hardware_type:
    1: ethernet
    6: ieee_802
    15: frame_relay
  protocol_type:
    0x0800: ipv4
    0x86dd: ipv6
    0x0806: arp
  operation_type:
    1: request
    2: reply

types:
  hardware_address:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _parent.hlen
  protocol_address:
    seq:
      - id: addr
        type: u1
        repeat: expr
        repeat-expr: _parent.plen