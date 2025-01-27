meta:
  id: arp
  endian: be
seq:
  - id: hardware_type
    type: u2
    enum: hardware_types
  - id: protocol_type
    type: u2
    enum: protocol_types
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
  protocol_types:
    0x0800: ipv4
  opcodes:
    1: request
    2: reply