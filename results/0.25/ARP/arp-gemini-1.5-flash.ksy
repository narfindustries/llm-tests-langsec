type: struct
endian: big
fields:
  - id: hardware_type
    type: u2
  - id: protocol_type
    type: u2
  - id: hardware_addr_len
    type: u1
  - id: protocol_addr_len
    type: u1
  - id: opcode
    type: u2
  - id: sender_hardware_addr
    type: bytes
    size: hardware_addr_len
  - id: sender_protocol_addr
    type: bytes
    size: protocol_addr_len
  - id: target_hardware_addr
    type: bytes
    size: hardware_addr_len
  - id: target_protocol_addr
    type: bytes
    size: protocol_addr_len
