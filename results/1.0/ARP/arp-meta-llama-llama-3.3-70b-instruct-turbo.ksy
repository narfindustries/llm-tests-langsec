meta:
  id: arp
  encoding: ASCII
  endian: le
seq:
  - id: hw_type
    type: u2
  - id: protocol_type
    type: u2
  - id: hw_size
    type: u1
  - id: protocol_size
    type: u1
  - id: opcode
    type: u2
  - id: sender_hw_addr
    type: bytes
    size: hw_size
  - id: sender_protocol_addr
    type: bytes
    size: protocol_size
  - id: target_hw_addr
    type: bytes
    size: hw_size
  - id: target_protocol_addr
    type: bytes
    size: protocol_size