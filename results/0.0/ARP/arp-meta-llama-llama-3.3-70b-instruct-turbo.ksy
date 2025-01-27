meta:
  id: arp
  title: Address Resolution Protocol
  endian: le

seq:
  - id: hardware_type
    type: u2
  - id: protocol_type
    type: u2
  - id: hardware_size
    type: u1
  - id: protocol_size
    type: u1
  - id: opcode
    type: u2
  - id: sender_hardware_addr
    type: bytes
    size: hardware_size
  - id: sender_protocol_addr
    type: bytes
    size: protocol_size
  - id: target_hardware_addr
    type: bytes
    size: hardware_size
  - id: target_protocol_addr
    type: bytes
    size: protocol_size